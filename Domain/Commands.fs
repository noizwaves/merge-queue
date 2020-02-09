module MergeQueue.Commands

open MergeQueue.DomainTypes
open MergeQueue.Domain
open MergeQueue.DbTypes

let rec private consolidateResultList<'a, 'b> (results: List<Result<'a, 'b>>): Result<List<'a>, 'b> =
    match results with
    | [] ->
        Ok []
    | Error b :: tail ->
        Error b
    | Ok a :: tail ->
        tail
        |> consolidateResultList
        |> Result.map (fun aa -> a :: aa)

// TODO: tidy this up with computation expressions and composition
// TODO: move to somewhere not the Command module?
let private toPullRequestDomain (number: int) (sha: string) (statuses: List<string * string>): Result<PullRequest, string> =
    let number' = PullRequestID.create number
    let sha' = SHA.create sha

    let statuses' =
        statuses
        |> List.map CommitStatus.create
        |> consolidateResultList

    match number', sha', statuses' with
    | Error err, _, _ -> Error err
    | _, Error err, _ -> Error err
    | _, _, Error err -> Error err
    | Ok number, Ok sha, Ok statuses -> Ok(PullRequest.create number sha statuses)

module Common =
    let toTwoTrackInput func =
        fun result ->
            match result with
            | Ok value -> func value
            | Error err -> Error err

module Enqueue =
    // SMELL: the word Enqueue appears a lot here

    // SMELL: domain object should be build within command and not an argument, accept arguments
    type EnqueueCommand =
        { number: int
          sha: string
          statuses: List<string * string> }

    type private EnqueueStepSuccess =
        | Enqueued of MergeQueue
        | SinBinned of MergeQueue

    type EnqueueStepError =
        | RejectedFailingBuildStatus
        | AlreadyEnqueued

    type EnqueueError =
        | ValidationError of string
        | EnqueueStepError of EnqueueStepError

    type EnqueueSuccess =
        | Enqueued
        | SinBinned

    type EnqueueResult = Result<EnqueueSuccess, EnqueueError>

    type private EnqueueStatusResult = Result<EnqueueStepSuccess, EnqueueStepError>

    type private ValidatePullRequest = EnqueueCommand -> Result<PullRequest, string>

    type private LoadMergeQueue = PullRequest -> (PullRequest * MergeQueue)

    type private EnqueueStep = PullRequest * MergeQueue -> EnqueueStatusResult

    type private SaveMergeQueue = EnqueueStepSuccess -> EnqueueStepSuccess

    type private FormatEnqueueResult = Result<EnqueueStepSuccess, EnqueueError> -> EnqueueResult

    let private validatePullRequest: ValidatePullRequest =
        fun command -> toPullRequestDomain command.number command.sha command.statuses

    let private loadMergeQueue (load: Load): LoadMergeQueue =
        fun pr ->
            let model = load()
            (pr, model)

    let private enqueueStep: EnqueueStep =
        fun (pullRequest, model) ->
            let isBuildFailing = (getBuildStatus pullRequest) = BuildFailure
            // TODO: Concept here, "locate pull request", multiple occurences
            // TODO: Currently not checking to see if the pull request is currently running!
            let alreadyEnqueued = model.queue |> AttemptQueue.contains pullRequest.id
            let alreadySinBinned = model.sinBin |> SinBin.contains pullRequest.id
            let prepared = prepareForQueue pullRequest

            match isBuildFailing, alreadyEnqueued, alreadySinBinned, prepared with
            | true, _, _, _ ->
                Error RejectedFailingBuildStatus
            | _, true, _, _ ->
                Error AlreadyEnqueued
            | _, _, true, _ ->
                Error AlreadyEnqueued
            | false, false, false, Choice2Of2 naughty ->
                let newModel = { model with sinBin = SinBin.append naughty model.sinBin }
                Ok(EnqueueStepSuccess.SinBinned(newModel))
            | false, false, false, Choice1Of2 passing ->
                let newModel = { model with queue = AttemptQueue.append passing model.queue }
                Ok(EnqueueStepSuccess.Enqueued(newModel))

    let private saveMergeQueue (save: Save): SaveMergeQueue =
        fun result ->
            match result with
            | EnqueueStepSuccess.Enqueued model ->
                save model |> ignore
                result
            | EnqueueStepSuccess.SinBinned model ->
                save model |> ignore
                result

    let private toEnqueueResult: FormatEnqueueResult =
        fun result ->
            match result with
            | Ok(EnqueueStepSuccess.SinBinned _) ->
                Ok(EnqueueSuccess.SinBinned)
            | Ok(EnqueueStepSuccess.Enqueued _) ->
                Ok(EnqueueSuccess.Enqueued)
            | Error(EnqueueError.EnqueueStepError(EnqueueStepError.RejectedFailingBuildStatus)) ->
                Error(EnqueueError.EnqueueStepError EnqueueStepError.RejectedFailingBuildStatus)
            | Error(EnqueueError.EnqueueStepError(EnqueueStepError.AlreadyEnqueued)) ->
                Error(EnqueueError.EnqueueStepError EnqueueStepError.AlreadyEnqueued)
            | Error(EnqueueError.ValidationError msg) -> failwithf "Validation error: %s" msg

    let enqueue (load: Load) (save: Save) (command: EnqueueCommand): EnqueueResult =
        let loadMergeQueue = loadMergeQueue load
        let saveMergeQueue = saveMergeQueue save

        let validatePullRequest = validatePullRequest >> (Result.mapError EnqueueError.ValidationError)
        let enqueueStep = enqueueStep >> (Result.mapError EnqueueError.EnqueueStepError)

        command
        |> validatePullRequest
        |> Result.map loadMergeQueue
        |> Common.toTwoTrackInput enqueueStep
        |> Result.map saveMergeQueue
        |> toEnqueueResult

module Dequeue =
    type DequeueCommand =
        { number: int }

    type DequeueResult =
        | Dequeued
        | DequeuedAndAbortRunningBatch of List<PullRequest> * PullRequestID
        | RejectedInMergingBatch
        | NotFound

    let dequeue (load: Load) (save: Save) (command: DequeueCommand): DequeueResult =
        // TODO: chain validation with further processing and return errors
        let id =
            match PullRequestID.create command.number with
            | Ok id -> id
            | Error error -> failwithf "Validation failed: %s" error

        let model = load()
        // TODO: Concept here, "locate pull request", multiple occurrences
        let isCurrent = model.activeBatch |> ActiveBatch.contains id
        let isEnqueued = model.queue |> AttemptQueue.contains id
        let isSinBinned = model.sinBin |> SinBin.contains id

        let result, newModel =
            match isCurrent, isEnqueued, isSinBinned with
            | true, _, _ ->
                match model.activeBatch with
                | Running(RunnableBatch batch) ->
                    let newQueue =
                        model.queue
                        |> AttemptQueue.prepend batch
                        |> AttemptQueue.removeById id

                    let newBatch = NoBatch

                    let pullRequests = batch |> Batch.toPullRequests
                    let result = DequeuedAndAbortRunningBatch(pullRequests, id)

                    let newModel =
                        { model with
                              queue = newQueue
                              activeBatch = newBatch }
                    result, newModel

                | Merging _ ->
                    RejectedInMergingBatch, model

                | NoBatch ->
                    // SMELL: this is an impossible branch to get into...
                    failwith "PullRequest cannot be in an empty batch"

            | _, true, _ ->
                let newQueue = model.queue |> AttemptQueue.removeById id
                Dequeued, { model with queue = newQueue }

            | _, _, true ->
                let newSinBin = model.sinBin |> SinBin.removeById id
                Dequeued, { model with sinBin = newSinBin }

            | false, false, false ->
                NotFound, model

        save newModel |> ignore // TODO: sometimes we don't update the model... so why save it?
        result

module StartBatch =
    type StartBatchCommand = unit

    type StartBatchResult =
        | PerformBatchBuild of List<PullRequest>
        | AlreadyRunning
        | EmptyQueue

    // SMELL: what calls this? synchronous after some other call?
    // maybe make start batch private, and call it inside enqueue && updateStatus?
    let startBatch (load: Load) (save: Save) (command: StartBatchCommand): StartBatchResult =
        let model = load()
        match model.activeBatch, model.queue with
        | Running _, _ -> AlreadyRunning
        | Merging _, _ -> AlreadyRunning
        | NoBatch, AttemptQueue [] -> EmptyQueue
        | NoBatch, queue ->
            match pickNextBatch queue with
            | Some(batch, remaining) ->
                let newModel =
                    { model with
                          activeBatch = Running batch
                          queue = remaining }
                save newModel

                let pullRequests = batch |> RunnableBatch.toPullRequests
                PerformBatchBuild pullRequests
            | None ->
                // SMELL: impossible code path, all non-empty queues have a next batch...
                // SMELL: how could execution get here and result is empty?
                EmptyQueue

module IngestBuild =
    // TODO/SMELL: move domain type out of Command type
    type BuildMessage =
        | Success of SHA // TODO: make this a string
        | Failure

    type IngestBuildCommand =
        { message: BuildMessage }

    type IngestBuildResult =
        | NoOp
        | PerformBatchMerge of List<PullRequest> * SHA
        | ReportBuildFailureWithRetry of List<PullRequest>
        | ReportBuildFailureNoRetry of List<PullRequest>

    let ingestBuildUpdate (load: Load) (save: Save) (command: IngestBuildCommand): IngestBuildResult =
        let message = command.message

        let model = load()
        match model.activeBatch, message with
        | Running failed, Failure ->
            match bisect failed with
            | None ->
                let queue, nextActive = failWithoutRetry model.queue failed

                let newModel =
                    { model with
                          queue = queue
                          activeBatch = nextActive }
                save newModel

                let prs = failed |> RunnableBatch.toPullRequests
                ReportBuildFailureNoRetry prs

            | Some(first, second) ->
                let queue, nextActive = failWithRetry model.queue first second

                let newModel =
                    { model with
                          queue = queue
                          activeBatch = nextActive }
                save newModel

                let prs = failed |> RunnableBatch.toPullRequests
                ReportBuildFailureWithRetry prs

        | Running succeeded, Success targetHead ->
            let nextActive = completeBuild succeeded
            let newModel = { model with activeBatch = nextActive }
            let pullRequests = succeeded |> RunnableBatch.toPullRequests
            let result = PerformBatchMerge(pullRequests, targetHead)

            save newModel
            result

        | NoBatch, Failure ->
            NoOp
        | Merging _, Failure ->
            NoOp
        | NoBatch, Success _ ->
            NoOp
        | Merging _, Success _ ->
            NoOp

module IngestMerge =
    type MergeMessage =
        | Success
        | Failure

    type IngestMergeCommand =
        { message: MergeMessage }

    type IngestMergeResult =
        | NoOp
        | MergeComplete of List<PullRequest>
        | ReportMergeFailure of List<PullRequest>

    let ingestMergeUpdate (load: Load) (save: Save) (command: IngestMergeCommand): IngestMergeResult =
        let message = command.message

        let model = load()
        match model.activeBatch, message with
        | Merging merged, MergeMessage.Success ->
            let queue, batch = merged |> completeMerge model.queue

            let newModel =
                { model with
                      queue = queue
                      activeBatch = batch }
            save newModel

            let pullRequests = merged |> MergeableBatch.toPullRequests

            MergeComplete pullRequests
        | Merging unmerged, MergeMessage.Failure ->
            let queue, batch = unmerged |> failMerge model.queue
            let pullRequests = unmerged |> MergeableBatch.toPullRequests

            let newModel =
                { model with
                      queue = queue
                      activeBatch = batch }
            save newModel

            ReportMergeFailure pullRequests
        | _, _ ->
            IngestMergeResult.NoOp

module UpdatePullRequest =
    type UpdatePullRequestCommand =
        { number: int
          sha: string }

    type UpdatePullRequestResult =
        | NoOp
        | AbortRunningBatch of List<PullRequest> * PullRequestID
        | AbortMergingBatch of List<PullRequest> * PullRequestID

    let updatePullRequestSha (load: Load) (save: Save) (command: UpdatePullRequestCommand): UpdatePullRequestResult =
        // TODO: chain validation with further processing and return errors
        let id' = PullRequestID.create command.number
        let newValue' = SHA.create command.sha

        let id, newValue =
            match id', newValue' with
            | Ok id, Ok newValue -> id, newValue
            | Error error, _ -> failwithf "Validation failed: %s" error
            | _, Error error -> failwithf "Validation failed: %s" error

        let model = load()

        let newSinBin = model.sinBin |> updateShaInSinBin id newValue
        let modelWithNewSinBin = { model with sinBin = newSinBin }

        match modelWithNewSinBin.activeBatch with
        | Running batch ->
            let abortRunningBatch, newQueue, newSinBin =
                updateShaInRunningBatch id newValue batch modelWithNewSinBin.queue modelWithNewSinBin.sinBin

            if abortRunningBatch then
                let newModel =
                    { modelWithNewSinBin with
                          queue = newQueue
                          activeBatch = NoBatch
                          sinBin = newSinBin }
                save newModel

                let pullRequests = batch |> RunnableBatch.toPullRequests
                AbortRunningBatch(pullRequests, id)

            else
                let newModel =
                    { modelWithNewSinBin with
                          queue = newQueue
                          sinBin = newSinBin }
                save newModel
                NoOp
        | NoBatch ->
            let newQueue, newSinBin =
                updateShaInQueue id newValue modelWithNewSinBin.queue modelWithNewSinBin.sinBin

            let newModel =
                { modelWithNewSinBin with
                      queue = newQueue
                      sinBin = newSinBin }
            save newModel
            NoOp

        | Merging batch ->
            let inMergingBatch = batch |> MergeableBatch.contains id

            if inMergingBatch then
                let newModel =
                    { modelWithNewSinBin with activeBatch = NoBatch }
                save newModel

                let pullRequests = batch |> MergeableBatch.toPullRequests
                AbortMergingBatch(pullRequests, id)
            else
                let newQueue, newSinBin =
                    updateShaInQueue id newValue modelWithNewSinBin.queue modelWithNewSinBin.sinBin

                let newModel =
                    { modelWithNewSinBin with
                          queue = newQueue
                          sinBin = newSinBin }
                save newModel
                NoOp

module UpdateStatuses =
    type UpdateStatusesResult = NoOp

    type UpdateStatusesCommand =
        { number: int
          sha: string
          statuses: List<string * string> }

    let updateStatuses (load: Load) (save: Save) (command: UpdateStatusesCommand): UpdateStatusesResult =
        // TODO: chain validation with further processing and return errors
        let id' = PullRequestID.create command.number
        let buildSha' = SHA.create command.sha

        let statuses' =
            command.statuses
            |> List.map CommitStatus.create
            |> consolidateResultList

        let id, buildSha, statuses =
            match id', buildSha', statuses' with
            | Ok id, Ok buildSha, Ok statuses -> id, buildSha, statuses
            | Error error, _, _ -> failwithf "Validation failed: %s" error
            | _, Error error, _ -> failwithf "Validation failed: %s" error
            | _, _, Error error -> failwithf "Validation failed: %s" error

        let model = load()

        // check to see if we should pull the matching commit out of the "sin bin"
        let newQueue, newSinBin =
            updateStatusesInSinBin id buildSha statuses model.queue model.sinBin

        let newModel =
            { model with
                  queue = newQueue
                  sinBin = newSinBin }
        save newModel

        NoOp
