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
// TODO: could this be inlined to Enqueue module?
let private toPullRequestDomain (number: int) (sha: string) (statuses: List<string * string>): Result<PullRequest, string> =
    let number' = PullRequestNumber.create number
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
    // Input is a func: a' -> Result<b', e'>
    // Produces a func: Result<a', e'> -> Result<b', e'>
    let bind func =
        fun result ->
            match result with
            | Ok value -> func value
            | Error err -> Error err

    let tee func =
        fun value ->
            func value |> ignore
            value

    let switch func =
        fun value -> func value |> Ok

    // Input is a func: unit -> a'
    // Produces a func: Result<b', e'> -> Result<a' * b', e'>
    let carrySecond func =
        fun result ->
            match result with
            | Ok value -> Ok(func(), value)
            | Error error -> Error error

    // Input is a func: a' -> b'
    // Produces: Result<c' * a', e'> -> Result<b', e'>
    let mapSecond func =
        Result.map (snd >> func)

    // Input is a func: a' -> b'
    // Produces: Result<a' * c', e'> -> Result<b', e'>
    let mapFirst func =
        Result.map (fst >> func)

module Enqueue =
    // Types
    type Command =
        { number: int
          sha: string
          statuses: List<string * string> }

    // TODO: convert to some kind of command DTO instead of using the domain type?
    type Success = EnqueueSuccess

    type Error =
        | ValidationError of string
        | EnqueueError of EnqueueError

    type EnqueueResult = Result<Success, Error>

    type EnqueueWorkflow = Command -> EnqueueResult

    // Implementation

    /// Validation
    type private ValidatePullRequest = Command -> Result<PullRequest, string>

    let private validatePullRequest: ValidatePullRequest =
        fun command -> toPullRequestDomain command.number command.sha command.statuses

    /// Loading the MergeQueue
    type private StepInput =
        { pullRequest: PullRequest
          mergeQueue: MergeQueue }

    type private LoadMergeQueue = PullRequest -> StepInput

    let private loadMergeQueue (load: Load): LoadMergeQueue =
        fun pr ->
            let model = load()
            { pullRequest = pr
              mergeQueue = model }

    /// Enqueue step
    type private EnqueueStep = StepInput -> Result<EnqueueSuccess * MergeQueue, EnqueueError>

    let private enqueueStep: EnqueueStep =
        fun input -> input.mergeQueue |> Domain.enqueue input.pullRequest

    /// Save changes to the MergeQueue
    type private SaveMergeQueue = MergeQueue -> unit

    let private saveMergeQueue (save: Save): SaveMergeQueue =
        fun model ->
            save model

    // the final workflow
    let enqueue (load: Load) (save: Save): EnqueueWorkflow =
        let loadMergeQueue = loadMergeQueue load
        let saveMergeQueue = saveMergeQueue save

        let validatePullRequest = validatePullRequest >> (Result.mapError Error.ValidationError)
        let enqueueStep = enqueueStep >> (Result.mapError Error.EnqueueError)

        fun command ->
            command
            |> validatePullRequest
            |> Result.map loadMergeQueue
            |> Common.bind enqueueStep
            |> Common.tee (Common.mapSecond saveMergeQueue)
            |> Common.mapFirst id

module Dequeue =
    // Types
    type Command =
        { number: int }

    type Success = DequeueSuccess

    type Error =
        | ValidationError of string
        | DequeueError of DequeueError

    type DequeueResult = Result<Success, Error>

    type DequeueWorkflow = Command -> DequeueResult

    // Implementation

    /// Validation
    type private ValidatedDequeue =
        { number: PullRequestNumber }

    type private ValidateDequeue = Command -> Result<ValidatedDequeue, string>

    let private validateDequeue: ValidateDequeue =
        fun command ->
            command.number
            |> PullRequestNumber.create
            |> Result.map (fun number -> { number = number })

    /// Loading
    type private StepInput =
        { number: PullRequestNumber
          mergeQueue: MergeQueue }

    type private LoadMergeQueue = ValidatedDequeue -> StepInput

    let private loadMergeQueue (load: Load): LoadMergeQueue =
        fun value ->
            let mergeQueue = load()
            { number = value.number
              mergeQueue = mergeQueue }

    /// Dequeue step
    type private DequeueStep = StepInput -> Result<DequeueSuccess * MergeQueue, DequeueError>

    let private dequeueStep: DequeueStep =
        fun input -> input.mergeQueue |> Domain.dequeue input.number

    /// Save
    type private SaveMergeQueue = MergeQueue -> unit

    let private saveMergeQueue (save: Save): SaveMergeQueue =
        fun mergeQueue ->
            save mergeQueue

    // the final workflow
    let dequeue (load: Load) (save: Save): DequeueWorkflow =
        let validateDequeue = validateDequeue >> Result.mapError ValidationError
        let loadMergeQueue = loadMergeQueue load
        let dequeueStep = dequeueStep >> Result.mapError DequeueError
        let saveMergeQueue = saveMergeQueue save

        fun command ->
            command
            |> validateDequeue
            |> Result.map loadMergeQueue
            |> Common.bind dequeueStep
            |> Common.tee (Common.mapSecond saveMergeQueue)
            |> Common.mapFirst id

module StartBatch =
    // Types
    type Command = unit

    type Success = StartBatchSuccess

    type Error = StartBatchError of StartBatchError

    type StartBatchResult = Result<Success, Error>

    type StartBatchWorkflow = Command -> StartBatchResult

    // Implementation

    /// Load
    type private LoadMergeQueue = unit -> MergeQueue

    let private loadMergeQueue (load: Load): LoadMergeQueue =
        load

    /// Start the batch
    type private StepInput =
        { mergeQueue: MergeQueue }

    type private StartBatchStep = StepInput -> Result<StartBatchSuccess * MergeQueue, StartBatchError>

    let private toInput (mergeQueue: MergeQueue): StepInput =
        { mergeQueue = mergeQueue }

    let private startBatchStep: StartBatchStep =
        fun input -> input.mergeQueue |> startBatch ()

    /// Save
    type private SaveMergeQueue = MergeQueue -> unit

    let private saveMergeQueue (save: Save): SaveMergeQueue =
        fun mergeQueue ->
            save mergeQueue

    // SMELL: what calls this? synchronous after some other call?
    // maybe make start batch private, and call it inside enqueue && updateStatus?
    let startBatch (load: Load) (save: Save): StartBatchWorkflow =
        let loadMergeQueue = loadMergeQueue load
        let startBatchStep = startBatchStep >> Result.mapError StartBatchError
        let saveMergeQueue = saveMergeQueue save

        fun command ->
            command
            |> Common.switch (loadMergeQueue >> toInput)
            |> Common.bind startBatchStep
            |> Common.tee (Common.mapSecond saveMergeQueue)
            |> Common.mapFirst id

module IngestBuild =
    // Types
    // TODO: Make this more like a DTO (i.e. not a DU?)
    type UnvalidatedBuildMessage =
        | Success of string // the SHA of the failed builds commit
        | Failure // TODO: how do we know which SHA failed? do we care?

    type Command =
        { message: UnvalidatedBuildMessage }

    // SMELL: Success type name clashes with Success constructor for BuildMessage
    type IngestBuildSuccess =
        | NoChange
        | PerformBatchMerge of List<PullRequest> * SHA
        | ReportBuildFailureWithRetry of List<PullRequest>
        | ReportBuildFailureNoRetry of List<PullRequest>

    // TODO: Validation error goes here
    type Error = ValidationError of string

    type IngestBuildResult = Result<IngestBuildSuccess, Error>

    type IngestBuildWorkflow = Command -> IngestBuildResult

    // Implementation

    /// Validation
    type private ValidatedCommand =
        { message: BuildMessage }

    type private ValidateCommand = Command -> Result<ValidatedCommand, string>

    let private validateCommand: ValidateCommand =
        fun command ->
            // TODO: make this impl less gross
            match command.message with
            | UnvalidatedBuildMessage.Success(shaValue) ->
                shaValue
                |> SHA.create
                |> Result.map (fun sha -> { message = BuildMessage.Success sha })
            | UnvalidatedBuildMessage.Failure ->
                Ok { message = BuildMessage.Failure }

    /// Loading
    type private StepInput =
        { message: BuildMessage
          mergeQueue: MergeQueue }

    type private LoadMergeQueue = ValidatedCommand -> StepInput

    let private loadMergeQueue (load: Load): LoadMergeQueue =
        fun command ->
            let model = load()
            { message = command.message
              mergeQueue = model }

    /// Ingest
    type private IngestBuildStep = StepInput -> DomainTypes.IngestBuildSuccess

    let private ingestBuildStep: IngestBuildStep =
        fun input ->
            let message = input.message
            let model = input.mergeQueue

            ingestBuildUpdate message model

    /// Save
    type private SaveMergeQueue = DomainTypes.IngestBuildSuccess -> unit

    let private saveMergeQueue (save: Save): SaveMergeQueue =
        // SMELL: so much mapping
        fun success ->
            match success with
            | DomainTypes.IngestBuildSuccess.NoChange ->
                success |> ignore
            | DomainTypes.IngestBuildSuccess.PerformBatchMerge(mergeQueue, _, _) ->
                save mergeQueue
            | DomainTypes.IngestBuildSuccess.ReportBuildFailureWithRetry(mergeQueue, _) ->
                save mergeQueue
            | DomainTypes.IngestBuildSuccess.ReportBuildFailureNoRetry(mergeQueue, _) ->
                save mergeQueue

    /// To command result type
    let private toSuccess (success: DomainTypes.IngestBuildSuccess): IngestBuildSuccess =
        // SMELL: so much mapping
        match success with
        | DomainTypes.IngestBuildSuccess.NoChange ->
            IngestBuildSuccess.NoChange
        | DomainTypes.IngestBuildSuccess.PerformBatchMerge(_, prs, sha) ->
            IngestBuildSuccess.PerformBatchMerge(prs, sha)
        | DomainTypes.IngestBuildSuccess.ReportBuildFailureWithRetry(_, prs) ->
            IngestBuildSuccess.ReportBuildFailureWithRetry(prs)
        | DomainTypes.IngestBuildSuccess.ReportBuildFailureNoRetry(_, prs) ->
            IngestBuildSuccess.ReportBuildFailureNoRetry(prs)

    /// The final workflow
    let ingestBuildUpdate (load: Load) (save: Save): IngestBuildWorkflow =
        let validateCommand = validateCommand >> Result.mapError ValidationError
        let loadMergeQueue = loadMergeQueue load
        let saveMergeQueue = saveMergeQueue save

        fun command ->
            command
            |> validateCommand
            |> Result.map loadMergeQueue
            |> Result.map ingestBuildStep
            |> Common.tee (Result.map saveMergeQueue)
            |> Result.map toSuccess

module IngestMerge =
    // Types
    // TODO: This should be a DTO that represents the Batch merged result
    type UnvalidatedMergeMessage =
        | Success
        | Failure

    type Command =
        { message: UnvalidatedMergeMessage }

    type IngestMergeSuccess =
        | NoChange // TODO: This should be an error: no merge in progress
        | MergeComplete of List<PullRequest>
        | ReportMergeFailure of List<PullRequest>

    type IngestMergeResult = IngestMergeSuccess

    type IngestMergeWorkflow = Command -> IngestMergeResult

    // Implementation

    // Validation
    type private ValidatedCommand =
        { message: MergeMessage }

    type private ValidateCommand = Command -> ValidatedCommand

    let private validateCommand: ValidateCommand =
        fun command ->
            let message =
                match command.message with
                | Success -> MergeMessage.Success
                | Failure -> MergeMessage.Failure

            { message = message }

    // Load merge queue
    type private StepInput =
        { message: MergeMessage
          mergeQueue: MergeQueue }

    type private LoadMergeQueue = ValidatedCommand -> StepInput

    let private loadMergeQueue (load: Load): LoadMergeQueue =
        fun command ->
            let mergeQueue = load()
            { message = command.message
              mergeQueue = mergeQueue }

    // Ingest merge update
    type private IngestMergeStep = StepInput -> DomainTypes.IngestMergeSuccess

    let private ingestMergeStep: IngestMergeStep =
        fun input -> ingestMergeUpdate input.message input.mergeQueue

    // Save merge queue
    type private SaveMergeQueue = DomainTypes.IngestMergeSuccess -> unit

    let private saveMergeQueue (save: Save): SaveMergeQueue =
        fun success ->
            match success with
            | DomainTypes.IngestMergeSuccess.NoChange ->
                success |> ignore
            | DomainTypes.IngestMergeSuccess.MergeComplete(mergeQueue, _) ->
                save mergeQueue
            | DomainTypes.IngestMergeSuccess.ReportMergeFailure(mergeQueue, _) ->
                save mergeQueue

    // and format the output
    let private toSuccess (success: DomainTypes.IngestMergeSuccess): IngestMergeSuccess =
        match success with
        | DomainTypes.IngestMergeSuccess.NoChange ->
            IngestMergeSuccess.NoChange
        | DomainTypes.IngestMergeSuccess.MergeComplete(_, prs) ->
            IngestMergeSuccess.MergeComplete prs
        | DomainTypes.IngestMergeSuccess.ReportMergeFailure(_, prs) ->
            IngestMergeSuccess.ReportMergeFailure prs

    let ingestMergeUpdate (load: Load) (save: Save): IngestMergeWorkflow =
        fun command ->
            command
            |> validateCommand
            |> loadMergeQueue load
            |> ingestMergeStep
            |> Common.tee (saveMergeQueue save)
            |> toSuccess

module UpdatePullRequest =
    // Types
    type Command =
        { number: int
          sha: string }

    // TODO: Expand with the other successful outcomes, like moved to Sin Bin
    type Success =
        | NoChange
        | AbortRunningBatch of List<PullRequest> * PullRequestNumber
        | AbortMergingBatch of List<PullRequest> * PullRequestNumber

    type UpdatePullRequestResult = Result<Success, unit>

    type UpdatePullRequestWorkflow = Command -> UpdatePullRequestResult

    // Implementation
    let updatePullRequestSha (load: Load) (save: Save): UpdatePullRequestWorkflow =
        fun command ->
            // TODO: chain validation with further processing and return errors
            let number' = PullRequestNumber.create command.number
            let newSha' = SHA.create command.sha

            let number, newSha =
                match number', newSha' with
                | Ok number, Ok newValue -> number, newValue
                | Error error, _ -> failwithf "Validation failed: %s" error
                | _, Error error -> failwithf "Validation failed: %s" error

            let model = load()

            let newSinBin = model.sinBin |> updateShaInSinBin number newSha
            let modelWithNewSinBin = { model with sinBin = newSinBin }

            match modelWithNewSinBin.activeBatch with
            | Running batch ->
                let abortRunningBatch, newQueue, newSinBin =
                    updateShaInRunningBatch number newSha batch modelWithNewSinBin.queue modelWithNewSinBin.sinBin

                if abortRunningBatch then
                    let newModel =
                        { modelWithNewSinBin with
                              queue = newQueue
                              activeBatch = NoBatch
                              sinBin = newSinBin }
                    save newModel

                    let pullRequests = batch |> RunnableBatch.toPullRequests
                    Ok(AbortRunningBatch(pullRequests, number))

                else
                    let newModel =
                        { modelWithNewSinBin with
                              queue = newQueue
                              sinBin = newSinBin }
                    save newModel
                    Ok NoChange
            | NoBatch ->
                let newQueue, newSinBin =
                    updateShaInQueue number newSha modelWithNewSinBin.queue modelWithNewSinBin.sinBin

                let newModel =
                    { modelWithNewSinBin with
                          queue = newQueue
                          sinBin = newSinBin }
                save newModel
                Ok NoChange

            | Merging batch ->
                let inMergingBatch = batch |> MergeableBatch.contains number

                if inMergingBatch then
                    let newModel =
                        { modelWithNewSinBin with activeBatch = NoBatch }
                    save newModel

                    let pullRequests = batch |> MergeableBatch.toPullRequests
                    Ok(AbortMergingBatch(pullRequests, number))
                else
                    let newQueue, newSinBin =
                        updateShaInQueue number newSha modelWithNewSinBin.queue modelWithNewSinBin.sinBin

                    let newModel =
                        { modelWithNewSinBin with
                              queue = newQueue
                              sinBin = newSinBin }
                    save newModel
                    Ok NoChange

module UpdateStatuses =
    // Types
    type Command =
        { number: int
          sha: string
          statuses: List<string * string> }

    type Success = NoChange

    type Error = unit

    type UpdateStatusResult = Result<Success, Error>

    type UpdateStatusWorkflow = Command -> UpdateStatusResult

    // Implementation
    let updateStatuses (load: Load) (save: Save): UpdateStatusWorkflow =
        fun command ->
            // TODO: chain validation with further processing and return errors
            let number' = PullRequestNumber.create command.number
            let buildSha' = SHA.create command.sha

            let statuses' =
                command.statuses
                |> List.map CommitStatus.create
                |> consolidateResultList

            let number, buildSha, statuses =
                match number', buildSha', statuses' with
                | Ok number, Ok buildSha, Ok statuses -> number, buildSha, statuses
                | Error error, _, _ -> failwithf "Validation failed: %s" error
                | _, Error error, _ -> failwithf "Validation failed: %s" error
                | _, _, Error error -> failwithf "Validation failed: %s" error

            let model = load()

            // check to see if we should pull the matching commit out of the "sin bin"
            let newQueue, newSinBin =
                updateStatusesInSinBin number buildSha statuses model.queue model.sinBin

            let newModel =
                { model with
                      queue = newQueue
                      sinBin = newSinBin }
            save newModel

            Ok NoChange
