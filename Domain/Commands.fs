module MergeQueue.Commands

open MergeQueue.DomainTypes
open MergeQueue.Domain
open MergeQueue.DbTypes

// SMELL: domain object should be build within command and not an argument, accept arguments
type EnqueueCommand =
    { number: int
      sha: string
      statuses: List<string * string> }

type EnqueueResult =
    | Enqueued
    | SinBinned
    | RejectedFailingBuildStatus
    | AlreadyEnqueued

let enqueue (load: Load) (save: Save) (command: EnqueueCommand): EnqueueResult =

    // TODO: perform validation here
    let statuses =
        command.statuses
        |> List.map (fun (context, state) -> CommitStatus.create context (CommitStatusState.create state))
    let pullRequest =
        PullRequest.pullRequest (PullRequestID.create command.number) (SHA.create command.sha) statuses

    // Eventually load a DTO and parse to domain object
    let model = load()

    // TODO: wrap most of the code below in a new domain method called `enqueue`
    let isBuildFailing = (getBuildStatus pullRequest) = BuildFailure
    // TODO: Concept here, "locate pull request", multiple occurences
    // TODO: Currently not checking to see if the pull request is currently running!
    let alreadyEnqueued = model.queue |> AttemptQueue.contains pullRequest.id
    let alreadySinBinned = model.sinBin |> SinBin.contains pullRequest.id
    let prepared = prepareForQueue pullRequest

    match isBuildFailing, alreadyEnqueued, alreadySinBinned, prepared with
    | true, _, _, _ ->
        RejectedFailingBuildStatus
    | _, true, _, _ ->
        AlreadyEnqueued
    | _, _, true, _ ->
        AlreadyEnqueued
    | false, false, false, Choice2Of2 naughty ->
        let newModel = { model with sinBin = SinBin.append naughty model.sinBin }
        save newModel
        SinBinned
    | false, false, false, Choice1Of2 passing ->
        let newModel = { model with queue = AttemptQueue.append passing model.queue }
        save newModel
        Enqueued

type DequeueResult =
    | Dequeued
    | DequeuedAndAbortRunningBatch of List<PullRequest> * PullRequestID
    | RejectedInMergingBatch
    | NotFound

let dequeue (load: Load) (save: Save) (id: PullRequestID): DequeueResult =
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

type StartBatchResult =
    | PerformBatchBuild of List<PullRequest>
    | AlreadyRunning
    | EmptyQueue

// SMELL: what calls this? synchronous after some other call?
// maybe make start batch private, and call it inside enqueue && updateStatus?
let startBatch (load: Load) (save: Save) (): StartBatchResult =
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

type BuildMessage =
    | Success of SHA
    | Failure

type IngestBuildResult =
    | NoOp
    | PerformBatchMerge of List<PullRequest> * SHA
    | ReportBuildFailureWithRetry of List<PullRequest>
    | ReportBuildFailureNoRetry of List<PullRequest>

let ingestBuildUpdate (load: Load) (save: Save) (message: BuildMessage): IngestBuildResult =
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

type MergeMessage =
    | Success
    | Failure

type IngestMergeResult =
    | NoOp
    | MergeComplete of List<PullRequest>
    | ReportMergeFailure of List<PullRequest>

let ingestMergeUpdate (load: Load) (save: Save) (message: MergeMessage): IngestMergeResult =
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

type UpdatePullRequestResult =
    | NoOp
    | AbortRunningBatch of List<PullRequest> * PullRequestID
    | AbortMergingBatch of List<PullRequest> * PullRequestID

let updatePullRequestSha (load: Load) (save: Save) (id: PullRequestID) (newValue: SHA): UpdatePullRequestResult =
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

// TODO: Make an UpdateStatusesResult type

let updateStatuses (load: Load) (save: Save) (id: PullRequestID) (buildSha: SHA) (statuses: CommitStatuses): unit =
    let model = load()

    // check to see if we should pull the matching commit out of the "sin bin"
    let newQueue, newSinBin =
        updateStatusesInSinBin id buildSha statuses model.queue model.sinBin

    let newModel =
        { model with
              queue = newQueue
              sinBin = newSinBin }
    save newModel

    ()
