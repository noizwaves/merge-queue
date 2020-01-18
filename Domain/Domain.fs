module MergeQueue.Domain

// Models
type PullRequestID = PullRequestID of int

type PullRequest =
    { id: PullRequestID }

type Batch = List<PullRequest>

type private CurrentBatch =
    | NoBatch
    | Running of Batch
    | Merging of Batch

type private MergeQueueModel =
    { queue: List<PullRequest>
      runningBatch: CurrentBatch }

type MergeQueueState = private MergeQueueState of MergeQueueModel


// Constructors
let emptyMergeQueue: MergeQueueState =
    MergeQueueState
        { queue = []
          runningBatch = NoBatch }

let pullRequest (id: PullRequestID): PullRequest =
    { id = id }

let pullRequestId (value: int): PullRequestID =
    PullRequestID value


// Commands
type EnqueueResult =
    | Success
    | AlreadyEnqueued

let enqueue (pullRequest: PullRequest) (MergeQueueState model): EnqueueResult * MergeQueueState =
    let alreadyEnqueued =
        model.queue |> List.contains pullRequest

    match alreadyEnqueued with
    | true -> AlreadyEnqueued, MergeQueueState model
    | false -> Success, MergeQueueState { model with queue = model.queue @ [ pullRequest ] }

type StartBatchResult =
    | Success of List<PullRequest>
    | AlreadyRunning
    | EmptyQueue

let startBatch (MergeQueueState model): StartBatchResult * MergeQueueState =
    match model.runningBatch, model.queue with
    | _, [] -> EmptyQueue, MergeQueueState model
    | Running _, _ -> AlreadyRunning, MergeQueueState model
    | Merging _, _ -> AlreadyRunning, MergeQueueState model
    | NoBatch, queue -> Success queue, MergeQueueState { model with runningBatch = Running queue }

type BuildMessage =
    | Success
    | Failure

type IngestBuildResult =
    | NoOp
    | PerformBatchMerge of List<PullRequest>
    | BuildFailure

let ingestBuildUpdate (message: BuildMessage) (MergeQueueState model): IngestBuildResult * MergeQueueState =
    match model.runningBatch, message with
    | NoBatch, Failure ->
        NoOp, MergeQueueState { model with runningBatch = NoBatch } // The record update doesn't change the value...
    | Running _, Failure ->
        BuildFailure, MergeQueueState { model with runningBatch = NoBatch }
    | Merging _, Failure ->
        NoOp, MergeQueueState model
    | NoBatch, Success ->
        NoOp, MergeQueueState model
    | Running batch, Success ->
        let result = PerformBatchMerge batch
        let state = MergeQueueState { model with runningBatch = Merging batch }
        (result, state)
    | Merging _, Success ->
        NoOp, MergeQueueState model

type MergeMessage =
    | Success
    | Failure

type IngestMergeResult =
    | NoOp
    | MergeComplete of List<PullRequest>
    | ReportMergeFailure of List<PullRequest>

let ingestMergeUpdate (message: MergeMessage) (MergeQueueState model): IngestMergeResult * MergeQueueState =
    match model.runningBatch, message with
    | Merging batch, MergeMessage.Success ->
        let newQueue = model.queue |> List.filter (fun n -> List.contains n batch |> not)

        let state =
            MergeQueueState
                { model with
                      queue = newQueue
                      runningBatch = NoBatch }
        IngestMergeResult.MergeComplete batch, state
    | Merging batch, MergeMessage.Failure ->
        let state = MergeQueueState { model with runningBatch = NoBatch }
        IngestMergeResult.ReportMergeFailure batch, state
    | _, _ ->
        IngestMergeResult.NoOp, MergeQueueState model

type UpdatePullRequestResult =
    | NoOp
    | AbortRunningBatch of Batch * PullRequestID

let updatePullRequestSha (id: PullRequestID) (MergeQueueState model): UpdatePullRequestResult * MergeQueueState =
    match model.runningBatch, model.queue with
    | Running batch, _ ->
        let running =
            batch
            |> List.map (fun pr -> pr.id)
            |> List.contains id

        if running then
            // dequeue changed PR
            let newQueue =
                model.queue |> List.filter (fun pr -> pr.id <> id)

            AbortRunningBatch(batch, id),
            MergeQueueState
                { model with
                      queue = newQueue
                      runningBatch = NoBatch }
        else
            NoOp, MergeQueueState model
    | _ ->
        NoOp, MergeQueueState model


// Queries
let getDepth (MergeQueueState model): int =
    model.queue |> List.length

type Status =
    | Idle
    | Running

let getStatus (MergeQueueState model): Status =
    match model.runningBatch with
    | NoBatch -> Idle
    | CurrentBatch.Running _ -> Running
    | CurrentBatch.Merging _ -> Running
