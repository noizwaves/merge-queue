module MergeQueue.Domain

// Models
type SHA = SHA of string

type PullRequestID = PullRequestID of int

type PullRequest =
    { id: PullRequestID
      sha: SHA }

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

let pullRequest (id: PullRequestID) (branchHead: SHA): PullRequest =
    { id = id
      sha = branchHead }

let pullRequestId (value: int): PullRequestID =
    PullRequestID value

let sha (value: string): SHA =
    SHA value

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
    | Success of List<PullRequest>
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
    | NoBatch, Success _ ->
        NoOp, MergeQueueState model
    | Running runningBatch, Success builtBatch ->
        let validSuccess = (runningBatch = builtBatch)
        match validSuccess with
        | true ->
            let result = PerformBatchMerge runningBatch
            let state = MergeQueueState { model with runningBatch = Merging runningBatch }
            (result, state)
        | false ->
            (NoOp, MergeQueueState model)
    | Merging _, Success _ ->
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
    | AbortMergingBatch of Batch * PullRequestID

let private updateShaForEnqueuedPr (newValue: SHA) (id: PullRequestID) (model: MergeQueueModel): List<PullRequest> =
    let updateCorrespondingSha pr =
        if pr.id = id then { pr with sha = newValue }
        else pr

    model.queue |> List.map updateCorrespondingSha

let updatePullRequestSha (id: PullRequestID) (newValue: SHA) (MergeQueueState model): UpdatePullRequestResult * MergeQueueState =
    match model.runningBatch with
    | Running batch ->
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
            let newQueue = model |> updateShaForEnqueuedPr newValue id
            let newModel = { model with queue = newQueue }
            NoOp, MergeQueueState newModel
    | NoBatch ->
        let newQueue = model |> updateShaForEnqueuedPr newValue id
        let newModel = { model with queue = newQueue }
        NoOp, MergeQueueState newModel
    | Merging batch ->
        let merging =
            batch
            |> List.map (fun pr -> pr.id)
            |> List.contains id

        if merging then
            // fast fail the current batch, an unsafe PR could be about to merge into target
            let newQueue = model.queue |> List.filter (fun n -> List.contains n batch |> not)
            AbortMergingBatch(batch, id),
            MergeQueueState
                { model with
                      queue = newQueue
                      runningBatch = NoBatch }
        else
            let newQueue = model |> updateShaForEnqueuedPr newValue id
            let newModel = { model with queue = newQueue }
            NoOp, MergeQueueState newModel

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
