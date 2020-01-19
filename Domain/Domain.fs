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

type private BisectPath = List<bool>

type private AttemptQueue = List<PullRequest * BisectPath>

type private MergeQueueModel =
    { queue: AttemptQueue
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


// misc
let private removeFromQueue (toRemove: List<PullRequest>) (queue: AttemptQueue): AttemptQueue =
    queue |> List.filter (fun (pr, _) -> List.contains pr toRemove |> not)

// Commands
type EnqueueResult =
    | Success
    | AlreadyEnqueued

let enqueue (pullRequest: PullRequest) (MergeQueueState model): EnqueueResult * MergeQueueState =
    let alreadyEnqueued =
        model.queue
        |> List.map fst
        |> List.contains pullRequest

    match alreadyEnqueued with
    | true -> AlreadyEnqueued, MergeQueueState model
    | false -> Success, MergeQueueState { model with queue = model.queue @ [ pullRequest, [] ] }

type StartBatchResult =
    | PerformBatchBuild of List<PullRequest>
    | AlreadyRunning
    | EmptyQueue

let private selectBatch (queue: AttemptQueue): Batch =
    match queue with
    | [] -> failwith "Should not be called on an empty queue"
    | head :: tail ->
        let matching =
            tail |> List.filter (fun (_, a) -> a = (head |> snd))

        head :: matching |> List.map fst

let startBatch (MergeQueueState model): StartBatchResult * MergeQueueState =
    match model.runningBatch, model.queue with
    | _, [] -> EmptyQueue, MergeQueueState model
    | Running _, _ -> AlreadyRunning, MergeQueueState model
    | Merging _, _ -> AlreadyRunning, MergeQueueState model
    | NoBatch, queue ->
        let batch = queue |> selectBatch
        PerformBatchBuild batch, MergeQueueState { model with runningBatch = Running batch }

type BuildMessage =
    | Success of SHA
    | Failure

type IngestBuildResult =
    | NoOp
    | PerformBatchMerge of List<PullRequest> * SHA
    | BuildFailure

let private bisect (batch: Batch): Option<Batch * Batch> =
    if List.length batch <= 1 then
        None
    else
        let midpoint = (List.length batch) / 2
        List.splitAt midpoint batch |> Some

let ingestBuildUpdate (message: BuildMessage) (MergeQueueState model): IngestBuildResult * MergeQueueState =
    match model.runningBatch, message with
    | NoBatch, Failure ->
        NoOp, MergeQueueState { model with runningBatch = NoBatch } // The record update doesn't change the value...
    | Running batch, Failure ->
        match bisect batch with
        | None ->
            // no way to bisect -> nothing more to attempt
            let queue = model.queue |> removeFromQueue batch
            BuildFailure,
            MergeQueueState
                { model with
                      queue = queue
                      runningBatch = NoBatch }
        | Some(first, second) ->
            // update attempts
            let newQueue =
                model.queue
                |> List.map (fun (pr, a) ->
                    if List.contains pr first then (pr, a @ [ true ])
                    elif List.contains pr second then (pr, a @ [ false ])
                    else pr, a)
            BuildFailure,
            MergeQueueState
                { model with
                      queue = newQueue
                      runningBatch = NoBatch }

    | Merging _, Failure ->
        NoOp, MergeQueueState model
    | NoBatch, Success _ ->
        NoOp, MergeQueueState model
    | Running runningBatch, Success targetHead ->
        let result = PerformBatchMerge(runningBatch, targetHead)
        let state = MergeQueueState { model with runningBatch = Merging runningBatch }
        (result, state)
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
    | Merging merging, MergeMessage.Success ->
        let newQueue = model.queue |> removeFromQueue merging

        let state =
            MergeQueueState
                { model with
                      queue = newQueue
                      runningBatch = NoBatch }

        MergeComplete merging, state
    | Merging batch, MergeMessage.Failure ->
        let state = MergeQueueState { model with runningBatch = NoBatch }
        ReportMergeFailure batch, state
    | _, _ ->
        IngestMergeResult.NoOp, MergeQueueState model

type UpdatePullRequestResult =
    | NoOp
    | AbortRunningBatch of Batch * PullRequestID
    | AbortMergingBatch of Batch * PullRequestID

let private updateShaForEnqueuedPr (newValue: SHA) (id: PullRequestID) (model: MergeQueueModel): AttemptQueue =
    let updateCorrespondingSha (pr, a) =
        if pr.id = id then { pr with sha = newValue }, a
        else pr, a

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
                model.queue |> List.filter (fun (pr, _) -> pr.id <> id)

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
            let newQueue = model.queue |> List.filter (fun (pr, _) -> List.contains pr batch |> not)
            AbortMergingBatch(batch, id),
            MergeQueueState
                { model with
                      queue = newQueue
                      runningBatch = NoBatch }
        else
            let newQueue = model |> updateShaForEnqueuedPr newValue id
            let newModel = { model with queue = newQueue }
            NoOp, MergeQueueState newModel


// "Properties"
let peekCurrentQueue (MergeQueueState model): List<PullRequest> =
    model.queue |> List.map fst

let peekCurrentBatch (MergeQueueState model): Option<List<PullRequest>> =
    match model.runningBatch with
    | NoBatch -> None
    | Running batch -> Some batch
    | Merging batch -> Some batch
