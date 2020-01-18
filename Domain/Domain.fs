module MergeQueue.Domain

// Models
type private PullRequestModel =
    { owner: string
      repo: string
      id: int }

type PullRequest = private PullRequest of PullRequestModel

type private RunningBatch = List<PullRequest> option

type private MergeQueueModel =
    { queue: List<PullRequest>
      runningBatch: RunningBatch }

type MergeQueueState = private MergeQueueState of MergeQueueModel

// Constructors
let emptyMergeQueue(): MergeQueueState =
    MergeQueueState
        { queue = []
          runningBatch = None }

let makePullRequest (owner: string) (repo: string) (id: int): PullRequest =
    PullRequest
        { owner = owner
          repo = repo
          id = id }

// Operations
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
    match model.queue, model.runningBatch with
    | [], _ -> EmptyQueue, MergeQueueState model
    | _, Some _ -> AlreadyRunning, MergeQueueState model
    | queue, None -> Success queue, MergeQueueState { model with runningBatch = Some queue }

type BuildProgressUpdate =
    | Success
    | Failure

let ingestBuildUpdate (update: BuildProgressUpdate) (MergeQueueState model): MergeQueueState =
    match update, model.runningBatch with
    | Failure, _ -> MergeQueueState { model with runningBatch = None }
    | Success, None -> MergeQueueState model
    | Success, Some batch ->
        let newQueue = model.queue |> List.filter (fun n -> List.contains n batch |> not)
        MergeQueueState
            { model with
                  queue = newQueue
                  runningBatch = None }

// Views
let getDepth (MergeQueueState model): int =
    model.queue |> List.length
