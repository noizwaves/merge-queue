module MergeQueue.Domain

// Models
type PullRequest = private PullRequest of string * string * int

type private RunningBatch = List<PullRequest> option

type MergeQueueState = private MergeQueueState of List<PullRequest> * RunningBatch

// Constructors
let emptyMergeQueue(): MergeQueueState =
    MergeQueueState([], None)

let makePullRequest (owner: string) (repo: string) (id: int): PullRequest =
    PullRequest(owner, repo, id)

// Operations
let enqueue (pullRequest: PullRequest) (MergeQueueState(queue, running)): MergeQueueState =
    MergeQueueState(queue @ [ pullRequest ], running)

type StartBatchResult =
    | Success of List<PullRequest>
    | AlreadyRunning
    | EmptyQueue

let startBatch (state: MergeQueueState): StartBatchResult * MergeQueueState =
    match state with
    | MergeQueueState([], _) -> EmptyQueue, state
    | MergeQueueState(queue, Some batch) -> AlreadyRunning, (MergeQueueState(queue, Some batch))
    | MergeQueueState(queue, None) -> Success queue, (MergeQueueState(queue, Some queue))

type BuildProgressUpdate =
    | Success
    | Failure

let ingestBuildUpdate (update: BuildProgressUpdate) (MergeQueueState(queue, running)): MergeQueueState =
    match update, running with
    | Failure, _ -> MergeQueueState(queue, None)
    | Success, None -> MergeQueueState(queue, None)
    | Success, Some batch ->
        let newQueue = queue |> List.filter (fun n -> List.contains n batch |> not)
        MergeQueueState(newQueue, None)

// Views
let getDepth (MergeQueueState(queue, _)): int =
    queue |> List.length
