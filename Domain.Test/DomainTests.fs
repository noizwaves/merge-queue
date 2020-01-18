module MergeQueue.Domain

open Xunit
open FsUnit.Xunit
open MergeQueue.Domain


let private waitingQueueWithTwoPullRequests: MergeQueueState =
    emptyMergeQueue()
    |> enqueue (makePullRequest "someOwner" "someRepo" 1)
    |> snd
    |> enqueue (makePullRequest "someOwner" "someRepo" 22)
    |> snd

[<Fact>]
let ``Empty queue``() =
    emptyMergeQueue()
    |> getDepth
    |> should equal 0

[<Fact>]
let ``Enqueue a Pull Request``() =
    let pr = makePullRequest "someOwner" "someRepo" 1

    let (result, state) =
        emptyMergeQueue() |> enqueue pr

    result |> should equal EnqueueResult.Success

    state
    |> getDepth
    |> should equal 1

[<Fact>]
let ``Enqueue an already enqueued Pull Request``() =
    let pr = makePullRequest "someOwner" "someRepo" 1

    let singlePrQueueState =
        emptyMergeQueue()
        |> enqueue pr
        |> snd

    let (result, state) =
        singlePrQueueState |> enqueue pr

    result |> should equal EnqueueResult.AlreadyEnqueued

    state
    |> getDepth
    |> should equal 1

[<Fact>]
let ``Enqueue multiple Pull Requests``() =
    let first = makePullRequest "someOwner" "someRepo" 1
    let second = makePullRequest "someOwner" "someRepo" 22


    let firstResult, firstState =
        emptyMergeQueue() |> enqueue first

    let secondResult, secondState =
        firstState |> enqueue second


    firstResult |> should equal EnqueueResult.Success

    secondResult |> should equal EnqueueResult.Success

    secondState
    |> getDepth
    |> should equal 2

[<Fact>]
let ``Start a batch``() =
    let (result, state) =
        waitingQueueWithTwoPullRequests |> startBatch

    result
    |> should equal
           (StartBatchResult.Success
               [ makePullRequest "someOwner" "someRepo" 1
                 makePullRequest "someOwner" "someRepo" 22 ])

    state
    |> getDepth
    |> should equal 2

[<Fact>]
let ``Attempt to start a second concurrent batch``() =
    let (_, runningBatch) =
        waitingQueueWithTwoPullRequests |> startBatch

    let (result, state) =
        runningBatch |> startBatch

    result |> should equal StartBatchResult.AlreadyRunning

    state
    |> getDepth
    |> should equal 2

[<Fact>]
let ``Attempt to start a second concurrent batch during merging``() =
    let (_, mergingState) =
        waitingQueueWithTwoPullRequests
        |> startBatch
        |> snd
        |> ingestBuildUpdate BuildMessage.Success

    let result, state =
        mergingState |> startBatch

    result |> should equal AlreadyRunning

    state |> should equal mergingState

[<Fact>]
let ``Attempt to start a batch on an empty queue``() =
    let queue =
        emptyMergeQueue()

    let (result, state) =
        queue |> startBatch

    result |> should equal StartBatchResult.EmptyQueue

    state |> should equal queue

// Batch Build Message ingestion

[<Fact>]
let ``Recieve message that batch successfully builds when batch is running``() =
    let runningQueue =
        waitingQueueWithTwoPullRequests
        |> startBatch
        |> snd

    let result, state =
        runningQueue |> ingestBuildUpdate (BuildMessage.Success)

    result
    |> should equal
           (IngestBuildResult.PerformBatchMerge
               [ makePullRequest "someOwner" "someRepo" 1
                 makePullRequest "someOwner" "someRepo" 22 ])

    state
    |> getDepth
    |> should equal 2

[<Fact>]
let ``Recieve message that batch failed the build when batch is running``() =
    let runningQueue =
        waitingQueueWithTwoPullRequests
        |> startBatch
        |> snd

    let result, state =
        runningQueue |> ingestBuildUpdate (BuildMessage.Failure)

    result |> should equal IngestBuildResult.BuildFailure

    state
    |> getDepth
    |> should equal 2

[<Fact>]
let ``Recieve message that build failed when no running batch``() =
    let runningQueue =
        waitingQueueWithTwoPullRequests

    let result, state =
        runningQueue |> ingestBuildUpdate (BuildMessage.Failure)

    result |> should equal IngestBuildResult.NoOp

    state |> should equal runningQueue

[<Fact>]
let ``Recieve message that build succeeded when no running batch``() =
    let runningQueue =
        waitingQueueWithTwoPullRequests

    let result, state =
        runningQueue |> ingestBuildUpdate (BuildMessage.Success)

    result |> should equal IngestBuildResult.NoOp

    state |> should equal runningQueue

[<Fact>]
let ``Recieve message that build failed when batch is being merged``() =
    let (_, mergingState) =
        waitingQueueWithTwoPullRequests
        |> startBatch
        |> snd
        |> ingestBuildUpdate BuildMessage.Success

    let (result, state) =
        mergingState |> ingestBuildUpdate BuildMessage.Failure

    result |> should equal IngestBuildResult.NoOp

    state |> should equal mergingState

[<Fact>]
let ``Recieve message that build succeeded when batch is being merged``() =
    let (_, mergingState) =
        waitingQueueWithTwoPullRequests
        |> startBatch
        |> snd
        |> ingestBuildUpdate BuildMessage.Success

    let (result, state) =
        mergingState |> ingestBuildUpdate BuildMessage.Success

    result |> should equal IngestBuildResult.NoOp

    state |> should equal mergingState

[<Fact>]
let ``A Pull Request enqueued during running batch is included in the next batch``() =
    let runningQueue =
        waitingQueueWithTwoPullRequests
        |> startBatch
        |> snd
        |> enqueue (makePullRequest "someOwner" "someRepo" 333)
        |> snd

    runningQueue
    |> getDepth
    |> should equal 3

    let finishedQueue =
        runningQueue
        |> ingestBuildUpdate (BuildMessage.Success)
        |> snd
        |> ingestMergeUpdate (MergeMessage.Success)
        |> snd

    finishedQueue
    |> getDepth
    |> should equal 1

    let (result, _) =
        finishedQueue |> startBatch

    result |> should equal (StartBatchResult.Success [ makePullRequest "someOwner" "someRepo" 333 ])

// Batch Merge Message ingestion

[<Fact>]
let ``Merge success message when batch is being merged``() =
    let mergingQueue =
        waitingQueueWithTwoPullRequests
        |> startBatch
        |> snd
        |> ingestBuildUpdate (BuildMessage.Success)
        |> snd

    let result, state =
        mergingQueue |> ingestMergeUpdate (MergeMessage.Success)

    state
    |> getDepth
    |> should equal 0

    result
    |> should equal
           (IngestMergeResult.MergeComplete
               [ makePullRequest "someOwner" "someRepo" 1
                 makePullRequest "someOwner" "someRepo" 22 ])

[<Fact>]
let ``Merge failure message when batch is being merged``() =
    let mergingQueue =
        waitingQueueWithTwoPullRequests
        |> startBatch
        |> snd
        |> ingestBuildUpdate (BuildMessage.Success)
        |> snd

    let result, state =
        mergingQueue |> ingestMergeUpdate (MergeMessage.Failure)

    state
    |> getDepth
    |> should equal 2

    result
    |> should equal
           (IngestMergeResult.ReportMergeFailure
               [ makePullRequest "someOwner" "someRepo" 1
                 makePullRequest "someOwner" "someRepo" 22 ])
