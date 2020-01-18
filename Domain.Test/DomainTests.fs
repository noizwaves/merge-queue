module MergeQueue.Domain

open Xunit
open FsUnit.Xunit
open MergeQueue.Domain


let private one = makePullRequest "someOwner" "someRepo" 1
let private two = makePullRequest "someOwner" "someRepo" 22
let private three = makePullRequest "someOwner" "someRepo" 333

let private idleWithTwoPullRequests: MergeQueueState =
    emptyMergeQueue()
    |> enqueue one
    |> snd
    |> enqueue two
    |> snd

let private runningBatchOfTwo: MergeQueueState =
    idleWithTwoPullRequests
    |> startBatch
    |> snd

let private mergingBatchOfTwo: MergeQueueState =
    runningBatchOfTwo
    |> ingestBuildUpdate BuildMessage.Success
    |> snd

[<Fact>]
let ``Empty queue``() =
    emptyMergeQueue()
    |> getDepth
    |> should equal 0

[<Fact>]
let ``Enqueue a Pull Request``() =
    let pr = one

    let (result, state) =
        emptyMergeQueue() |> enqueue pr

    result |> should equal EnqueueResult.Success

    state
    |> getDepth
    |> should equal 1

[<Fact>]
let ``Enqueue an already enqueued Pull Request``() =
    let pr = one

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
    let first = one
    let second = two


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
        idleWithTwoPullRequests |> startBatch

    result |> should equal (StartBatchResult.Success [ one; two ])

    state
    |> getDepth
    |> should equal 2

[<Fact>]
let ``Attempt to start a second concurrent batch``() =
    let (_, runningBatch) =
        idleWithTwoPullRequests |> startBatch

    let (result, state) =
        runningBatch |> startBatch

    result |> should equal StartBatchResult.AlreadyRunning

    state
    |> getDepth
    |> should equal 2

[<Fact>]
let ``Attempt to start a second concurrent batch during merging``() =
    let mergingQueue = mergingBatchOfTwo

    let result, state =
        mergingQueue |> startBatch

    result |> should equal AlreadyRunning

    state |> should equal mergingQueue

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
    let runningQueue = runningBatchOfTwo

    let result, state =
        runningQueue |> ingestBuildUpdate (BuildMessage.Success)

    result |> should equal (IngestBuildResult.PerformBatchMerge [ one; two ])

    state
    |> getDepth
    |> should equal 2

[<Fact>]
let ``Recieve message that batch failed the build when batch is running``() =
    let runningQueue = runningBatchOfTwo

    let result, state =
        runningQueue |> ingestBuildUpdate (BuildMessage.Failure)

    result |> should equal IngestBuildResult.BuildFailure

    state
    |> getDepth
    |> should equal 2

[<Fact>]
let ``Recieve message that build failed when no running batch``() =
    let idleQueue = idleWithTwoPullRequests

    let result, state =
        idleQueue |> ingestBuildUpdate (BuildMessage.Failure)

    result |> should equal IngestBuildResult.NoOp

    state |> should equal idleQueue

[<Fact>]
let ``Recieve message that build succeeded when no running batch``() =
    let idleQueue = idleWithTwoPullRequests

    let result, state =
        idleQueue |> ingestBuildUpdate (BuildMessage.Success)

    result |> should equal IngestBuildResult.NoOp

    state |> should equal idleQueue

[<Fact>]
let ``Recieve message that build failed when batch is being merged``() =
    let mergingQueue = mergingBatchOfTwo

    let (result, state) =
        mergingQueue |> ingestBuildUpdate BuildMessage.Failure

    result |> should equal IngestBuildResult.NoOp

    state |> should equal mergingQueue

[<Fact>]
let ``Recieve message that build succeeded when batch is being merged``() =
    let mergingQueue = mergingBatchOfTwo

    let (result, state) =
        mergingQueue |> ingestBuildUpdate BuildMessage.Success

    result |> should equal IngestBuildResult.NoOp

    state |> should equal mergingQueue

[<Fact>]
let ``A Pull Request enqueued during running batch is included in the next batch``() =
    let runningQueueDepthThree =
        runningBatchOfTwo
        |> enqueue three
        |> snd

    runningQueueDepthThree
    |> getDepth
    |> should equal 3

    let finishedQueue =
        runningQueueDepthThree
        |> ingestBuildUpdate (BuildMessage.Success)
        |> snd
        |> ingestMergeUpdate (MergeMessage.Success)
        |> snd

    finishedQueue
    |> getDepth
    |> should equal 1

    let (result, _) =
        finishedQueue |> startBatch

    result |> should equal (StartBatchResult.Success [ three ])

// Batch Merge Message ingestion

[<Fact>]
let ``Merge success message when batch is being merged``() =
    let mergingQueue = mergingBatchOfTwo

    let result, state =
        mergingQueue |> ingestMergeUpdate (MergeMessage.Success)

    state
    |> getDepth
    |> should equal 0

    result |> should equal (IngestMergeResult.MergeComplete [ one; two ])

[<Fact>]
let ``Merge failure message when batch is being merged``() =
    let mergingQueue = mergingBatchOfTwo

    let result, state =
        mergingQueue |> ingestMergeUpdate (MergeMessage.Failure)

    state
    |> getDepth
    |> should equal 2

    result |> should equal (IngestMergeResult.ReportMergeFailure [ one; two ])
