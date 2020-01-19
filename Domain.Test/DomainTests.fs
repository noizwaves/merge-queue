module MergeQueue.Domain

open Xunit
open FsUnit.Xunit
open MergeQueue.Domain

let private passedCircleCI = commitStatus "circleci" CommitStatusState.Success
let private notPassedCircleCI = commitStatus "circleci" CommitStatusState.NoSuccess

let private one = pullRequest (pullRequestId 1) (sha "00001111") [ passedCircleCI ]
let private two = pullRequest (pullRequestId 22) (sha "00002222") [ passedCircleCI ]
let private three = pullRequest (pullRequestId 333) (sha "00003333") [ passedCircleCI ]
let private four = pullRequest (pullRequestId 4444) (sha "00004444") [ passedCircleCI ]

let private idleWithTwoPullRequests: MergeQueueState =
    emptyMergeQueue
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
    |> ingestBuildUpdate (BuildMessage.Success(sha "12345678"))
    |> snd

[<Fact>]
let ``Empty queue``() =
    let queue = emptyMergeQueue

    queue
    |> peekCurrentQueue
    |> should be Empty

    queue
    |> peekCurrentBatch
    |> should equal None

    queue
    |> previewBatches
    |> should be Empty

[<Fact>]
let ``Enqueue a Pull Request``() =
    let (result, state) =
        emptyMergeQueue |> enqueue one

    result |> should equal EnqueueResult.Success

    state
    |> peekCurrentQueue
    |> should equal [ one ]

    state
    |> peekCurrentBatch
    |> should equal None

    state
    |> previewBatches
    |> should equal [ [ one ] ]

[<Fact>]
let ``Enqueue a Pull Request that has failed a build step is rejected``() =
    let failingPr = pullRequest (pullRequestId 1) (sha "00001111") [ notPassedCircleCI ]
    let (result, state) =
        emptyMergeQueue |> enqueue failingPr

    result |> should equal EnqueueResult.RejectedNeedAllStatusesSuccess

    state |> should equal emptyMergeQueue

[<Fact>]
let ``Enqueuing a Pull Request that has no build statuses is rejected``() =
    let failingPr = pullRequest (pullRequestId 1) (sha "00001111") []
    let (result, state) =
        emptyMergeQueue |> enqueue failingPr

    result |> should equal EnqueueResult.RejectedNeedAllStatusesSuccess

    state |> should equal emptyMergeQueue

[<Fact>]
let ``Enqueue an already enqueued Pull Request``() =
    let singlePrQueueState =
        emptyMergeQueue
        |> enqueue one
        |> snd

    let (result, state) =
        singlePrQueueState |> enqueue one

    result |> should equal EnqueueResult.AlreadyEnqueued

    state
    |> peekCurrentQueue
    |> should equal [ one ]

    state
    |> previewBatches
    |> should equal [ [ one ] ]

[<Fact>]
let ``Enqueue multiple Pull Requests``() =
    let first = one
    let second = two


    let firstResult, firstState =
        emptyMergeQueue |> enqueue first

    let secondResult, secondState =
        firstState |> enqueue second


    firstResult |> should equal EnqueueResult.Success

    secondResult |> should equal EnqueueResult.Success

    secondState
    |> peekCurrentQueue
    |> should equal [ one; two ]

    secondState
    |> previewBatches
    |> should equal [ [ one; two ] ]

[<Fact>]
let ``Start a batch``() =
    let (result, state) =
        idleWithTwoPullRequests |> startBatch

    result |> should equal (StartBatchResult.PerformBatchBuild [ one; two ])

    state
    |> peekCurrentQueue
    |> should equal [ one; two ]

    state
    |> peekCurrentBatch
    |> should equal (Some [ one; two ])

    state
    |> previewBatches
    |> should equal [ [ one; two ] ]

[<Fact>]
let ``Attempt to start a second concurrent batch``() =
    let (_, runningBatch) =
        idleWithTwoPullRequests |> startBatch

    let (result, state) =
        runningBatch |> startBatch

    result |> should equal StartBatchResult.AlreadyRunning

    state
    |> peekCurrentQueue
    |> should equal [ one; two ]

    state
    |> peekCurrentBatch
    |> should equal (Some [ one; two ])

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
        emptyMergeQueue

    let (result, state) =
        queue |> startBatch

    result |> should equal StartBatchResult.EmptyQueue

    state |> should equal queue

// Batch Build Message ingestion

[<Fact>]
let ``Recieve message that batch successfully builds when batch is running``() =
    let runningQueue = runningBatchOfTwo

    let result, state =
        runningQueue |> ingestBuildUpdate (BuildMessage.Success(sha "12345678"))

    result |> should equal (IngestBuildResult.PerformBatchMerge([ one; two ], (sha "12345678")))

    state
    |> peekCurrentQueue
    |> should equal [ one; two ]

    state
    |> peekCurrentBatch
    |> should equal (Some [ one; two ])

[<Fact>]
let ``Recieve message that batch failed the build when batch is running``() =
    let runningQueue = runningBatchOfTwo

    let result, state =
        runningQueue |> ingestBuildUpdate BuildMessage.Failure

    result |> should equal (IngestBuildResult.BuildFailureWithRetry [ one; two ])

    state
    |> peekCurrentQueue
    |> should equal [ one; two ]

    state
    |> peekCurrentBatch
    |> should equal None

[<Fact>]
let ``Single PR batches that fail to build are dequeued``() =
    let runningBatchOfOne =
        emptyMergeQueue
        |> enqueue one
        |> snd
        |> startBatch
        |> snd

    let result, state =
        runningBatchOfOne |> ingestBuildUpdate BuildMessage.Failure

    result |> should equal (IngestBuildResult.BuildFailureNoRetry [ one ])

    state
    |> peekCurrentQueue
    |> should be Empty

[<Fact>]
let ``Recieve message that build failed when no running batch``() =
    let idleQueue = idleWithTwoPullRequests

    let result, state =
        idleQueue |> ingestBuildUpdate BuildMessage.Failure

    result |> should equal IngestBuildResult.NoOp

    state |> should equal idleQueue

[<Fact>]
let ``Recieve message that build succeeded when no running batch``() =
    let idleQueue = idleWithTwoPullRequests

    let result, state =
        idleQueue |> ingestBuildUpdate (BuildMessage.Success(sha "12345678"))

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
        mergingQueue |> ingestBuildUpdate (BuildMessage.Success(sha "12345678"))

    result |> should equal IngestBuildResult.NoOp

    state |> should equal mergingQueue

[<Fact>]
let ``A Pull Request enqueued during running batch is included in the next batch``() =
    let runningQueueDepthThree =
        runningBatchOfTwo
        |> enqueue three
        |> snd

    runningQueueDepthThree
    |> previewBatches
    |> should equal
           [ [ one; two ]
             [ three ] ]

    let finishedQueue =
        runningQueueDepthThree
        |> ingestBuildUpdate (BuildMessage.Success(sha "12345678"))
        |> snd
        |> ingestMergeUpdate MergeMessage.Success
        |> snd

    finishedQueue
    |> peekCurrentQueue
    |> should equal [ three ]

    let (result, _) =
        finishedQueue |> startBatch

    result |> should equal (StartBatchResult.PerformBatchBuild [ three ])

// Batch Merge Message ingestion

[<Fact>]
let ``Merge success message when batch is being merged``() =
    let mergingQueue = mergingBatchOfTwo

    let result, state =
        mergingQueue |> ingestMergeUpdate MergeMessage.Success

    result |> should equal (IngestMergeResult.MergeComplete [ one; two ])

    state |> should equal emptyMergeQueue

[<Fact>]
let ``Merge failure message when batch is being merged``() =
    let mergingQueue = mergingBatchOfTwo

    let result, state =
        mergingQueue |> ingestMergeUpdate (MergeMessage.Failure)

    state
    |> peekCurrentQueue
    |> should equal [ one; two ]

    result |> should equal (IngestMergeResult.ReportMergeFailure [ one; two ])

// PRs are updated

[<Fact>]
let ``The branch head for an enqueued PR is updated``() =
    let idle = idleWithTwoPullRequests

    let result, state =
        idle |> updatePullRequestSha (pullRequestId 1) (sha "10101010")

    result |> should equal UpdatePullRequestResult.NoOp

    let expectedState =
        emptyMergeQueue
        |> enqueue (pullRequest (pullRequestId 1) (sha "10101010") [ passedCircleCI ])
        |> snd
        |> enqueue (pullRequest (pullRequestId 22) (sha "00002222") [ passedCircleCI ])
        |> snd

    state |> should equal expectedState

[<Fact>]
let ``The branch head for a running PR is updated``() =
    let running = runningBatchOfTwo

    let result, state =
        running |> updatePullRequestSha (pullRequestId 1) (sha "10101010")

    result |> should equal (UpdatePullRequestResult.AbortRunningBatch([ one; two ], pullRequestId 1))

    let expectedState =
        emptyMergeQueue
        |> enqueue two
        |> snd

    state |> should equal expectedState

[<Fact>]
let ``The branch head for a unknown PR is updated when batch is running``() =
    let running = runningBatchOfTwo

    let result, state =
        running |> updatePullRequestSha (pullRequestId 404) (sha "40400404")

    result |> should equal UpdatePullRequestResult.NoOp

    state |> should equal running

[<Fact>]
let ``The branch head for an enqueued (but not running) PR is updated when batch is running``() =
    let runningBatchAndOnePrWaiting =
        runningBatchOfTwo
        |> enqueue three
        |> snd

    let result, state =
        runningBatchAndOnePrWaiting |> updatePullRequestSha (pullRequestId 333) (sha "30303030")

    result |> should equal UpdatePullRequestResult.NoOp

    let expectedState =
        runningBatchOfTwo
        |> enqueue (pullRequest (pullRequestId 333) (sha "30303030") [ passedCircleCI ])
        |> snd

    state |> should equal expectedState

[<Fact>]
let ``The branch head for an enqueued (but not batched) PR is updated when batch is merging``() =
    let mergingBatchAndOnePrWaiting =
        mergingBatchOfTwo
        |> enqueue three
        |> snd

    let result, state =
        mergingBatchAndOnePrWaiting |> updatePullRequestSha (pullRequestId 333) (sha "30303030")

    result |> should equal UpdatePullRequestResult.NoOp

    let expectedState =
        mergingBatchOfTwo
        |> enqueue (pullRequest (pullRequestId 333) (sha "30303030") [ passedCircleCI ])
        |> snd

    state |> should equal expectedState

[<Fact>]
let ``The branch head for a batched PR is updated when batch is merging``() =
    let mergingBatchAndOnePrWaiting =
        mergingBatchOfTwo
        |> enqueue three
        |> snd

    let result, state =
        mergingBatchAndOnePrWaiting |> updatePullRequestSha (pullRequestId 1) (sha "10101010")

    let expectedResult = UpdatePullRequestResult.AbortMergingBatch([ one; two ], (pullRequestId 1))
    result |> should equal expectedResult

    // a fail fast and safe state
    // we don't know which PRs were merged, so they should be removed from the queue
    let expectedState =
        emptyMergeQueue
        |> enqueue three
        |> snd
    state |> should equal expectedState

// Failed batches are bisected

[<Fact>]
let ``Failed batches are bisected upon build failure``() =
    let failedBuildOfFour =
        idleWithTwoPullRequests
        |> enqueue three
        |> snd
        |> enqueue four
        |> snd
        |> startBatch
        |> snd
        |> ingestBuildUpdate BuildMessage.Failure
        |> snd

    // next batch contains only `one` and `two`
    failedBuildOfFour
    |> previewBatches
    |> should equal
           [ [ one; two ]
             [ three; four ] ]

    let firstResult, firstState =
        failedBuildOfFour |> startBatch

    firstResult |> should equal (StartBatchResult.PerformBatchBuild [ one; two ])

    // fail the first bisected batch
    let _, bisectedFails =
        firstState |> ingestBuildUpdate BuildMessage.Failure

    // next batch contains only `one`
    bisectedFails
    |> previewBatches
    |> should equal
           [ [ one ]
             [ two ]
             [ three; four ] ]

    let secondResult, secondState =
        bisectedFails |> startBatch

    secondResult |> should equal (StartBatchResult.PerformBatchBuild [ one ])

    secondState
    |> peekCurrentQueue
    |> should equal [ one; two; three; four ]
