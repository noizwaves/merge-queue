module MergeQueue.Domain.Tests

open Xunit
open FsUnit.Xunit
open MergeQueue.Domain
open MergeQueue.DomainTypes

let private passedLinter = commitStatus "uberlinter" CommitStatusState.Success
let private runningCircleCI = commitStatus "circleci" CommitStatusState.Pending
let private passedCircleCI = commitStatus "circleci" CommitStatusState.Success
let private failedCircleCI = commitStatus "circleci" CommitStatusState.Failure

let private one = pullRequest (pullRequestId 1) (sha "00001111") [ passedCircleCI ]
let private two = pullRequest (pullRequestId 22) (sha "00002222") [ passedCircleCI ]
let private three = pullRequest (pullRequestId 333) (sha "00003333") [ passedCircleCI ]
let private four = pullRequest (pullRequestId 4444) (sha "00004444") [ passedCircleCI ]

let private idleWithTwoPullRequests: MergeQueue =
    emptyMergeQueue
    |> enqueue one
    |> snd
    |> enqueue two
    |> snd

let private runningBatchOfTwo: MergeQueue =
    idleWithTwoPullRequests
    |> startBatch
    |> snd

let private mergingBatchOfTwo: MergeQueue =
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
    |> previewExecutionPlan
    |> should be Empty

    queue
    |> peekSinBin
    |> should be Empty

[<Fact>]
let ``Enqueue a Pull Request``() =
    let (result, state) =
        emptyMergeQueue |> enqueue one

    result |> should equal EnqueueResult.Enqueued

    state
    |> peekCurrentQueue
    |> should equal [ one ]

    state
    |> peekCurrentBatch
    |> should equal None

    state
    |> previewExecutionPlan
    |> should equal [ PlannedBatch [ one.id ] ]

[<Fact>]
let ``Enqueue a Pull Request with a failing commit status is rejected``() =
    let failingPr = pullRequest (pullRequestId 1) (sha "00001111") [ passedLinter; failedCircleCI ]
    let (result, state) =
        emptyMergeQueue |> enqueue failingPr

    result |> should equal EnqueueResult.RejectedFailingBuildStatus

    state |> should equal emptyMergeQueue

[<Fact>]
let ``Enqueue a Pull Request with a pending commit status is sin binned``() =
    let runningPr = pullRequest (pullRequestId 1) (sha "00001111") [ passedLinter; runningCircleCI ]
    let (result, state) =
        emptyMergeQueue |> enqueue runningPr

    result |> should equal EnqueueResult.SinBinned

    state
    |> peekCurrentQueue
    |> should be Empty

    state
    |> peekSinBin
    |> should equal [ runningPr ]

[<Fact>]
let ``Enqueuing a Pull Request that has no commit statuses is rejected``() =
    let failingPr = pullRequest (pullRequestId 1) (sha "00001111") []
    let (result, state) =
        emptyMergeQueue |> enqueue failingPr

    result |> should equal EnqueueResult.RejectedFailingBuildStatus

    state |> should equal emptyMergeQueue

[<Fact>]
let ``Enqueue an already enqueued Pull Request``() =
    let singlePrQueueState =
        emptyMergeQueue
        |> enqueue one
        |> snd

    let (result, state) =
        singlePrQueueState |> enqueue (pullRequest (pullRequestId 1) (sha "92929292") [ passedCircleCI ])

    result |> should equal EnqueueResult.AlreadyEnqueued

    state
    |> peekCurrentQueue
    |> should equal [ one ]

    state
    |> previewExecutionPlan
    |> should equal [ PlannedBatch [ one.id ] ]

[<Fact>]
let ``Enqueue a sin binned Pull Request``() =
    let singlePrInSinBin =
        emptyMergeQueue
        |> enqueue one
        |> snd
        |> updatePullRequestSha (pullRequestId 1) (sha "10101010")
        |> snd

    let result, state =
        singlePrInSinBin |> enqueue (pullRequest (pullRequestId 1) (sha "92929292") [ passedCircleCI ])

    result |> should equal EnqueueResult.AlreadyEnqueued

    state |> should equal singlePrInSinBin

[<Fact>]
let ``Enqueue multiple Pull Requests``() =
    let first = one
    let second = two


    let firstResult, firstState =
        emptyMergeQueue |> enqueue first

    let secondResult, secondState =
        firstState |> enqueue second


    firstResult |> should equal EnqueueResult.Enqueued

    secondResult |> should equal EnqueueResult.Enqueued

    secondState
    |> peekCurrentQueue
    |> should equal [ one; two ]

    secondState
    |> previewExecutionPlan
    |> should equal [ PlannedBatch [ one.id; two.id ] ]

// Dequeue a Pull Request
[<Fact>]
let ``Dequeue an enqueued Pull Request``() =
    let result, state =
        idleWithTwoPullRequests |> dequeue (pullRequestId 1)

    result |> should equal (DequeueResult.Dequeued)

    state
    |> peekCurrentQueue
    |> should equal [ two ]

    state
    |> peekSinBin
    |> should be Empty

[<Fact>]
let ``Dequeue a sin binned Pull Request``() =
    let idleWithOneEnqueuedOneSinBinned =
        idleWithTwoPullRequests
        |> updatePullRequestSha (pullRequestId 1) (sha "10101010")
        |> snd

    let result, state =
        idleWithOneEnqueuedOneSinBinned |> dequeue (pullRequestId 1)

    result |> should equal (DequeueResult.Dequeued)

    state
    |> peekCurrentQueue
    |> should equal [ two ]

    state
    |> peekSinBin
    |> should be Empty

[<Fact>]
let ``Dequeue an unknown Pull Request``() =
    let result, state =
        idleWithTwoPullRequests |> dequeue (pullRequestId 404)

    result |> should equal (DequeueResult.NotFound)

    state
    |> peekCurrentQueue
    |> should equal [ one; two ]

    state
    |> peekSinBin
    |> should be Empty

[<Fact>]
let ``Dequeue a Pull Request that is in a running batch``() =
    let result, state =
        runningBatchOfTwo |> dequeue (pullRequestId 1)

    result |> should equal (DequeueResult.DequeuedAndAbortRunningBatch([ one; two ], (pullRequestId 1)))

    state
    |> peekCurrentBatch
    |> should equal None

    state
    |> peekCurrentQueue
    |> should equal [ two ]

    state
    |> peekSinBin
    |> should be Empty

[<Fact>]
let ``Dequeue a Pull Request that is waiting behind a running batch``() =
    let result, state =
        runningBatchOfTwo
        |> enqueue three
        |> snd
        |> dequeue (pullRequestId 333)

    result |> should equal DequeueResult.Dequeued

    state
    |> peekCurrentBatch
    |> should equal (Some [ one; two ])

    state
    |> peekCurrentQueue
    |> should be Empty

    state
    |> peekSinBin
    |> should be Empty

[<Fact>]
let ``Dequeue a Pull Request that is in a merging batch``() =
    let result, state =
        mergingBatchOfTwo |> dequeue (pullRequestId 1)

    result |> should equal DequeueResult.RejectedInMergingBatch

    state |> should equal mergingBatchOfTwo

[<Fact>]
let ``Dequeue a Pull Request that is waiting behind a merging batch``() =
    let result, state =
        mergingBatchOfTwo
        |> enqueue three
        |> snd
        |> dequeue (pullRequestId 333)

    result |> should equal DequeueResult.Dequeued

    state
    |> peekCurrentBatch
    |> should equal (Some [ one; two ])

    state
    |> peekCurrentQueue
    |> should be Empty

    state
    |> peekSinBin
    |> should be Empty

// Starting a batch

[<Fact>]
let ``Start a batch``() =
    let (result, state) =
        idleWithTwoPullRequests |> startBatch

    result |> should equal (StartBatchResult.PerformBatchBuild [ one; two ])

    state
    |> peekCurrentQueue
    |> should be Empty

    state
    |> peekCurrentBatch
    |> should equal (Some [ one; two ])

    state
    |> previewExecutionPlan
    |> should equal [ PlannedBatch [ one.id; two.id ] ]

[<Fact>]
let ``Attempt to start a second concurrent batch``() =
    let (_, runningBatch) =
        idleWithTwoPullRequests |> startBatch

    let (result, state) =
        runningBatch |> startBatch

    result |> should equal StartBatchResult.AlreadyRunning

    state
    |> peekCurrentQueue
    |> should be Empty

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
    |> should be Empty

    state
    |> peekCurrentBatch
    |> should equal (Some [ one; two ])

[<Fact>]
let ``Recieve message that batch failed the build when batch is running``() =
    let runningQueue = runningBatchOfTwo

    let result, state =
        runningQueue |> ingestBuildUpdate BuildMessage.Failure

    result |> should equal (IngestBuildResult.ReportBuildFailureWithRetry [ one; two ])

    state
    |> peekCurrentQueue
    |> should equal [ one; two ]

    state
    |> peekCurrentBatch
    |> should equal None

    state
    |> peekSinBin
    |> should be Empty

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

    result |> should equal (IngestBuildResult.ReportBuildFailureNoRetry [ one ])

    state
    |> peekCurrentQueue
    |> should be Empty

    state
    |> peekCurrentBatch
    |> should equal None

    state
    |> peekSinBin
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
    |> previewExecutionPlan
    |> should equal
           [ PlannedBatch [ one.id; two.id ]
             PlannedBatch [ three.id ] ]

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

// updated PRs should be removed from the queue until their builds pass again
// at which point they are added to the end of the queue again

[<Fact>]
let ``The branch head for an enqueued PR is updated``() =
    let idle = idleWithTwoPullRequests

    let result, state =
        idle |> updatePullRequestSha (pullRequestId 1) (sha "10101010")

    result |> should equal UpdatePullRequestResult.NoOp

    state
    |> peekCurrentQueue
    |> should equal [ two ]

    // TODO: should the statuses be different? None? our PullRequest technically `passesBuild`
    state
    |> peekSinBin
    |> should equal [ { one with sha = (sha "10101010") } ]

[<Fact>]
let ``The branch head for a running PR is updated``() =
    let running = runningBatchOfTwo

    let result, state =
        running |> updatePullRequestSha (pullRequestId 1) (sha "10101010")

    result |> should equal (UpdatePullRequestResult.AbortRunningBatch([ one; two ], pullRequestId 1))

    state
    |> peekCurrentQueue
    |> should equal [ two ]

    // TODO: should the statuses be different? None? our PullRequest technically `passesBuild`
    state
    |> peekSinBin
    |> should equal [ { one with sha = (sha "10101010") } ]

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

    state
    |> peekCurrentBatch
    |> should equal (Some [ one; two ])

    // TODO: should the statuses be different? None? our PullRequest technically `passesBuild`
    state
    |> peekSinBin
    |> should equal [ { three with sha = (sha "30303030") } ]

[<Fact>]
let ``The branch head for an enqueued (but not batched) PR is updated when batch is merging``() =
    let mergingBatchAndOnePrWaiting =
        mergingBatchOfTwo
        |> enqueue three
        |> snd

    let result, state =
        mergingBatchAndOnePrWaiting |> updatePullRequestSha (pullRequestId 333) (sha "30303030")

    result |> should equal UpdatePullRequestResult.NoOp

    state
    |> peekCurrentBatch
    |> should equal (Some [ one; two ])

    // TODO: should the statuses be different? None? our PullRequest technically `passesBuild`
    state
    |> peekSinBin
    |> should equal [ { three with sha = (sha "30303030") } ]

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

    // Future improvement here!
    // a fail fast and safe state
    // we don't know which PRs were merged, so they should be removed from the queue

    state
    |> peekCurrentQueue
    |> should equal [ three ]

    state
    |> peekSinBin
    |> should be Empty

// Updated PR commit statuses come in

[<Fact>]
let ``An updated PR with successful build status is re-enqueued at the bottom``() =
    let awaitingStatusInfo =
        idleWithTwoPullRequests
        |> updatePullRequestSha (pullRequestId 1) (sha "10101010")
        |> snd

    let state =
        awaitingStatusInfo |> updateStatuses (pullRequestId 1) (sha "10101010") [ passedCircleCI ]

    state
    |> peekCurrentQueue
    |> should equal
           [ two
             (pullRequest (pullRequestId 1) (sha "10101010") [ passedCircleCI ]) ]

[<Fact>]
let ``An updated PR with failing build status is not re-enqueued``() =
    let awaitingStatusInfo =
        idleWithTwoPullRequests
        |> updatePullRequestSha (pullRequestId 1) (sha "10101010")
        |> snd

    let state =
        awaitingStatusInfo |> updateStatuses (pullRequestId 1) (sha "10101010") [ failedCircleCI ]

    state
    |> peekCurrentQueue
    |> should equal [ two ]

[<Fact>]
let ``Updates for a PR not in the sin bin is ignored``() =
    let awaitingStatusInfo =
        idleWithTwoPullRequests
        |> updatePullRequestSha (pullRequestId 1) (sha "10101010")
        |> snd

    let state =
        awaitingStatusInfo |> updateStatuses (pullRequestId 333) (sha "10101010") [ passedCircleCI ]

    state |> should equal awaitingStatusInfo

[<Fact>]
let ``Old updates for a PR with many SHA updates are ignored``() =
    let awaitingStatusInfo =
        idleWithTwoPullRequests
        |> updatePullRequestSha (pullRequestId 1) (sha "10101010")
        |> snd
        |> updatePullRequestSha (pullRequestId 1) (sha "11001100")
        |> snd

    let state =
        awaitingStatusInfo |> updateStatuses (pullRequestId 1) (sha "10101010") [ passedCircleCI ]

    state
    |> peekCurrentQueue
    |> should equal [ two ]

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
    |> previewExecutionPlan
    |> should equal
           [ PlannedBatch [ one.id; two.id ]
             PlannedBatch [ three.id; four.id ] ]

    let firstResult, firstState =
        failedBuildOfFour |> startBatch

    firstResult |> should equal (StartBatchResult.PerformBatchBuild [ one; two ])

    // fail the first bisected batch
    let _, bisectedFails =
        firstState |> ingestBuildUpdate BuildMessage.Failure

    // next batch contains only `one`
    bisectedFails
    |> previewExecutionPlan
    |> should equal
           [ PlannedBatch [ one.id ]
             PlannedBatch [ two.id ]
             PlannedBatch [ three.id; four.id ] ]

    let secondResult, secondState =
        bisectedFails |> startBatch

    secondResult |> should equal (StartBatchResult.PerformBatchBuild [ one ])

    secondState
    |> peekCurrentBatch
    |> should equal (Some [ one ])

    secondState
    |> peekCurrentQueue
    |> should equal [ two; three; four ]
