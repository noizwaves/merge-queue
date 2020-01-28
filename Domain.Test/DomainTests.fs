module MergeQueue.Domain.Tests

open Xunit
open FsUnit.Xunit
open MergeQueue.Domain
open MergeQueue.DomainTypes
open MergeQueue.DbTypes
open MergeQueue.Commands

let private passedLinter = CommitStatus.create "uberlinter" CommitStatusState.Success
let private runningCircleCI = CommitStatus.create "circleci" CommitStatusState.Pending
let private passedCircleCI = CommitStatus.create "circleci" CommitStatusState.Success
let private failedCircleCI = CommitStatus.create "circleci" CommitStatusState.Failure

let private one = PullRequest.pullRequest (PullRequestID.create 1) (SHA.create "00001111") [ passedCircleCI ]
let private two = PullRequest.pullRequest (PullRequestID.create 22) (SHA.create "00002222") [ passedCircleCI ]
let private three = PullRequest.pullRequest (PullRequestID.create 333) (SHA.create "00003333") [ passedCircleCI ]
let private four = PullRequest.pullRequest (PullRequestID.create 4444) (SHA.create "00004444") [ passedCircleCI ]

let private applyCommands<'a> (func: Load -> Save -> 'a) (initial: MergeQueue): 'a * MergeQueue =
    // SMELL: collect all updates
    let mutable state = initial
    let update v = state <- v
    let fetch() = state

    func fetch update, state

// SMELL: using commands to construct domain objects for testing domain logic
let private idleWithTwoPullRequests: MergeQueue =
    MergeQueue.empty
    |> applyCommands (fun load save ->
        enqueue load save one |> ignore
        enqueue load save two)
    |> snd

let private runningBatchOfTwo: MergeQueue =
    idleWithTwoPullRequests
    |> applyCommands (fun load save -> startBatch load save ())
    |> snd

let private mergingBatchOfTwo: MergeQueue =
    runningBatchOfTwo
    |> applyCommands (fun load save -> ingestBuildUpdate load save (BuildMessage.Success(SHA.create "12345678")))
    |> snd

[<Fact>]
let ``Empty queue``() =
    let queue = MergeQueue.empty

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
        MergeQueue.empty |> applyCommands (fun load save -> enqueue load save one)

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
    let failingPr =
        PullRequest.pullRequest (PullRequestID.create 1) (SHA.create "00001111") [ passedLinter; failedCircleCI ]
    let (result, state) =
        MergeQueue.empty |> applyCommands (fun load save -> enqueue load save failingPr)

    result |> should equal EnqueueResult.RejectedFailingBuildStatus

    state |> should equal MergeQueue.empty

[<Fact>]
let ``Enqueue a Pull Request with a pending commit status is sin binned``() =
    let runningPr =
        PullRequest.pullRequest (PullRequestID.create 1) (SHA.create "00001111") [ passedLinter; runningCircleCI ]
    let (result, state) =
        MergeQueue.empty |> applyCommands (fun load save -> enqueue load save runningPr)

    result |> should equal EnqueueResult.SinBinned

    state
    |> peekCurrentQueue
    |> should be Empty

    state
    |> peekSinBin
    |> should equal [ runningPr ]

[<Fact>]
let ``Enqueuing a Pull Request that has no commit statuses is rejected``() =
    let failingPr = PullRequest.pullRequest (PullRequestID.create 1) (SHA.create "00001111") []
    let (result, state) =
        MergeQueue.empty |> applyCommands (fun load save -> enqueue load save failingPr)

    result |> should equal EnqueueResult.RejectedFailingBuildStatus

    state |> should equal MergeQueue.empty

[<Fact>]
let ``Enqueue an already enqueued Pull Request``() =
    let singlePrQueueState =
        MergeQueue.empty
        |> applyCommands (fun load save -> enqueue load save one)
        |> snd

    let duplicatePr = (PullRequest.pullRequest (PullRequestID.create 1) (SHA.create "92929292") [ passedCircleCI ])
    let (result, state) =
        singlePrQueueState |> applyCommands (fun load save -> enqueue load save duplicatePr)

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
        MergeQueue.empty
        |> applyCommands (fun load save ->
            enqueue load save one |> ignore
            updatePullRequestSha load save (PullRequestID.create 1) (SHA.create "10101010") |> ignore)
        |> snd

    let result, state =
        singlePrInSinBin
        |> applyCommands
            (fun load save ->
            enqueue load save
                (PullRequest.pullRequest (PullRequestID.create 1) (SHA.create "92929292") [ passedCircleCI ]))

    result |> should equal EnqueueResult.AlreadyEnqueued

    state |> should equal singlePrInSinBin

[<Fact>]
let ``Enqueue multiple Pull Requests``() =
    let first = one
    let second = two

    let firstResult, firstState =
        MergeQueue.empty |> applyCommands (fun load save -> enqueue load save first)

    let secondResult, secondState =
        firstState |> applyCommands (fun load save -> enqueue load save second)


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
        idleWithTwoPullRequests |> applyCommands (fun load save -> dequeue load save (PullRequestID.create 1))

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
        |> applyCommands
            (fun load save -> updatePullRequestSha load save (PullRequestID.create 1) (SHA.create "10101010"))
        |> snd

    let result, state =
        idleWithOneEnqueuedOneSinBinned
        |> applyCommands (fun load save -> dequeue load save (PullRequestID.create 1))

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
        idleWithTwoPullRequests |> applyCommands (fun load save -> dequeue load save (PullRequestID.create 404))

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
        runningBatchOfTwo |> applyCommands (fun load save -> dequeue load save (PullRequestID.create 1))

    result |> should equal (DequeueResult.DequeuedAndAbortRunningBatch([ one; two ], (PullRequestID.create 1)))

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
        |> applyCommands (fun load save ->
            enqueue load save three |> ignore
            dequeue load save (PullRequestID.create 333))

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
        mergingBatchOfTwo |> applyCommands (fun load save -> dequeue load save (PullRequestID.create 1))

    result |> should equal DequeueResult.RejectedInMergingBatch

    state |> should equal mergingBatchOfTwo

[<Fact>]
let ``Dequeue a Pull Request that is waiting behind a merging batch``() =
    let result, state =
        mergingBatchOfTwo
        |> applyCommands (fun load save ->
            enqueue load save three |> ignore
            dequeue load save (PullRequestID.create 333))

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
        idleWithTwoPullRequests |> applyCommands (fun load save -> startBatch load save ())

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
        idleWithTwoPullRequests |> applyCommands (fun load save -> startBatch load save ())

    let (result, state) =
        runningBatch |> applyCommands (fun load save -> startBatch load save ())

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
        mergingQueue |> applyCommands (fun load save -> startBatch load save ())

    result |> should equal AlreadyRunning

    state |> should equal mergingQueue

[<Fact>]
let ``Attempt to start a batch on an empty queue``() =
    let queue =
        MergeQueue.empty

    let (result, state) =
        queue |> applyCommands (fun load save -> startBatch load save ())

    result |> should equal StartBatchResult.EmptyQueue

    state |> should equal queue

// Batch Build Message ingestion

[<Fact>]
let ``Recieve message that batch successfully builds when batch is running``() =
    let runningQueue = runningBatchOfTwo

    let result, state =
        runningQueue
        |> applyCommands
            (fun load save -> ingestBuildUpdate load save (BuildMessage.Success(SHA.create "12345678")))

    result |> should equal (IngestBuildResult.PerformBatchMerge([ one; two ], (SHA.create "12345678")))

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
        runningQueue |> applyCommands (fun load save -> ingestBuildUpdate load save BuildMessage.Failure)

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
        MergeQueue.empty
        |> applyCommands (fun load save ->
            enqueue load save one |> ignore
            startBatch load save ())
        |> snd

    let result, state =
        runningBatchOfOne |> applyCommands (fun load save -> ingestBuildUpdate load save BuildMessage.Failure)

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
        idleQueue |> applyCommands (fun load save -> ingestBuildUpdate load save BuildMessage.Failure)

    result |> should equal IngestBuildResult.NoOp

    state |> should equal idleQueue

[<Fact>]
let ``Recieve message that build succeeded when no running batch``() =
    let idleQueue = idleWithTwoPullRequests

    let result, state =
        idleQueue
        |> applyCommands
            (fun load save -> ingestBuildUpdate load save (BuildMessage.Success(SHA.create "12345678")))

    result |> should equal IngestBuildResult.NoOp

    state |> should equal idleQueue

[<Fact>]
let ``Recieve message that build failed when batch is being merged``() =
    let mergingQueue = mergingBatchOfTwo

    let (result, state) =
        mergingQueue |> applyCommands (fun load save -> ingestBuildUpdate load save BuildMessage.Failure)

    result |> should equal IngestBuildResult.NoOp

    state |> should equal mergingQueue

[<Fact>]
let ``Recieve message that build succeeded when batch is being merged``() =
    let mergingQueue = mergingBatchOfTwo

    let (result, state) =
        mergingQueue
        |> applyCommands
            (fun load save -> ingestBuildUpdate load save (BuildMessage.Success(SHA.create "12345678")))

    result |> should equal IngestBuildResult.NoOp

    state |> should equal mergingQueue

[<Fact>]
let ``A Pull Request enqueued during running batch is included in the next batch``() =
    let runningQueueDepthThree =
        runningBatchOfTwo
        |> applyCommands (fun load save -> enqueue load save three)
        |> snd

    runningQueueDepthThree
    |> previewExecutionPlan
    |> should equal
           [ PlannedBatch [ one.id; two.id ]
             PlannedBatch [ three.id ] ]

    let finishedQueue =
        runningQueueDepthThree
        |> applyCommands (fun load save ->
            ingestBuildUpdate load save (BuildMessage.Success(SHA.create "12345678")) |> ignore
            ingestMergeUpdate load save MergeMessage.Success)
        |> snd

    finishedQueue
    |> peekCurrentQueue
    |> should equal [ three ]

    let (result, _) =
        finishedQueue |> applyCommands (fun load save -> startBatch load save ())

    result |> should equal (StartBatchResult.PerformBatchBuild [ three ])

// Batch Merge Message ingestion

[<Fact>]
let ``Merge success message when batch is being merged``() =
    let mergingQueue = mergingBatchOfTwo

    let result, state =
        mergingQueue |> applyCommands (fun load save -> ingestMergeUpdate load save MergeMessage.Success)

    result |> should equal (IngestMergeResult.MergeComplete [ one; two ])

    state |> should equal MergeQueue.empty

[<Fact>]
let ``Merge failure message when batch is being merged``() =
    let mergingQueue = mergingBatchOfTwo

    let result, state =
        mergingQueue |> applyCommands (fun load save -> ingestMergeUpdate load save (MergeMessage.Failure))

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
        idle
        |> applyCommands
            (fun load save -> updatePullRequestSha load save (PullRequestID.create 1) (SHA.create "10101010"))

    result |> should equal UpdatePullRequestResult.NoOp

    state
    |> peekCurrentQueue
    |> should equal [ two ]

    // TODO: should the statuses be different? None? our PullRequest technically `passesBuild`
    state
    |> peekSinBin
    |> should equal [ { one with sha = (SHA.create "10101010") } ]

[<Fact>]
let ``The branch head for a running PR is updated``() =
    let running = runningBatchOfTwo

    let result, state =
        running
        |> applyCommands
            (fun load save -> updatePullRequestSha load save (PullRequestID.create 1) (SHA.create "10101010"))

    result |> should equal (UpdatePullRequestResult.AbortRunningBatch([ one; two ], PullRequestID.create 1))

    state
    |> peekCurrentQueue
    |> should equal [ two ]

    // TODO: should the statuses be different? None? our PullRequest technically `passesBuild`
    state
    |> peekSinBin
    |> should equal [ { one with sha = (SHA.create "10101010") } ]

[<Fact>]
let ``The branch head for a unknown PR is updated when batch is running``() =
    let running = runningBatchOfTwo

    let result, state =
        running
        |> applyCommands
            (fun load save -> updatePullRequestSha load save (PullRequestID.create 404) (SHA.create "40400404"))

    result |> should equal UpdatePullRequestResult.NoOp

    state |> should equal running

[<Fact>]
let ``The branch head for an enqueued (but not running) PR is updated when batch is running``() =
    let runningBatchAndOnePrWaiting =
        runningBatchOfTwo
        |> applyCommands (fun load save -> enqueue load save three)
        |> snd

    let result, state =
        runningBatchAndOnePrWaiting
        |> applyCommands
            (fun load save -> updatePullRequestSha load save (PullRequestID.create 333) (SHA.create "30303030"))

    result |> should equal UpdatePullRequestResult.NoOp

    state
    |> peekCurrentBatch
    |> should equal (Some [ one; two ])

    // TODO: should the statuses be different? None? our PullRequest technically `passesBuild`
    state
    |> peekSinBin
    |> should equal [ { three with sha = (SHA.create "30303030") } ]

[<Fact>]
let ``The branch head for an enqueued (but not batched) PR is updated when batch is merging``() =
    let mergingBatchAndOnePrWaiting =
        mergingBatchOfTwo
        |> applyCommands (fun load save -> enqueue load save three)
        |> snd

    let result, state =
        mergingBatchAndOnePrWaiting
        |> applyCommands
            (fun load save -> updatePullRequestSha load save (PullRequestID.create 333) (SHA.create "30303030"))

    result |> should equal UpdatePullRequestResult.NoOp

    state
    |> peekCurrentBatch
    |> should equal (Some [ one; two ])

    // TODO: should the statuses be different? None? our PullRequest technically `passesBuild`
    state
    |> peekSinBin
    |> should equal [ { three with sha = (SHA.create "30303030") } ]

[<Fact>]
let ``The branch head for a batched PR is updated when batch is merging``() =
    let mergingBatchAndOnePrWaiting =
        mergingBatchOfTwo
        |> applyCommands (fun load save -> enqueue load save three)
        |> snd

    let result, state =
        mergingBatchAndOnePrWaiting
        |> applyCommands
            (fun load save -> updatePullRequestSha load save (PullRequestID.create 1) (SHA.create "10101010"))

    let expectedResult = UpdatePullRequestResult.AbortMergingBatch([ one; two ], (PullRequestID.create 1))
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
        |> applyCommands
            (fun load save -> updatePullRequestSha load save (PullRequestID.create 1) (SHA.create "10101010"))
        |> snd

    let state =
        awaitingStatusInfo
        |> applyCommands
            (fun load save ->
            updateStatuses load save (PullRequestID.create 1) (SHA.create "10101010") [ passedCircleCI ])
        |> snd

    state
    |> peekCurrentQueue
    |> should equal
           [ two
             (PullRequest.pullRequest (PullRequestID.create 1) (SHA.create "10101010") [ passedCircleCI ]) ]

[<Fact>]
let ``An updated PR with failing build status is not re-enqueued``() =
    let awaitingStatusInfo =
        idleWithTwoPullRequests
        |> applyCommands
            (fun load save -> updatePullRequestSha load save (PullRequestID.create 1) (SHA.create "10101010"))
        |> snd

    let state =
        awaitingStatusInfo
        |> applyCommands
            (fun load save ->
            updateStatuses load save (PullRequestID.create 1) (SHA.create "10101010") [ failedCircleCI ])
        |> snd

    state
    |> peekCurrentQueue
    |> should equal [ two ]

[<Fact>]
let ``Updates for a PR not in the sin bin is ignored``() =
    let awaitingStatusInfo =
        idleWithTwoPullRequests
        |> applyCommands
            (fun load save -> updatePullRequestSha load save (PullRequestID.create 1) (SHA.create "10101010"))
        |> snd

    let state =
        awaitingStatusInfo
        |> applyCommands
            (fun load save ->
            updateStatuses load save (PullRequestID.create 333) (SHA.create "10101010") [ passedCircleCI ])
        |> snd

    state |> should equal awaitingStatusInfo

[<Fact>]
let ``Old updates for a PR with many SHA updates are ignored``() =
    let awaitingStatusInfo =
        idleWithTwoPullRequests
        |> applyCommands (fun load save ->
            updatePullRequestSha load save (PullRequestID.create 1) (SHA.create "10101010") |> ignore
            updatePullRequestSha load save (PullRequestID.create 1) (SHA.create "11001100"))
        |> snd

    let state =
        awaitingStatusInfo
        |> applyCommands
            (fun load save ->
            updateStatuses load save (PullRequestID.create 1) (SHA.create "10101010") [ passedCircleCI ])
        |> snd

    state
    |> peekCurrentQueue
    |> should equal [ two ]

// Failed batches are bisected

[<Fact>]
let ``Failed batches are bisected upon build failure``() =
    let failedBuildOfFour =
        idleWithTwoPullRequests
        |> applyCommands (fun load save ->
            enqueue load save three |> ignore
            enqueue load save four |> ignore
            startBatch load save () |> ignore
            ingestBuildUpdate load save BuildMessage.Failure)
        |> snd

    // next batch contains only `one` and `two`
    failedBuildOfFour
    |> previewExecutionPlan
    |> should equal
           [ PlannedBatch [ one.id; two.id ]
             PlannedBatch [ three.id; four.id ] ]

    let firstResult, firstState =
        failedBuildOfFour |> applyCommands (fun load save -> startBatch load save ())

    firstResult |> should equal (StartBatchResult.PerformBatchBuild [ one; two ])

    // fail the first bisected batch
    let _, bisectedFails =
        firstState |> applyCommands (fun load save -> ingestBuildUpdate load save BuildMessage.Failure)

    // next batch contains only `one`
    bisectedFails
    |> previewExecutionPlan
    |> should equal
           [ PlannedBatch [ one.id ]
             PlannedBatch [ two.id ]
             PlannedBatch [ three.id; four.id ] ]

    let secondResult, secondState =
        bisectedFails |> applyCommands (fun load save -> startBatch load save ())

    secondResult |> should equal (StartBatchResult.PerformBatchBuild [ one ])

    secondState
    |> peekCurrentBatch
    |> should equal (Some [ one ])

    secondState
    |> peekCurrentQueue
    |> should equal [ two; three; four ]
