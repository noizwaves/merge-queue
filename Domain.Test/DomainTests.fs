module MergeQueue.Domain.Tests

open Xunit
open FsUnit.Xunit
open MergeQueue.Domain
open MergeQueue.DomainTypes
open MergeQueue.DomainServiceTypes
open MergeQueue.DbTypes
open MergeQueue.Workflows
open MergeQueue.Workflows.Enqueue
open MergeQueue.Workflows.UpdateStatuses
open MergeQueue.Workflows.Dequeue
open MergeQueue.Workflows.StartBatch
open MergeQueue.Workflows.IngestBuild
open MergeQueue.Workflows.IngestMerge
open MergeQueue.Workflows.UpdatePullRequest


let private getOrFail result =
    match result with
    | Ok a -> a
    | Error b -> failwithf "Failed because: %s" b

let private makePullRequestID = PullRequestNumber.create >> getOrFail
let private makeSha = SHA.create >> getOrFail
let private makeCommitStatus = CommitStatus.create >> getOrFail

let private passedLinter = makeCommitStatus ("uberlinter", "Success")
let private runningCircleCI = makeCommitStatus ("circleci", "Pending")
let private passedCircleCI = makeCommitStatus ("circleci", "Success")
let private failedCircleCI = makeCommitStatus ("circleci", "Failure")

let private one = PullRequest.create (makePullRequestID 1) (makeSha "00001111") [ passedCircleCI ]

let private oneCmd: Enqueue.Command =
    { number = 1
      sha = "00001111"
      statuses = [ "circleci", "Success" ] }

let private two = PullRequest.create (makePullRequestID 22) (makeSha "00002222") [ passedCircleCI ]

let private twoCmd: Enqueue.Command =
    { number = 22
      sha = "00002222"
      statuses = [ "circleci", "Success" ] }

let private three = PullRequest.create (makePullRequestID 333) (makeSha "00003333") [ passedCircleCI ]

let private threeCmd: Enqueue.Command =
    { number = 333
      sha = "00003333"
      statuses = [ "circleci", "Success" ] }

let private four = PullRequest.create (makePullRequestID 4444) (makeSha "00004444") [ passedCircleCI ]

let private fourCmd: Enqueue.Command =
    { number = 4444
      sha = "00004444"
      statuses = [ "circleci", "Success" ] }

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
        enqueue load save oneCmd |> ignore
        enqueue load save twoCmd)
    |> snd

let private runningBatchOfTwo: MergeQueue =
    idleWithTwoPullRequests
    |> applyCommands (fun load save -> startBatch load save ())
    |> snd

let private mergingBatchOfTwo: MergeQueue =
    runningBatchOfTwo
    |> applyCommands (fun load save ->
        ingestBuildUpdate load save { message = UnvalidatedBuildMessage.Success "12345678" })
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
        MergeQueue.empty |> applyCommands (fun load save -> enqueue load save oneCmd)

    let expected: EnqueueResult = Ok Success.Enqueued
    result |> should equal expected

    state
    |> peekCurrentQueue
    |> should equal [ one ]

    state
    |> peekCurrentBatch
    |> should equal None

    state
    |> previewExecutionPlan
    |> should equal [ PlannedBatch [ one.number ] ]

[<Fact>]
let ``Enqueue a Pull Request with a failing commit status is rejected``() =
    let failingCmd =
        { oneCmd with
              statuses =
                  [ "uberlinter", "Success"
                    "circleci", "Failure" ] }

    let (result, state) =
        MergeQueue.empty |> applyCommands (fun load save -> enqueue load save failingCmd)

    let expected: EnqueueResult = Error(Error.EnqueueError RejectedFailingBuildStatus)
    result |> should equal expected

    state |> should equal MergeQueue.empty

[<Fact>]
let ``Enqueue a Pull Request with a pending commit status is sin binned``() =
    let runningCmd =
        { oneCmd with
              statuses =
                  [ "uberlinter", "Success"
                    "circleci", "Pending" ] }

    let (result, state) =
        MergeQueue.empty |> applyCommands (fun load save -> enqueue load save runningCmd)

    printf "%O" result

    let expected: EnqueueResult = Ok Success.SinBinned
    result |> should equal expected

    state
    |> peekCurrentQueue
    |> should be Empty

    state
    |> peekSinBin
    |> should equal
           [ PullRequest.create (makePullRequestID 1) (makeSha "00001111") [ passedLinter; runningCircleCI ] ]

[<Fact>]
let ``Enqueuing a Pull Request that has no commit statuses is rejected``() =
    let noStatusesCmd = { oneCmd with statuses = [] }
    let (result, state) =
        MergeQueue.empty |> applyCommands (fun load save -> enqueue load save noStatusesCmd)

    let expected: EnqueueResult =
        Error(Error.EnqueueError EnqueueError.RejectedFailingBuildStatus)
    result |> should equal expected

    state |> should equal MergeQueue.empty

[<Fact>]
let ``Enqueue an already enqueued Pull Request``() =
    let singlePrQueueState =
        MergeQueue.empty
        |> applyCommands (fun load save -> enqueue load save oneCmd)
        |> snd

    let duplicateCmd = { oneCmd with sha = "92929292" }
    let (result, state) =
        singlePrQueueState |> applyCommands (fun load save -> enqueue load save duplicateCmd)

    let expected: EnqueueResult =
        Error(Error.EnqueueError EnqueueError.AlreadyEnqueued)
    result |> should equal expected

    state
    |> peekCurrentQueue
    |> should equal [ one ]

    state
    |> previewExecutionPlan
    |> should equal [ PlannedBatch [ one.number ] ]

[<Fact>]
let ``Enqueue a sin binned Pull Request``() =
    let singlePrInSinBin =
        MergeQueue.empty
        |> applyCommands (fun load save ->
            enqueue load save oneCmd |> ignore
            updatePullRequestSha load save
                { number = 1
                  sha = "10101010" }
            |> ignore)
        |> snd

    let result, state =
        singlePrInSinBin
        |> applyCommands (fun load save ->
            enqueue load save
                { number = 1
                  sha = "92929292"
                  statuses = [ "circleci", "Success" ] })

    let expected: EnqueueResult =
        Error(Error.EnqueueError EnqueueError.AlreadyEnqueued)
    result |> should equal expected

    state |> should equal singlePrInSinBin

[<Fact>]
let ``Enqueue multiple Pull Requests``() =
    let firstResult, firstState =
        MergeQueue.empty |> applyCommands (fun load save -> enqueue load save oneCmd)

    let secondResult, secondState =
        firstState |> applyCommands (fun load save -> enqueue load save twoCmd)

    let expected: EnqueueResult = Ok Success.Enqueued
    firstResult |> should equal expected

    secondResult |> should equal expected

    secondState
    |> peekCurrentQueue
    |> should equal [ one; two ]

    secondState
    |> previewExecutionPlan
    |> should equal [ PlannedBatch [ one.number; two.number ] ]

// Dequeue a Pull Request
[<Fact>]
let ``Dequeue an enqueued Pull Request``() =
    let result, state =
        idleWithTwoPullRequests |> applyCommands (fun load save -> dequeue load save { number = 1 })

    let expected: DequeueResult = Ok Success.Dequeued
    result |> should equal expected

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
        |> applyCommands (fun load save ->
            updatePullRequestSha load save
                { number = 1
                  sha = "10101010" })
        |> snd

    let result, state =
        idleWithOneEnqueuedOneSinBinned |> applyCommands (fun load save -> dequeue load save { number = 1 })

    let expected: DequeueResult = Ok Success.Dequeued
    result |> should equal (expected)

    state
    |> peekCurrentQueue
    |> should equal [ two ]

    state
    |> peekSinBin
    |> should be Empty

[<Fact>]
let ``Dequeue an unknown Pull Request``() =
    let result, state =
        idleWithTwoPullRequests |> applyCommands (fun load save -> dequeue load save { number = 404 })

    let expected: DequeueResult = Error(Error.DequeueError DequeueError.NotFound)
    result |> should equal expected

    state
    |> peekCurrentQueue
    |> should equal [ one; two ]

    state
    |> peekSinBin
    |> should be Empty

[<Fact>]
let ``Dequeue a Pull Request that is in a running batch``() =
    let result, state =
        runningBatchOfTwo |> applyCommands (fun load save -> dequeue load save { number = 1 })

    let expected: DequeueResult = Ok(Success.DequeuedAndAbortRunningBatch([ one; two ], (makePullRequestID 1)))
    result |> should equal expected

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
            enqueue load save threeCmd |> ignore
            dequeue load save { number = 333 })

    let expected: DequeueResult = Ok Success.Dequeued
    result |> should equal expected

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
        mergingBatchOfTwo |> applyCommands (fun load save -> dequeue load save { number = 1 })

    let expected: DequeueResult = Error(Error.DequeueError DequeueError.RejectedInMergingBatch)
    result |> should equal expected

    state |> should equal mergingBatchOfTwo

[<Fact>]
let ``Dequeue a Pull Request that is waiting behind a merging batch``() =
    let result, state =
        mergingBatchOfTwo
        |> applyCommands (fun load save ->
            enqueue load save threeCmd |> ignore
            dequeue load save { number = 333 })

    let expected: DequeueResult = Ok Success.Dequeued
    result |> should equal expected

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

    let expected: StartBatchResult = Ok(Success.PerformBatchBuild [ one; two ])
    result |> should equal expected

    state
    |> peekCurrentQueue
    |> should be Empty

    state
    |> peekCurrentBatch
    |> should equal (Some [ one; two ])

    state
    |> previewExecutionPlan
    |> should equal [ PlannedBatch [ one.number; two.number ] ]

[<Fact>]
let ``Attempt to start a second concurrent batch``() =
    let (_, runningBatch) =
        idleWithTwoPullRequests |> applyCommands (fun load save -> startBatch load save ())

    let (result, state) =
        runningBatch |> applyCommands (fun load save -> startBatch load save ())

    let expected: StartBatchResult = Error(StartBatchError AlreadyRunning)
    result |> should equal expected

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

    let expected: StartBatchResult = Error(StartBatchError AlreadyRunning)
    result |> should equal expected

    state |> should equal mergingQueue

[<Fact>]
let ``Attempt to start a batch on an empty queue``() =
    let queue =
        MergeQueue.empty

    let (result, state) =
        queue |> applyCommands (fun load save -> startBatch load save ())

    let expected: StartBatchResult = Error(StartBatchError EmptyQueue)
    result |> should equal expected

    state |> should equal queue

// Batch Build Message ingestion

[<Fact>]
let ``Recieve message that batch successfully builds when batch is running``() =
    let runningQueue = runningBatchOfTwo

    let result, state =
        runningQueue
        |> applyCommands (fun load save ->
            ingestBuildUpdate load save { message = UnvalidatedBuildMessage.Success "12345678" })

    let expected: IngestBuildResult = Ok(PerformBatchMerge([ one; two ], (makeSha "12345678")))
    result |> should equal expected

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
        runningQueue
        |> applyCommands (fun load save -> ingestBuildUpdate load save { message = UnvalidatedBuildMessage.Failure })

    let expected: IngestBuildResult = Ok(ReportBuildFailureWithRetry [ one; two ])
    result |> should equal expected

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
            enqueue load save oneCmd |> ignore
            startBatch load save ())
        |> snd

    let result, state =
        runningBatchOfOne
        |> applyCommands (fun load save -> ingestBuildUpdate load save { message = UnvalidatedBuildMessage.Failure })

    let expected: IngestBuildResult = Ok(ReportBuildFailureNoRetry [ one ])
    result |> should equal expected

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
        idleQueue
        |> applyCommands (fun load save -> ingestBuildUpdate load save { message = UnvalidatedBuildMessage.Failure })

    let expected: IngestBuildResult = Error(IngestBuildError NotCurrentlyBuilding)
    result |> should equal expected

    state |> should equal idleQueue

[<Fact>]
let ``Recieve message that build succeeded when no running batch``() =
    let idleQueue = idleWithTwoPullRequests

    let result, state =
        idleQueue
        |> applyCommands (fun load save ->
            ingestBuildUpdate load save { message = UnvalidatedBuildMessage.Success "12345678" })

    let expected: IngestBuildResult = Error(IngestBuildError NotCurrentlyBuilding)
    result |> should equal expected

    state |> should equal idleQueue

[<Fact>]
let ``Recieve message that build failed when batch is being merged``() =
    let mergingQueue = mergingBatchOfTwo

    let (result, state) =
        mergingQueue
        |> applyCommands (fun load save -> ingestBuildUpdate load save { message = UnvalidatedBuildMessage.Failure })

    let expected: IngestBuildResult = Error(IngestBuildError NotCurrentlyBuilding)
    result |> should equal expected

    state |> should equal mergingQueue

[<Fact>]
let ``Recieve message that build succeeded when batch is being merged``() =
    let mergingQueue = mergingBatchOfTwo

    let (result, state) =
        mergingQueue
        |> applyCommands (fun load save ->
            ingestBuildUpdate load save { message = UnvalidatedBuildMessage.Success "12345678" })

    let expected: IngestBuildResult = Error(IngestBuildError NotCurrentlyBuilding)
    result |> should equal expected

    state |> should equal mergingQueue

[<Fact>]
let ``A Pull Request enqueued during running batch is included in the next batch``() =
    let runningQueueDepthThree =
        runningBatchOfTwo
        |> applyCommands (fun load save -> enqueue load save threeCmd)
        |> snd

    runningQueueDepthThree
    |> previewExecutionPlan
    |> should equal
           [ PlannedBatch [ one.number; two.number ]
             PlannedBatch [ three.number ] ]

    let finishedQueue =
        runningQueueDepthThree
        |> applyCommands (fun load save ->
            ingestBuildUpdate load save { message = UnvalidatedBuildMessage.Success "12345678" } |> ignore
            ingestMergeUpdate load save { message = UnvalidatedMergeMessage.Success })
        |> snd

    finishedQueue
    |> peekCurrentQueue
    |> should equal [ three ]

    let (result, _) =
        finishedQueue |> applyCommands (fun load save -> startBatch load save ())

    let expected: StartBatchResult = Ok(Success.PerformBatchBuild [ three ])
    result |> should equal expected

// Batch Merge Message ingestion

[<Fact>]
let ``Merge success message when batch is being merged``() =
    let mergingQueue = mergingBatchOfTwo

    let result, state =
        mergingQueue
        |> applyCommands (fun load save -> ingestMergeUpdate load save { message = UnvalidatedMergeMessage.Success })

    let expected: IngestMergeResult = Ok(MergeComplete [ one; two ])
    result |> should equal expected

    state |> should equal MergeQueue.empty

[<Fact>]
let ``Merge failure message when batch is being merged``() =
    let mergingQueue = mergingBatchOfTwo

    let result, state =
        mergingQueue
        |> applyCommands (fun load save -> ingestMergeUpdate load save { message = UnvalidatedMergeMessage.Failure })

    state
    |> peekCurrentQueue
    |> should equal [ one; two ]

    let expected: IngestMergeResult = Ok(ReportMergeFailure [ one; two ])
    result |> should equal expected

// PRs are updated

// updated PRs should be removed from the queue until their builds pass again
// at which point they are added to the end of the queue again

[<Fact>]
let ``The branch head for an enqueued PR is updated``() =
    let idle = idleWithTwoPullRequests

    let result, state =
        idle
        |> applyCommands (fun load save ->
            updatePullRequestSha load save
                { number = 1
                  sha = "10101010" })

    let expected: UpdatePullRequestResult = Ok UpdatePullRequestSuccess.NoChange
    result |> should equal expected

    state
    |> peekCurrentQueue
    |> should equal [ two ]

    // TODO: should the statuses be different? None? our PullRequest technically `passesBuild`
    state
    |> peekSinBin
    |> should equal [ { one with sha = (makeSha "10101010") } ]

[<Fact>]
let ``The branch head for a running PR is updated``() =
    let running = runningBatchOfTwo

    let result, state =
        running
        |> applyCommands (fun load save ->
            updatePullRequestSha load save
                { number = 1
                  sha = "10101010" })

    let expected: UpdatePullRequestResult = Ok(Success.AbortRunningBatch([ one; two ], makePullRequestID 1))
    result |> should equal expected

    state
    |> peekCurrentQueue
    |> should equal [ two ]

    // TODO: should the statuses be different? None? our PullRequest technically `passesBuild`
    state
    |> peekSinBin
    |> should equal [ { one with sha = (makeSha "10101010") } ]

[<Fact>]
let ``The branch head for a unknown PR is updated when batch is running``() =
    let running = runningBatchOfTwo

    let result, state =
        running
        |> applyCommands (fun load save ->
            updatePullRequestSha load save
                { number = 404
                  sha = "40400404" })

    let expected: UpdatePullRequestResult = Ok UpdatePullRequestSuccess.NoChange
    result |> should equal expected

    state |> should equal running

[<Fact>]
let ``The branch head for an enqueued (but not running) PR is updated when batch is running``() =
    let runningBatchAndOnePrWaiting =
        runningBatchOfTwo
        |> applyCommands (fun load save -> enqueue load save threeCmd)
        |> snd

    let result, state =
        runningBatchAndOnePrWaiting
        |> applyCommands (fun load save ->
            updatePullRequestSha load save
                { number = 333
                  sha = "30303030" })

    let expected: UpdatePullRequestResult = Ok UpdatePullRequestSuccess.NoChange
    result |> should equal expected

    state
    |> peekCurrentBatch
    |> should equal (Some [ one; two ])

    // TODO: should the statuses be different? None? our PullRequest technically `passesBuild`
    state
    |> peekSinBin
    |> should equal [ { three with sha = (makeSha "30303030") } ]

[<Fact>]
let ``The branch head for an enqueued (but not batched) PR is updated when batch is merging``() =
    let mergingBatchAndOnePrWaiting =
        mergingBatchOfTwo
        |> applyCommands (fun load save -> enqueue load save threeCmd)
        |> snd

    let result, state =
        mergingBatchAndOnePrWaiting
        |> applyCommands (fun load save ->
            updatePullRequestSha load save
                { number = 333
                  sha = "30303030" })

    let expected: UpdatePullRequestResult = Ok UpdatePullRequestSuccess.NoChange
    result |> should equal expected

    state
    |> peekCurrentBatch
    |> should equal (Some [ one; two ])

    // TODO: should the statuses be different? None? our PullRequest technically `passesBuild`
    state
    |> peekSinBin
    |> should equal [ { three with sha = (makeSha "30303030") } ]

[<Fact>]
let ``The branch head for a batched PR is updated when batch is merging``() =
    let mergingBatchAndOnePrWaiting =
        mergingBatchOfTwo
        |> applyCommands (fun load save -> enqueue load save threeCmd)
        |> snd

    let result, state =
        mergingBatchAndOnePrWaiting
        |> applyCommands (fun load save ->
            updatePullRequestSha load save
                { number = 1
                  sha = "10101010" })

    let expectedResult: UpdatePullRequestResult = Ok(AbortMergingBatch([ one; two ], (makePullRequestID 1)))
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
        |> applyCommands (fun load save ->
            updatePullRequestSha load save
                { number = 1
                  sha = "10101010" })
        |> snd

    let state =
        awaitingStatusInfo
        |> applyCommands (fun load save ->
            updateStatuses load save
                { number = 1
                  sha = "10101010"
                  statuses = [ "circleci", "Success" ] })
        |> snd

    state
    |> peekCurrentQueue
    |> should equal
           [ two
             (PullRequest.create (makePullRequestID 1) (makeSha "10101010") [ passedCircleCI ]) ]

[<Fact>]
let ``An updated PR with failing build status is not re-enqueued``() =
    let awaitingStatusInfo =
        idleWithTwoPullRequests
        |> applyCommands (fun load save ->
            updatePullRequestSha load save
                { number = 1
                  sha = "10101010" })
        |> snd

    let state =
        awaitingStatusInfo
        |> applyCommands (fun load save ->
            updateStatuses load save
                { number = 1
                  sha = "10101010"
                  statuses = [ "circleci", "Failure" ] })
        |> snd

    state
    |> peekCurrentQueue
    |> should equal [ two ]

[<Fact>]
let ``Updates for a PR not in the sin bin is ignored``() =
    let awaitingStatusInfo =
        idleWithTwoPullRequests
        |> applyCommands (fun load save ->
            updatePullRequestSha load save
                { number = 1
                  sha = "10101010" })
        |> snd

    let state =
        awaitingStatusInfo
        |> applyCommands (fun load save ->
            updateStatuses load save
                { number = 333
                  sha = "10101010"
                  statuses = [ "circleci", "Success" ] })
        |> snd

    state |> should equal awaitingStatusInfo

[<Fact>]
let ``Old updates for a PR with many SHA updates are ignored``() =
    let awaitingStatusInfo =
        idleWithTwoPullRequests
        |> applyCommands (fun load save ->
            updatePullRequestSha load save
                { number = 1
                  sha = "10101010" }
            |> ignore
            updatePullRequestSha load save
                { number = 1
                  sha = "11001100" })
        |> snd

    let state =
        awaitingStatusInfo
        |> applyCommands (fun load save ->
            updateStatuses load save
                { number = 1
                  sha = "10101010"
                  statuses = [ "circleci", "Success" ] })
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
            enqueue load save threeCmd |> ignore
            enqueue load save fourCmd |> ignore
            startBatch load save () |> ignore
            ingestBuildUpdate load save { message = UnvalidatedBuildMessage.Failure })
        |> snd

    // next batch contains only `one` and `two`
    failedBuildOfFour
    |> previewExecutionPlan
    |> should equal
           [ PlannedBatch [ one.number; two.number ]
             PlannedBatch [ three.number; four.number ] ]

    let firstResult, firstState =
        failedBuildOfFour |> applyCommands (fun load save -> startBatch load save ())

    let expected: StartBatchResult = Ok(Success.PerformBatchBuild [ one; two ])
    firstResult |> should equal expected

    // fail the first bisected batch
    let _, bisectedFails =
        firstState
        |> applyCommands (fun load save -> ingestBuildUpdate load save { message = UnvalidatedBuildMessage.Failure })

    // next batch contains only `one`
    bisectedFails
    |> previewExecutionPlan
    |> should equal
           [ PlannedBatch [ one.number ]
             PlannedBatch [ two.number ]
             PlannedBatch [ three.number; four.number ] ]

    let secondResult, secondState =
        bisectedFails |> applyCommands (fun load save -> startBatch load save ())

    let expected: StartBatchResult = Ok(Success.PerformBatchBuild [ one ])
    secondResult |> should equal expected

    secondState
    |> peekCurrentBatch
    |> should equal (Some [ one ])

    secondState
    |> peekCurrentQueue
    |> should equal [ two; three; four ]
