module MergeQueue.Domain.IntegrationTests

open Xunit
open FsUnit.Xunit
open MergeQueue.Domain
open MergeQueue.DomainTypes
open MergeQueue.GitHubTypes
open MergeQueue.Workflows
open MergeQueue.Workflows.Enqueue
open MergeQueue.Workflows.UpdateStatus
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

let private passedCircleCI = makeCommitStatus ("circleci", "Success")
let private pendingCircleCI = makeCommitStatus ("circleci", "Pending")
let private failedCircleCI = makeCommitStatus ("circleci", "Failure")

let private one = PullRequest.create (makePullRequestID 1111) (makeSha "00001111") [ passedCircleCI ]

let private oneCmd: Enqueue.Command =
    { number = 1111
      repoOwner = "some-owner"
      repoName = "some-name" }

let private oneLookup: LookUpPullRequestDetails =
    fun _ ->
        async {
            return Ok
                       { sha = "00001111"
                         statuses = [ "circleci", State.Success ] }
        }

let private two = PullRequest.create (makePullRequestID 2222) (makeSha "00002222") [ passedCircleCI ]

let private twoCmd: Enqueue.Command =
    { number = 2222
      repoOwner = "some-owner"
      repoName = "some-name" }

let private twoLookup: LookUpPullRequestDetails =
    fun _ ->
        async {
            return Ok
                       { sha = "00002222"
                         statuses = [ "circleci", State.Success ] }
        }

let private three = PullRequest.create (makePullRequestID 3333) (makeSha "00003333") [ passedCircleCI ]

let private threeCmd: Enqueue.Command =
    { number = 3333
      repoOwner = "some-owner"
      repoName = "some-name" }

let private threeLookup: LookUpPullRequestDetails =
    fun _ ->
        async {
            return Ok
                       { sha = "00003333"
                         statuses = [ "circleci", State.Success ] }
        }

let private four = PullRequest.create (makePullRequestID 4444) (makeSha "00004444") [ passedCircleCI ]

let private fourCmd: Enqueue.Command =
    { number = 4444
      repoOwner = "some-owner"
      repoName = "some-name" }

let private fourLookup: LookUpPullRequestDetails =
    fun _ ->
        async {
            return Ok
                       { sha = "00004444"
                         statuses = [ "circleci", State.Success ] }
        }

let private five = PullRequest.create (makePullRequestID 5555) (makeSha "00005555") [ pendingCircleCI ]

let private fiveCmd: Enqueue.Command =
    { number = 5555
      repoOwner = "some-owner"
      repoName = "some-name" }

let private fiveLookup: LookUpPullRequestDetails =
    fun _ ->
        async {
            return Ok
                       { sha = "00005555"
                         statuses = [ "circleci", State.Pending ] }
        }

let private six = PullRequest.create (makePullRequestID 6666) (makeSha "00006666") [ passedCircleCI ]

let private sixCmd: Enqueue.Command =
    { number = 6666
      repoOwner = "some-owner"
      repoName = "some-name" }

let private sixLookup: LookUpPullRequestDetails =
    fun _ ->
        async {
            return Ok
                       { sha = "00006666"
                         statuses = [ "circleci", State.Success ] }
        }

let private seven = PullRequest.create (makePullRequestID 7777) (makeSha "00007777") [ passedCircleCI ]

let private sevenCmd: Enqueue.Command =
    { number = 7777
      repoOwner = "some-owner"
      repoName = "some-name" }

let private sevenLookup: LookUpPullRequestDetails =
    fun _ ->
        async {
            return Ok
                       { sha = "00007777"
                         statuses = [ "circleci", State.Success ] }
        }

let private eight = PullRequest.create (makePullRequestID 8888) (makeSha "00008888") [ passedCircleCI ]

let private eightCmd: Enqueue.Command =
    { number = 8888
      repoOwner = "some-owner"
      repoName = "some-name" }

let private eightLookup: LookUpPullRequestDetails =
    fun _ ->
        async {
            return Ok
                       { sha = "00008888"
                         statuses = [ "circleci", State.Success ] }
        }


[<Fact>]
let ``Realistic workflow``() =
    let mutable state = MergeQueue.empty
    let update v = state <- v
    let fetch() = state

    // small bit of DI
    let enqueue' = enqueue fetch update
    let startBatch' = startBatch fetch update
    let updatePullRequestSha' = updatePullRequestSha fetch update
    let updateStatus' = updateStatus fetch update
    let ingestBuildUpdate' = ingestBuildUpdate fetch update
    let ingestMergeUpdate' = ingestMergeUpdate fetch update
    let dequeue' = dequeue fetch update

    // 1. Four enqueued but not started
    enqueue' oneLookup oneCmd |> ignore
    enqueue' twoLookup twoCmd |> ignore
    enqueue' threeLookup threeCmd |> ignore
    enqueue' fourLookup fourCmd |> ignore
    let ``Four enqueued but not started`` = fetch()

    ``Four enqueued but not started``
    |> peekCurrentQueue
    |> should equal [ one; two; three; four ]

    ``Four enqueued but not started``
    |> peekCurrentBatch
    |> should equal None

    ``Four enqueued but not started``
    |> peekSinBin
    |> should be Empty

    ``Four enqueued but not started``
    |> previewExecutionPlan
    |> should equal [ PlannedBatch [ one.number; two.number; three.number; four.number ] ]

    // 2. First batch running some additional enqueued
    startBatch'() |> ignore
    enqueue' fiveLookup fiveCmd |> ignore
    enqueue' sixLookup sixCmd |> ignore
    let ``First batch running some additional enqueued`` = fetch()

    ``First batch running some additional enqueued``
    |> peekCurrentQueue
    |> should equal [ six ]

    ``First batch running some additional enqueued``
    |> peekCurrentBatch
    |> should equal (Some [ one; two; three; four ])

    ``First batch running some additional enqueued``
    |> peekSinBin
    |> should equal [ five ]

    ``First batch running some additional enqueued``
    |> previewExecutionPlan
    |> should equal
           [ PlannedBatch [ one.number; two.number; three.number; four.number ]
             PlannedBatch [ six.number ] ]

    // 3. Five fails to build, Six's branch is updated, Seven is enqueued, batch continues to build
    updateStatus'
        { number = 5555
          sha = "00005555"
          status = "circleci", "Failure" }
    |> ignore
    updatePullRequestSha'
        { number = 6666
          sha = "60606060" }
    |> ignore
    enqueue' sevenLookup sevenCmd |> ignore
    let ``Five fails to build, Six's branch is updated, batch continues to build`` = fetch()

    let six_v2 = PullRequest.create (makePullRequestID 6666) (makeSha "60606060") []
    let five_v2 = PullRequest.create (makePullRequestID 5555) (makeSha "00005555") [ failedCircleCI ]

    ``Five fails to build, Six's branch is updated, batch continues to build``
    |> peekCurrentQueue
    |> should equal [ seven ]

    ``Five fails to build, Six's branch is updated, batch continues to build``
    |> peekCurrentBatch
    |> should equal (Some [ one; two; three; four ])

    ``Five fails to build, Six's branch is updated, batch continues to build``
    |> peekSinBin
    |> should equal [ five_v2; six_v2 ]

    ``Five fails to build, Six's branch is updated, batch continues to build``
    |> previewExecutionPlan
    |> should equal
           [ PlannedBatch [ one.number; two.number; three.number; four.number ]
             PlannedBatch [ seven.number ] ]

    // 4. Five's branch is updated, Batch fails to build, Six's build passes
    updatePullRequestSha'
        { number = 5555
          sha = "50505050" }
    |> ignore
    ingestBuildUpdate' { message = UnvalidatedBuildMessage.Failure } |> ignore
    updateStatus'
        { number = 6666
          sha = "60606060"
          status = "circleci", "Success" }
    |> ignore
    let ``Five's branch is updated, Batch fails to build, Six's build passes`` = fetch()

    let six_v3 = PullRequest.create (makePullRequestID 6666) (makeSha "60606060") [ passedCircleCI ]
    let five_v3 = PullRequest.create (makePullRequestID 5555) (makeSha "50505050") []

    ``Five's branch is updated, Batch fails to build, Six's build passes``
    |> peekCurrentQueue
    |> should equal [ one; two; three; four; seven; six_v3 ]

    ``Five's branch is updated, Batch fails to build, Six's build passes``
    |> peekCurrentBatch
    |> should equal None

    ``Five's branch is updated, Batch fails to build, Six's build passes``
    |> peekSinBin
    |> should equal [ five_v3 ]

    ``Five's branch is updated, Batch fails to build, Six's build passes``
    |> previewExecutionPlan
    |> should equal
           [ PlannedBatch [ one.number; two.number ]
             PlannedBatch [ three.number; four.number ]
             PlannedBatch [ seven.number; six.number ] ]

    // 5. Start another batch, Eight is enqueued, Five's build fails again
    startBatch'() |> ignore
    enqueue' eightLookup eightCmd |> ignore
    updateStatus'
        { number = 5555
          sha = "50505050"
          status = "circleci", "Failure" }
    |> ignore
    let ``Start another batch, Eight is enqueued, Five's build fails again`` = fetch()

    let five_v4 = PullRequest.create (makePullRequestID 5555) (makeSha "50505050") [ failedCircleCI ]

    ``Start another batch, Eight is enqueued, Five's build fails again``
    |> peekCurrentQueue
    |> should equal [ three; four; seven; six_v3; eight ]

    ``Start another batch, Eight is enqueued, Five's build fails again``
    |> peekCurrentBatch
    |> should equal (Some [ one; two ])

    ``Start another batch, Eight is enqueued, Five's build fails again``
    |> peekSinBin
    |> should equal [ five_v4 ]

    ``Start another batch, Eight is enqueued, Five's build fails again``
    |> previewExecutionPlan
    |> should equal
           [ PlannedBatch [ one.number; two.number ]
             PlannedBatch [ three.number; four.number ]
             PlannedBatch [ seven.number; six.number; eight.number ] ]

    // 6. Five is dequeued, Three is dequeued, The batch builds and merges successfully
    dequeue'
        { number = 5555
          repoOwner = "some-owner"
          repoName = "some-name" }
    |> ignore
    dequeue'
        { number = 3333
          repoOwner = "some-owner"
          repoName = "some-name" }
    |> ignore
    ingestBuildUpdate' { message = (UnvalidatedBuildMessage.Success "12000000") } |> ignore
    ingestMergeUpdate' { message = UnvalidatedMergeMessage.Success } |> ignore
    let ``Five is dequeued, Three is dequeued, The batch builds and merges successfully`` = fetch()

    ``Five is dequeued, Three is dequeued, The batch builds and merges successfully``
    |> peekCurrentQueue
    |> should equal [ four; seven; six_v3; eight ]

    ``Five is dequeued, Three is dequeued, The batch builds and merges successfully``
    |> peekCurrentBatch
    |> should equal None

    ``Five is dequeued, Three is dequeued, The batch builds and merges successfully``
    |> peekSinBin
    |> should be Empty

    ``Five is dequeued, Three is dequeued, The batch builds and merges successfully``
    |> previewExecutionPlan
    |> should equal
           [ PlannedBatch [ four.number ]
             PlannedBatch [ seven.number; six.number; eight.number ] ]

    // 7. Start a batch and it fails
    startBatch'() |> ignore
    ingestBuildUpdate' { message = UnvalidatedBuildMessage.Failure } |> ignore
    let ``Start a batch and it fails`` = fetch()

    ``Start a batch and it fails``
    |> peekCurrentQueue
    |> should equal [ seven; six_v3; eight ]

    ``Start a batch and it fails``
    |> peekCurrentBatch
    |> should equal None

    ``Start a batch and it fails``
    |> peekSinBin
    |> should be Empty

    ``Start a batch and it fails``
    |> previewExecutionPlan
    |> should equal [ PlannedBatch [ seven.number; six.number; eight.number ] ]

    // 8. Start another batch, Six's branch is updated during the build causing an abort
    startBatch'() |> ignore
    updatePullRequestSha'
        { number = 6666
          sha = "66006600" }
    |> ignore
    let ``Start another batch, Six's branch is updated during the build causing an abort`` = fetch()

    let six_v4 = PullRequest.create (makePullRequestID 6666) (makeSha "66006600") []

    ``Start another batch, Six's branch is updated during the build causing an abort``
    |> peekCurrentQueue
    |> should equal [ seven; eight ]

    ``Start another batch, Six's branch is updated during the build causing an abort``
    |> peekCurrentBatch
    |> should equal None

    ``Start another batch, Six's branch is updated during the build causing an abort``
    |> peekSinBin
    |> should equal [ six_v4 ]

    ``Start another batch, Six's branch is updated during the build causing an abort``
    |> previewExecutionPlan
    |> should equal [ PlannedBatch [ seven.number; eight.number ] ]

    // 9. Six's build starts then passes, start a batch
    updateStatus'
        { number = 6666
          sha = "66006600"
          status = "circleci", "Pending" }
    |> ignore
    updateStatus'
        { number = 6666
          sha = "66006600"
          status = "circleci", "Success" }
    |> ignore
    startBatch'() |> ignore
    let ``Six's build starts then passes, start a batch`` = fetch()

    let six_v5 = PullRequest.create (makePullRequestID 6666) (makeSha "66006600") [ passedCircleCI ]

    ``Six's build starts then passes, start a batch``
    |> peekCurrentQueue
    |> should be Empty

    ``Six's build starts then passes, start a batch``
    |> peekCurrentBatch
    |> should equal (Some [ seven; eight; six_v5 ])

    ``Six's build starts then passes, start a batch``
    |> peekSinBin
    |> should be Empty

    ``Six's build starts then passes, start a batch``
    |> previewExecutionPlan
    |> should equal [ PlannedBatch [ seven.number; eight.number; six.number ] ]

    // 10. Batch builds and merges successfully
    ingestBuildUpdate' { message = UnvalidatedBuildMessage.Success "76800000" } |> ignore
    ingestMergeUpdate' { message = UnvalidatedMergeMessage.Success } |> ignore
    let ``Batch builds and merges successfully`` = fetch()

    ``Batch builds and merges successfully`` |> should equal MergeQueue.empty
