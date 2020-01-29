module MergeQueue.Domain.IntegrationTests

open Xunit
open FsUnit.Xunit
open MergeQueue.Domain
open MergeQueue.DomainTypes
open MergeQueue.Commands

let private passedCircleCI = CommitStatus.create "circleci" CommitStatusState.Success
let private pendingCircleCI = CommitStatus.create "circleci" CommitStatusState.Pending
let private failedCircleCI = CommitStatus.create "circleci" CommitStatusState.Failure

let private one = PullRequest.pullRequest (PullRequestID.create 1111) (SHA.create "00001111") [ passedCircleCI ]
let private oneCmd = { number= 1111; sha = "00001111"; statuses = [ "circleci", "Success" ] }
let private two = PullRequest.pullRequest (PullRequestID.create 2222) (SHA.create "00002222") [ passedCircleCI ]
let private twoCmd = { number= 2222; sha = "00002222"; statuses = [ "circleci", "Success" ] }
let private three = PullRequest.pullRequest (PullRequestID.create 3333) (SHA.create "00003333") [ passedCircleCI ]
let private threeCmd = { number= 3333; sha = "00003333"; statuses = [ "circleci", "Success" ] }
let private four = PullRequest.pullRequest (PullRequestID.create 4444) (SHA.create "00004444") [ passedCircleCI ]
let private fourCmd = { number= 4444; sha = "00004444"; statuses = [ "circleci", "Success" ] }
let private five = PullRequest.pullRequest (PullRequestID.create 5555) (SHA.create "00005555") [ pendingCircleCI ]
let private fiveCmd = { number= 5555; sha = "00005555"; statuses = [ "circleci", "Pending" ] }
let private six = PullRequest.pullRequest (PullRequestID.create 6666) (SHA.create "00006666") [ passedCircleCI ]
let private sixCmd = { number= 6666; sha = "00006666"; statuses = [ "circleci", "Success" ] }
let private seven = PullRequest.pullRequest (PullRequestID.create 7777) (SHA.create "00007777") [ passedCircleCI ]
let private sevenCmd = { number= 7777; sha = "00007777"; statuses = [ "circleci", "Success" ] }
let private eight = PullRequest.pullRequest (PullRequestID.create 8888) (SHA.create "00008888") [ passedCircleCI ]
let private eightCmd = { number= 8888; sha = "00008888"; statuses = [ "circleci", "Success" ] }


[<Fact>]
let ``Realistic workflow``() =
    let mutable state = MergeQueue.empty
    let update v = state <- v
    let fetch () = state
    
    // small bit of DI
    let enqueue' = enqueue fetch update
    let startBatch' = startBatch fetch update
    let updatePullRequestSha' = updatePullRequestSha fetch update
    let updateStatuses' = updateStatuses fetch update
    let ingestBuildUpdate' = ingestBuildUpdate fetch update
    let ingestMergeUpdate' = ingestMergeUpdate fetch update
    let dequeue' = dequeue fetch update
    
    // 1. Four enqueued but not started
    enqueue' oneCmd |> ignore
    enqueue' twoCmd |> ignore
    enqueue' threeCmd |> ignore
    enqueue' fourCmd |> ignore
    let ``Four enqueued but not started`` = fetch ()

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
    |> should equal [ PlannedBatch [ one.id; two.id; three.id; four.id ] ]

    // 2. First batch running some additional enqueued
    startBatch' () |> ignore
    enqueue' fiveCmd |> ignore
    enqueue' sixCmd |> ignore
    let ``First batch running some additional enqueued`` = fetch ()

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
           [ PlannedBatch [ one.id; two.id; three.id; four.id ]
             PlannedBatch [ six.id ] ]

    // 3. Five fails to build, Six's branch is updated, Seven is enqueued, batch continues to build
    updateStatuses' (PullRequestID.create 5555) (SHA.create "00005555") [ failedCircleCI ] |> ignore
    updatePullRequestSha' (PullRequestID.create 6666) (SHA.create "60606060") |> ignore
    enqueue' sevenCmd |> ignore
    let ``Five fails to build, Six's branch is updated, batch continues to build`` = fetch ()

    let six_v2 = PullRequest.pullRequest (PullRequestID.create 6666) (SHA.create "60606060") [ passedCircleCI ]
    let five_v2 = PullRequest.pullRequest (PullRequestID.create 5555) (SHA.create "00005555") [ failedCircleCI ]

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
           [ PlannedBatch [ one.id; two.id; three.id; four.id ]
             PlannedBatch [ seven.id ] ]

    // 4. Five's branch is updated, Batch fails to build, Six's build passes
    updatePullRequestSha' (PullRequestID.create 5555) (SHA.create "50505050") |> ignore
    ingestBuildUpdate' BuildMessage.Failure |> ignore
    updateStatuses' (PullRequestID.create 6666) (SHA.create "60606060") [ passedCircleCI ] |> ignore
    let ``Five's branch is updated, Batch fails to build, Six's build passes`` = fetch()

    let six_v3 = PullRequest.pullRequest (PullRequestID.create 6666) (SHA.create "60606060") [ passedCircleCI ]
    let five_v3 = PullRequest.pullRequest (PullRequestID.create 5555) (SHA.create "50505050") [ failedCircleCI ]

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
           [ PlannedBatch [ one.id; two.id ]
             PlannedBatch [ three.id; four.id ]
             PlannedBatch [ seven.id; six.id ] ]

    // 5. Start another batch, Eight is enqueued, Five's build fails again
    startBatch' ()  |> ignore
    enqueue' eightCmd  |> ignore
    updateStatuses' (PullRequestID.create 5555) (SHA.create "50505050") [ failedCircleCI ]  |> ignore
    let ``Start another batch, Eight is enqueued, Five's build fails again`` = fetch()

    let five_v4 = PullRequest.pullRequest (PullRequestID.create 5555) (SHA.create "50505050") [ failedCircleCI ]

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
           [ PlannedBatch [ one.id; two.id ]
             PlannedBatch [ three.id; four.id ]
             PlannedBatch [ seven.id; six.id; eight.id ] ]

    // 6. Five is dequeued, Three is dequeued, The batch builds and merges successfully
    dequeue' (PullRequestID.create 5555) |> ignore
    dequeue' (PullRequestID.create 3333) |> ignore
    ingestBuildUpdate' (BuildMessage.Success(SHA.create "12000000")) |> ignore
    ingestMergeUpdate' MergeMessage.Success |> ignore
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
           [ PlannedBatch [ four.id ]
             PlannedBatch [ seven.id; six.id; eight.id ] ]

    // 7. Start a batch and it fails
    startBatch' () |> ignore
    ingestBuildUpdate' BuildMessage.Failure |> ignore
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
    |> should equal [ PlannedBatch [ seven.id; six.id; eight.id ] ]

    // 8. Start another batch, Six's branch is updated during the build causing an abort
    startBatch' () |> ignore
    updatePullRequestSha' (PullRequestID.create 6666) (SHA.create "66006600") |> ignore
    let ``Start another batch, Six's branch is updated during the build causing an abort`` = fetch()

    let six_v4 = PullRequest.pullRequest (PullRequestID.create 6666) (SHA.create "66006600") [ passedCircleCI ]

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
    |> should equal [ PlannedBatch [ seven.id; eight.id ] ]

    // 9. Six's build starts then passes, start a batch
    updateStatuses' (PullRequestID.create 6666) (SHA.create "66006600") [ pendingCircleCI ] |> ignore
    updateStatuses' (PullRequestID.create 6666) (SHA.create "66006600") [ passedCircleCI ] |> ignore
    startBatch' () |> ignore
    let ``Six's build starts then passes, start a batch`` = fetch()

    let six_v5 = PullRequest.pullRequest (PullRequestID.create 6666) (SHA.create "66006600") [ passedCircleCI ]

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
    |> should equal [ PlannedBatch [ seven.id; eight.id; six.id ] ]

    // 10. Batch builds and merges successfully
    ingestBuildUpdate' (BuildMessage.Success(SHA.create "76800000")) |> ignore
    ingestMergeUpdate' MergeMessage.Success |> ignore
    let ``Batch builds and merges successfully`` = fetch()

    ``Batch builds and merges successfully`` |> should equal MergeQueue.empty
