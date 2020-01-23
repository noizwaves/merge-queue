module MergeQueue.Domain.IntegrationTests

open Xunit
open FsUnit.Xunit
open MergeQueue.Domain
open MergeQueue.DomainTypes

let private passedCircleCI = commitStatus "circleci" CommitStatusState.Success
let private pendingCircleCI = commitStatus "circleci" CommitStatusState.Pending
let private failedCircleCI = commitStatus "circleci" CommitStatusState.Failure

let private one = pullRequest (pullRequestId 1111) (sha "00001111") [ passedCircleCI ]
let private two = pullRequest (pullRequestId 2222) (sha "00002222") [ passedCircleCI ]
let private three = pullRequest (pullRequestId 3333) (sha "00003333") [ passedCircleCI ]
let private four = pullRequest (pullRequestId 4444) (sha "00004444") [ passedCircleCI ]
let private five = pullRequest (pullRequestId 5555) (sha "00005555") [ pendingCircleCI ]
let private six = pullRequest (pullRequestId 6666) (sha "00006666") [ passedCircleCI ]
let private seven = pullRequest (pullRequestId 7777) (sha "00007777") [ passedCircleCI ]
let private eight = pullRequest (pullRequestId 8888) (sha "00008888") [ passedCircleCI ]



[<Fact>]
let ``Realistic workflow``() =
    // 1. Four enqueued but not started
    let ``Four enqueued but not started`` =
        emptyMergeQueue
        |> enqueue one
        |> snd
        |> enqueue two
        |> snd
        |> enqueue three
        |> snd
        |> enqueue four
        |> snd

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
    |> should equal [ [ one; two; three; four ] ]

    // 2. First batch running some additional enqueued
    let ``First batch running some additional enqueued`` =
        ``Four enqueued but not started``
        |> startBatch
        |> snd
        |> enqueue five
        |> snd
        |> enqueue six
        |> snd

    ``First batch running some additional enqueued``
    |> peekCurrentQueue
    |> should equal [ one; two; three; four; six ]

    ``First batch running some additional enqueued``
    |> peekCurrentBatch
    |> should equal (Some [ one; two; three; four ])

    ``First batch running some additional enqueued``
    |> peekSinBin
    |> should equal [ five ]

    ``First batch running some additional enqueued``
    |> previewExecutionPlan
    |> should equal
           [ [ one; two; three; four ]
             [ six ] ]

    // 3. Five fails to build, Six's branch is updated, Seven is enqueued, batch continues to build
    let ``Five fails to build, Six's branch is updated, batch continues to build`` =
        ``First batch running some additional enqueued``
        |> updateStatuses (pullRequestId 5555) (sha "00005555") [ failedCircleCI ]
        |> updatePullRequestSha (pullRequestId 6666) (sha "60606060")
        |> snd
        |> enqueue seven
        |> snd

    let six_v2 = pullRequest (pullRequestId 6666) (sha "60606060") [ passedCircleCI ]
    let five_v2 = pullRequest (pullRequestId 5555) (sha "00005555") [ failedCircleCI ]

    ``Five fails to build, Six's branch is updated, batch continues to build``
    |> peekCurrentQueue
    |> should equal [ one; two; three; four; seven ]

    ``Five fails to build, Six's branch is updated, batch continues to build``
    |> peekCurrentBatch
    |> should equal (Some [ one; two; three; four ])

    ``Five fails to build, Six's branch is updated, batch continues to build``
    |> peekSinBin
    |> should equal [ five_v2; six_v2 ]

    ``Five fails to build, Six's branch is updated, batch continues to build``
    |> previewExecutionPlan
    |> should equal
           [ [ one; two; three; four ]
             [ seven ] ]

    // 4. Five's branch is updated, Batch fails to build, Six's build passes
    let ``Five's branch is updated, Batch fails to build, Six's build passes`` =
        ``Five fails to build, Six's branch is updated, batch continues to build``
        |> updatePullRequestSha (pullRequestId 5555) (sha "50505050")
        |> snd
        |> ingestBuildUpdate BuildMessage.Failure
        |> snd
        |> updateStatuses (pullRequestId 6666) (sha "60606060") [ passedCircleCI ]

    let six_v3 = pullRequest (pullRequestId 6666) (sha "60606060") [ passedCircleCI ]
    let five_v3 = pullRequest (pullRequestId 5555) (sha "50505050") [ failedCircleCI ]

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
           [ [ one; two ]
             [ three; four ]
             [ seven; six_v3 ] ]

    // 5. Start another batch, Eight is enqueued, Five's build fails again
    let ``Start another batch, Eight is enqueued, Five's build fails again`` =
        ``Five's branch is updated, Batch fails to build, Six's build passes``
        |> startBatch
        |> snd
        |> enqueue eight
        |> snd
        |> updateStatuses (pullRequestId 5555) (sha "50505050") [ failedCircleCI ]

    let five_v4 = pullRequest (pullRequestId 5555) (sha "50505050") [ failedCircleCI ]

    ``Start another batch, Eight is enqueued, Five's build fails again``
    |> peekCurrentQueue
    |> should equal [ one; two; three; four; seven; six_v3; eight ]

    ``Start another batch, Eight is enqueued, Five's build fails again``
    |> peekCurrentBatch
    |> should equal (Some [ one; two ])

    ``Start another batch, Eight is enqueued, Five's build fails again``
    |> peekSinBin
    |> should equal [ five_v4 ]

    ``Start another batch, Eight is enqueued, Five's build fails again``
    |> previewExecutionPlan
    |> should equal
           [ [ one; two ]
             [ three; four ]
             [ seven; six_v3; eight ] ]

    // 6. Five is dequeued, Three is dequeued, The batch builds and merges successfully
    let ``Five is dequeued, Three is dequeued, The batch builds and merges successfully`` =
        ``Start another batch, Eight is enqueued, Five's build fails again``
        |> dequeue (pullRequestId 5555)
        |> snd
        |> dequeue (pullRequestId 3333)
        |> snd
        |> ingestBuildUpdate (BuildMessage.Success(sha "12000000"))
        |> snd
        |> ingestMergeUpdate MergeMessage.Success
        |> snd

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
           [ [ four ]
             [ seven; six_v3; eight ] ]

    // 7. Start a batch and it fails
    let ``Start a batch and it fails`` =
        ``Five is dequeued, Three is dequeued, The batch builds and merges successfully``
        |> startBatch
        |> snd
        |> ingestBuildUpdate BuildMessage.Failure
        |> snd

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
    |> should equal [ [ seven; six_v3; eight ] ]

    // 8. Start another batch, Six's branch is updated during the build causing an abort
    let ``Start another batch, Six's branch is updated during the build causing an abort`` =
        ``Start a batch and it fails``
        |> startBatch
        |> snd
        |> updatePullRequestSha (pullRequestId 6666) (sha "66006600")
        |> snd

    let six_v4 = pullRequest (pullRequestId 6666) (sha "66006600") [ passedCircleCI ]

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
    |> should equal [ [ seven; eight ] ]

    // 9. Six's build starts then passes, start a batch
    let ``Six's build starts then passes, start a batch`` =
        ``Start another batch, Six's branch is updated during the build causing an abort``
        |> updateStatuses (pullRequestId 6666) (sha "66006600") [ pendingCircleCI ]
        |> updateStatuses (pullRequestId 6666) (sha "66006600") [ passedCircleCI ]
        |> startBatch
        |> snd

    let six_v5 = pullRequest (pullRequestId 6666) (sha "66006600") [ passedCircleCI ]

    ``Six's build starts then passes, start a batch``
    |> peekCurrentQueue
    |> should equal [ seven; eight; six_v5 ]

    ``Six's build starts then passes, start a batch``
    |> peekCurrentBatch
    |> should equal (Some [ seven; eight; six_v5 ])

    ``Six's build starts then passes, start a batch``
    |> peekSinBin
    |> should be Empty

    ``Six's build starts then passes, start a batch``
    |> previewExecutionPlan
    |> should equal [ [ seven; eight; six_v5 ] ]

    // 10. Batch builds and merges successfully
    let ``Batch builds and merges successfully`` =
        ``Six's build starts then passes, start a batch``
        |> ingestBuildUpdate (BuildMessage.Success(sha "76800000"))
        |> snd
        |> ingestMergeUpdate MergeMessage.Success
        |> snd

    ``Batch builds and merges successfully`` |> should equal emptyMergeQueue
