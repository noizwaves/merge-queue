module MergeQueue.Domain

open Xunit
open FsUnit.Xunit
open MergeQueue.Domain

[<Fact>]
let ``Empty queue``() =
    emptyMergeQueue()
    |> getDepth
    |> should equal 0

[<Fact>]
let ``Enqueue a single Pull Request``() =
    let pr = makePullRequest "someOwner" "someRepo" 12

    let result =
        emptyMergeQueue() |> enqueue pr

    result
    |> getDepth
    |> should equal 1

[<Fact>]
let ``Enqueue multiple Pull Requests``() =
    let first = makePullRequest "someOwner" "someRepo" 1
    let second = makePullRequest "someOwner" "someRepo" 22

    let result =
        emptyMergeQueue()
        |> enqueue first
        |> enqueue second

    result
    |> getDepth
    |> should equal 2

[<Fact>]
let ``Start a batch``() =
    let queue =
        emptyMergeQueue()
        |> enqueue (makePullRequest "someOwner" "someRepo" 1)
        |> enqueue (makePullRequest "someOwner" "someRepo" 22)

    let (result, state) =
        queue |> startBatch

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
    let queue =
        emptyMergeQueue()
        |> enqueue (makePullRequest "someOwner" "someRepo" 1)
        |> enqueue (makePullRequest "someOwner" "someRepo" 22)

    let (result, state) =
        queue
        |> startBatch
        |> snd
        |> startBatch

    result |> should equal StartBatchResult.AlreadyRunning

    state
    |> getDepth
    |> should equal 2

[<Fact>]
let ``Attempt to start a batch on an empty queue``() =
    let queue =
        emptyMergeQueue()

    let (result, state) =
        queue |> startBatch

    result |> should equal StartBatchResult.EmptyQueue

    state |> should equal queue

[<Fact>]
let ``Recieve message that batch successfully builds``() =
    let runningQueue =
        emptyMergeQueue()
        |> enqueue (makePullRequest "someOwner" "someRepo" 1)
        |> enqueue (makePullRequest "someOwner" "someRepo" 22)
        |> startBatch
        |> snd

    let result =
        runningQueue |> ingestBuildUpdate (BuildProgressUpdate.Success)

    result
    |> getDepth
    |> should equal 0

[<Fact>]
let ``Recieve message that batch failed the build``() =
    let runningQueue =
        emptyMergeQueue()
        |> enqueue (makePullRequest "someOwner" "someRepo" 1)
        |> enqueue (makePullRequest "someOwner" "someRepo" 22)
        |> startBatch
        |> snd

    let result =
        runningQueue |> ingestBuildUpdate (BuildProgressUpdate.Failure)

    result
    |> getDepth
    |> should equal 2

[<Fact>]
let ``A Pull Request enqueued during running batch is included in the next``() =
    let runningQueue =
        emptyMergeQueue()
        |> enqueue (makePullRequest "someOwner" "someRepo" 1)
        |> enqueue (makePullRequest "someOwner" "someRepo" 22)
        |> startBatch
        |> snd
        |> enqueue (makePullRequest "someOwner" "someRepo" 333)

    runningQueue
    |> getDepth
    |> should equal 3

    let finishedQueue =
        runningQueue |> ingestBuildUpdate (BuildProgressUpdate.Success)

    finishedQueue
    |> getDepth
    |> should equal 1

    let (result, _) =
        finishedQueue |> startBatch

    result |> should equal (StartBatchResult.Success [ makePullRequest "someOwner" "someRepo" 333 ])
