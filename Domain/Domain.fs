﻿module MergeQueue.Domain

open MergeQueue.DomainTypes

module MergeQueue =
    let emptyMergeQueue: MergeQueue =
        { queue = []
          sinBin = SinBin []
          batch = NoBatch }

module PullRequest =
    let pullRequest (id: PullRequestID) (branchHead: SHA) (commitStatuses: CommitStatuses): PullRequest =
        { id = id
          sha = branchHead
          statuses = commitStatuses }

module CommitStatus =
    let create (context: string) (state: CommitStatusState): CommitStatus =
        { context = context
          state = state }

module SHA =
    let create (value: string): SHA =
        SHA value

module PullRequestID =
    let getValue (PullRequestID id): int =
        id

    let create (value: int): PullRequestID =
        PullRequestID value

module Batch =
    let toPullRequests (Batch batch): List<PullRequest> =
        batch |> List.map (fun ((PassingPullRequest pr), _) -> pr)

module AttemptQueue =
    let append (item: PassingPullRequest) (queue: AttemptQueue): AttemptQueue =
        queue @ [ item, [] ]

    let prepend (Batch batch) (queue: AttemptQueue) =
        batch @ queue

    let toPullRequests (queue: AttemptQueue): List<PullRequest> =
        queue |> List.map (fun ((PassingPullRequest pr), _) -> pr)

    let removeById (id: PullRequestID) (queue: AttemptQueue): AttemptQueue =
        queue |> List.filter (fun ((PassingPullRequest pr), _) -> pr.id <> id)

module SinBin =
    let tryFind pred (SinBin sinBin) =
        sinBin |> List.tryFind pred

    let append pr (SinBin sinBin): SinBin =
        sinBin @ [ pr ] |> SinBin

    let toPullRequests (SinBin sinBin): List<PullRequest> =
        sinBin |> List.map (fun (NaughtyPullRequest pr) -> pr)

    let remove (id: PullRequestID) (SinBin sinBin): SinBin =
        sinBin
        |> List.filter (fun (NaughtyPullRequest pr) -> pr.id <> id)
        |> SinBin

// Helpers
let private inQueue (id: PullRequestID) (queue: AttemptQueue): bool =
    queue
    |> List.map fst
    |> List.map (fun (PassingPullRequest pr) -> pr.id)
    |> List.contains id

let private inSinBin (id: PullRequestID) (SinBin sinBin): bool =
    sinBin
    |> List.map (fun (NaughtyPullRequest pr) -> pr.id)
    |> List.contains id

let private inBatch (id: PullRequestID) (Batch batch): bool =
    batch
    |> List.map (fun ((PassingPullRequest pr), _) -> pr.id)
    |> List.contains id

let private inRunningBatch (id: PullRequestID) (current: CurrentBatch): bool =
    match current with
    | Running batch ->
        inBatch id batch
    | Merging _ ->
        false
    | NoBatch ->
        false

let private inCurrentBatch (id: PullRequestID) (current: CurrentBatch): bool =
    match current with
    | Running batch ->
        inBatch id batch
    | Merging batch ->
        inBatch id batch
    | NoBatch ->
        false

// Domain Logic

let getBuildStatus (pullRequest: PullRequest): BuildStatus =
    match pullRequest.statuses with
    | [] -> BuildFailure
    | statuses ->
        let anyFailures = statuses |> List.tryFind (fun s -> s.state = CommitStatusState.Failure)
        let anyPending = statuses |> List.tryFind (fun s -> s.state = CommitStatusState.Pending)

        match anyFailures, anyPending with
        | Some _, _ -> BuildFailure
        | _, Some _ -> BuildPending
        | _, _ -> BuildSuccess

let prepareForQueue (pr: PullRequest): Choice<PassingPullRequest, NaughtyPullRequest> =
    match getBuildStatus pr with
    | BuildSuccess -> PassingPullRequest pr |> Choice1Of2
    | BuildPending -> NaughtyPullRequest pr |> Choice2Of2
    | BuildFailure -> NaughtyPullRequest pr |> Choice2Of2

let addPullRequestToQueue (pullRequest: PassingPullRequest) (queue: AttemptQueue): AttemptQueue =
    queue @ [ pullRequest, [] ]

let addPullRequestToSinBin (pullRequest: NaughtyPullRequest) (SinBin sinBin): SinBin =
    sinBin @ [ pullRequest ] |> SinBin

let pickNextBatch (queue: AttemptQueue): Option<Batch * AttemptQueue> =
    match queue with
    | [] -> None
    | (headPr, headA) :: tail ->
        let matching, remaining =
            tail |> List.partition (fun (_, a) -> a = headA)

        let batch =
            (headPr, headA) :: matching |> Batch

        Some(batch, remaining)

let bisect (Batch batch): Option<Batch * Batch> =
    if List.length batch <= 1 then
        None
    else
        let midpoint = (List.length batch) / 2
        List.splitAt midpoint batch
        |> fun (a, b) -> Batch a, Batch b
        |> Some

let completeBuild (batch: Batch): CurrentBatch =
    Merging batch

let failWithoutRetry (batch: Batch) (queue: AttemptQueue) =
    queue, NoBatch

let failWithRetry (Batch first) (Batch second) (existing: AttemptQueue): AttemptQueue * CurrentBatch =
    let first' = first |> List.map (fun (pr, a) -> pr, a @ [ true ])
    let second' = second |> List.map (fun (pr, a) -> pr, a @ [ false ])
    let queue = first' @ second' @ existing
    queue, NoBatch

let completeMerge (batch: Batch) (existing: AttemptQueue): AttemptQueue * CurrentBatch =
    existing, NoBatch

let failMerge (Batch batch) (queue: AttemptQueue): CurrentBatch * AttemptQueue =
    NoBatch, batch @ queue

let private updateShaInQueue (id: PullRequestID) (newValue: SHA) (queue: AttemptQueue) (sinBin: SinBin): AttemptQueue * SinBin =
    // get PR (and update SHA)
    let updatedPr =
        queue
        |> List.tryFind (fun ((PassingPullRequest pr), _) -> pr.id = id)
        // TODO: a sha update should always clear the commit statuses, always making it a NaughtyPullRequest
        |> Option.map (fun ((PassingPullRequest pr), _) -> { pr with sha = newValue })
        |> Option.map NaughtyPullRequest

    // we know it is naughty because it *should* have empty statuses
    // which implies it is naughty, but we don't call PrepareForQueue


    // move to the sin bin
    let newSinBin =
        match updatedPr with
        | None -> sinBin
        | Some item -> sinBin |> SinBin.append item

    // from the queue
    let newQueue = queue |> AttemptQueue.removeById id

    newQueue, newSinBin

let private updateShaInSinBin (id: PullRequestID) (newValue: SHA) (SinBin sinBin): SinBin =
    // TODO: a sha update should always clear the commit statuses
    sinBin
    |> List.map (fun (NaughtyPullRequest pr) ->
        if pr.id = id then NaughtyPullRequest { pr with sha = newValue }
        else NaughtyPullRequest pr)
    |> SinBin

// SMELL: these signatures are huge! Why?
let private updateShaInRunningBatch (id: PullRequestID) (newValue: SHA) (batch: Batch) (queue: AttemptQueue)
    (sinBin: SinBin): bool * AttemptQueue * SinBin =
    let inRunningBatch = batch |> inBatch id

    if inRunningBatch then
        // TODO: moving PRs back to queue should coincide with changing the CurrentBatch
        // ... it's not obvious that updateSha.. will move PRs into the queue
        let newQueue, newSinBin =
            updateShaInQueue id newValue (AttemptQueue.prepend batch queue) sinBin

        true, newQueue, newSinBin

    else
        let newQueue, newSinBin =
            updateShaInQueue id newValue queue sinBin

        false, newQueue, newSinBin


// SMELL: these signatures are huge! Why?
let private updateStatusesInSinBin (id: PullRequestID) (buildSha: SHA) (statuses: CommitStatuses) (queue: AttemptQueue)
    (sinBin: SinBin): AttemptQueue * SinBin =
    // check to see if we should pull the matching commit out of the "sin bin"
    let matching =
        sinBin |> SinBin.tryFind (fun (NaughtyPullRequest pr) -> pr.id = id && pr.sha = buildSha)

    match matching with
    | Some(NaughtyPullRequest pr) ->
        // update PR's status
        let updated = { pr with statuses = statuses }
        let updatedSinBin = sinBin |> SinBin.remove id

        match prepareForQueue updated with
        | Choice1Of2 passing ->
            let newQueue = AttemptQueue.append passing queue
            newQueue, updatedSinBin
        | Choice2Of2 naughty ->
            let newSinBin = SinBin.append naughty updatedSinBin
            queue, newSinBin
    | None ->
        queue, sinBin


// Commands
type EnqueueResult =
    | Enqueued
    | SinBinned
    | RejectedFailingBuildStatus
    | AlreadyEnqueued

let enqueue (pullRequest: PullRequest) (model: MergeQueue): EnqueueResult * MergeQueue =
    let isBuildFailing = (getBuildStatus pullRequest) = BuildFailure
    let alreadyEnqueued = model.queue |> inQueue pullRequest.id
    let alreadySinBinned = model.sinBin |> inSinBin pullRequest.id
    let prepared = prepareForQueue pullRequest

    match isBuildFailing, alreadyEnqueued, alreadySinBinned, prepared with
    | true, _, _, _ -> RejectedFailingBuildStatus, model
    | _, true, _, _ -> AlreadyEnqueued, model
    | _, _, true, _ -> AlreadyEnqueued, model
    | false, false, false, Choice2Of2 naughty ->
        let newModel = { model with sinBin = addPullRequestToSinBin naughty model.sinBin }
        SinBinned, newModel
    | false, false, false, Choice1Of2 passing ->
        let newModel = { model with queue = addPullRequestToQueue passing model.queue }
        Enqueued, newModel

type DequeueResult =
    | Dequeued
    | DequeuedAndAbortRunningBatch of List<PullRequest> * PullRequestID
    | RejectedInMergingBatch
    | NotFound

let dequeue (id: PullRequestID) (model: MergeQueue): DequeueResult * MergeQueue =
    // TODO: Concept here, "locate pull request"
    let isCurrent = model.batch |> inCurrentBatch id
    let isEnqueued = model.queue |> inQueue id
    let isSinBinned = model.sinBin |> inSinBin id

    let result, newModel =
        match isCurrent, isEnqueued, isSinBinned with
        | true, _, _ ->
            match model.batch with
            | Running batch ->
                let newQueue =
                    model.queue
                    |> AttemptQueue.prepend batch
                    |> AttemptQueue.removeById id

                let newBatch = NoBatch

                let pullRequests = batch |> Batch.toPullRequests
                let result = DequeuedAndAbortRunningBatch(pullRequests, id)

                let newModel =
                    { model with
                          queue = newQueue
                          batch = newBatch }
                result, newModel

            | Merging _ ->
                RejectedInMergingBatch, model

            | NoBatch ->
                // SMELL: this is an impossible branch to get into...
                failwith "PullRequest cannot be in an empty batch"

        | _, true, _ ->
            let newQueue = model.queue |> AttemptQueue.removeById id
            Dequeued, { model with queue = newQueue }

        | _, _, true ->
            let newSinBin = model.sinBin |> SinBin.remove id
            Dequeued, { model with sinBin = newSinBin }

        | false, false, false ->
            NotFound, model

    result, newModel

type StartBatchResult =
    | PerformBatchBuild of List<PullRequest>
    | AlreadyRunning
    | EmptyQueue

// SMELL: what calls this? synchronous after some other call?
// maybe make start batch private, and call it inside enqueue && updateStatus?
let startBatch (model: MergeQueue): StartBatchResult * MergeQueue =
    match model.batch, model.queue with
    | Running _, _ -> AlreadyRunning, model
    | Merging _, _ -> AlreadyRunning, model
    | NoBatch, [] -> EmptyQueue, model
    | NoBatch, queue ->
        match pickNextBatch queue with
        | Some(batch, remaining) ->
            let pullRequests = batch |> Batch.toPullRequests
            PerformBatchBuild pullRequests,
            { model with
                  batch = Running batch
                  queue = remaining }
        | None ->
            // SMELL: impossible code path, all non-empty queues have a next batch...
            // SMELL: how could execution get here and result is empty?
            EmptyQueue, model

type BuildMessage =
    | Success of SHA
    | Failure

type IngestBuildResult =
    | NoOp
    | PerformBatchMerge of List<PullRequest> * SHA
    | ReportBuildFailureWithRetry of List<PullRequest>
    | ReportBuildFailureNoRetry of List<PullRequest>

let ingestBuildUpdate (message: BuildMessage) (model: MergeQueue): IngestBuildResult * MergeQueue =
    match model.batch, message with
    | Running batch, Failure ->
        match bisect batch with
        | None ->
            let newQueue, newBatch = model.queue |> failWithoutRetry batch

            let newModel =
                { model with
                      queue = newQueue
                      batch = newBatch }

            let b = batch |> Batch.toPullRequests
            ReportBuildFailureNoRetry b, newModel

        | Some(first, second) ->
            let newQueue, newBatch = model.queue |> failWithRetry first second

            let newModel =
                { model with
                      queue = newQueue
                      batch = newBatch }

            let b = batch |> Batch.toPullRequests
            ReportBuildFailureWithRetry b, newModel

    | Running succeeded, Success targetHead ->
        let newBatch = completeBuild succeeded
        let newState = { model with batch = newBatch }
        let pullRequests = succeeded |> Batch.toPullRequests
        let result = PerformBatchMerge(pullRequests, targetHead)
        result, newState

    | NoBatch, Failure ->
        NoOp, model
    | Merging _, Failure ->
        NoOp, model
    | NoBatch, Success _ ->
        NoOp, model
    | Merging _, Success _ ->
        NoOp, model

type MergeMessage =
    | Success
    | Failure

type IngestMergeResult =
    | NoOp
    | MergeComplete of List<PullRequest>
    | ReportMergeFailure of List<PullRequest>

let ingestMergeUpdate (message: MergeMessage) (model: MergeQueue): IngestMergeResult * MergeQueue =
    match model.batch, message with
    | Merging merged, MergeMessage.Success ->
        let newQueue, newBatch = model.queue |> completeMerge merged

        let newModel =
            { model with
                  queue = newQueue
                  batch = newBatch }

        let pullRequests = merged |> Batch.toPullRequests

        MergeComplete pullRequests, newModel
    | Merging unmerged, MergeMessage.Failure ->
        let newBatch, newQueue = failMerge unmerged model.queue
        let pullRequests = unmerged |> Batch.toPullRequests

        let newModel =
            { model with
                  batch = newBatch
                  queue = newQueue }
        ReportMergeFailure pullRequests, newModel
    | _, _ ->
        IngestMergeResult.NoOp, model

type UpdatePullRequestResult =
    | NoOp
    | AbortRunningBatch of List<PullRequest> * PullRequestID
    | AbortMergingBatch of List<PullRequest> * PullRequestID

let updatePullRequestSha (id: PullRequestID) (newValue: SHA) (model: MergeQueue): UpdatePullRequestResult * MergeQueue =
    let newSinBin = model.sinBin |> updateShaInSinBin id newValue
    let modelWithNewSinBin = { model with sinBin = newSinBin }

    match modelWithNewSinBin.batch with
    | Running batch ->
        let abortRunningBatch, newQueue, newSinBin =
            updateShaInRunningBatch id newValue batch modelWithNewSinBin.queue modelWithNewSinBin.sinBin

        if abortRunningBatch then
            let newModel =
                { modelWithNewSinBin with
                      queue = newQueue
                      batch = NoBatch
                      sinBin = newSinBin }

            let pullRequests = batch |> Batch.toPullRequests
            AbortRunningBatch(pullRequests, id), newModel

        else
            let newModel =
                { modelWithNewSinBin with
                      queue = newQueue
                      sinBin = newSinBin }
            NoOp, newModel
    | NoBatch ->
        let newQueue, newSinBin =
            updateShaInQueue id newValue modelWithNewSinBin.queue modelWithNewSinBin.sinBin

        let newModel =
            { modelWithNewSinBin with
                  queue = newQueue
                  sinBin = newSinBin }
        NoOp, newModel

    | Merging batch ->
        let inMergingBatch = batch |> inBatch id

        if inMergingBatch then
            let newModel =
                { modelWithNewSinBin with batch = NoBatch }

            let pullRequests = batch |> Batch.toPullRequests
            AbortMergingBatch(pullRequests, id), newModel
        else
            let newQueue, newSinBin =
                updateShaInQueue id newValue modelWithNewSinBin.queue modelWithNewSinBin.sinBin

            let newModel =
                { modelWithNewSinBin with
                      queue = newQueue
                      sinBin = newSinBin }
            NoOp, newModel

let updateStatuses (id: PullRequestID) (buildSha: SHA) (statuses: CommitStatuses) (model: MergeQueue): MergeQueue =
    // check to see if we should pull the matching commit out of the "sin bin"
    let newQueue, newSinBin =
        updateStatusesInSinBin id buildSha statuses model.queue model.sinBin

    { model with
          queue = newQueue
          sinBin = newSinBin }

// "Properties"

// Should these return DTOs?
let peekCurrentQueue (model: MergeQueue): List<PullRequest> =
    model.queue |> AttemptQueue.toPullRequests

let peekCurrentBatch (model: MergeQueue): Option<List<PullRequest>> =
    match model.batch with
    | NoBatch -> None
    | Running batch ->
        batch
        |> Batch.toPullRequests
        |> Some
    | Merging batch ->
        batch
        |> Batch.toPullRequests
        |> Some

let peekSinBin (model: MergeQueue): List<PullRequest> =
    model.sinBin |> SinBin.toPullRequests

let previewExecutionPlan (model: MergeQueue): ExecutionPlan =
    let rec splitIntoBatches (queue: AttemptQueue): List<PlannedBatch> =
        match queue with
        | [] ->
            []
        | _ ->
            match pickNextBatch queue with
            | Some(batch, remainder) ->
                let batchIds =
                    batch
                    |> Batch.toPullRequests
                    |> List.map (fun pr -> pr.id)
                PlannedBatch batchIds :: (splitIntoBatches remainder)
            | None ->
                // SMELL: impossible code path, all non-empty queues have a next batch...
                // SMELL: how could execution get here and result is empty?
                []

    let current =
        match model.batch with
        | NoBatch -> []
        | Running batch ->
            let batchIds =
                batch
                |> Batch.toPullRequests
                |> List.map (fun pr -> pr.id)
            [ PlannedBatch batchIds ]
        | Merging batch ->
            let batchIds =
                batch
                |> Batch.toPullRequests
                |> List.map (fun pr -> pr.id)
            [ PlannedBatch batchIds ]

    let fromQueue =
        model.queue |> splitIntoBatches

    current @ fromQueue
