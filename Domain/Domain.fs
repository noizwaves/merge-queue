﻿module MergeQueue.Domain

open MergeQueue.DomainTypes

module PullRequest =
    let create (id: PullRequestID) (branchHead: SHA) (commitStatuses: CommitStatuses): PullRequest =
        { id = id
          sha = branchHead
          statuses = commitStatuses }

module CommitStatusState =
    let create (value: string): CommitStatusState =
        match value with
        | "Pending" -> Pending
        | "Success" -> Success
        | "Failure" -> Failure
        | _ -> failwith "This failed. It can fail. It should return a Result.Error"

module CommitStatus =
    let create (context: string) (state: CommitStatusState): CommitStatus =
        { context = context
          state = state }

module SHA =
    let create (value: string): SHA =
        SHA value

module PullRequestID =
    let value (PullRequestID id): int =
        id

    let create (value: int): PullRequestID =
        PullRequestID value

module Batch =
    let toPullRequests (Batch batch): List<PullRequest> =
        batch |> List.map (fun ((PassingPullRequest pr), _) -> pr)

    let contains (id: PullRequestID) (batch: Batch): bool =
        batch
        |> toPullRequests
        |> List.map (fun pr -> pr.id)
        |> List.contains id

    let length (Batch batch): int =
        List.length batch

    let splitAt (index: int) (Batch batch): Batch * Batch =
        List.splitAt index batch |> (fun (a, b) -> Batch a, Batch b)

module RunnableBatch =
    let toPullRequests (RunnableBatch batch): List<PullRequest> =
        Batch.toPullRequests batch

    let contains (id: PullRequestID) (RunnableBatch batch): bool =
        batch |> Batch.contains id

module MergeableBatch =
    let toPullRequests (MergeableBatch batch): List<PullRequest> =
        Batch.toPullRequests batch

    let contains (id: PullRequestID) (MergeableBatch batch): bool =
        batch |> Batch.contains id

module BisectedBatch =
    // SMELL: kinda stinky, such unwrapping
    let addBisectPathSegment (segment: bool) (BisectedBatch(Batch batch)): BisectedBatch =
        batch
        |> List.map (fun (pr, a) -> pr, a @ [ segment ])
        |> Batch
        |> BisectedBatch

module AttemptQueue =
    let empty: AttemptQueue =
        AttemptQueue []

    let append (item: PassingPullRequest) (AttemptQueue queue): AttemptQueue =
        queue @ [ item, [] ] |> AttemptQueue

    let prepend (Batch batch) (AttemptQueue queue): AttemptQueue =
        batch @ queue |> AttemptQueue

    let contains (id: PullRequestID) (AttemptQueue queue): bool =
        queue
        |> List.map (fun ((PassingPullRequest pr), _) -> pr.id)
        |> List.contains id

    let tryFind predicate (AttemptQueue queue) =
        queue |> List.tryFind predicate

    let toPullRequests (AttemptQueue queue): List<PullRequest> =
        queue |> List.map (fun ((PassingPullRequest pr), _) -> pr)

    let removeById (id: PullRequestID) (AttemptQueue queue): AttemptQueue =
        queue
        |> List.filter (fun ((PassingPullRequest pr), _) -> pr.id <> id)
        |> AttemptQueue

module SinBin =
    let empty: SinBin =
        SinBin []

    let append (item: NaughtyPullRequest) (SinBin sinBin): SinBin =
        sinBin @ [ item ] |> SinBin

    let tryFind predicate (SinBin sinBin) =
        sinBin |> List.tryFind predicate

    let contains (id: PullRequestID) (SinBin sinBin): bool =
        sinBin
        |> List.map (fun (NaughtyPullRequest pr) -> pr.id)
        |> List.contains id

    let toPullRequests (SinBin sinBin): List<PullRequest> =
        sinBin |> List.map (fun (NaughtyPullRequest pr) -> pr)

    let removeById (id: PullRequestID) (SinBin sinBin): SinBin =
        sinBin
        |> List.filter (fun (NaughtyPullRequest pr) -> pr.id <> id)
        |> SinBin

module ActiveBatch =
    let toPullRequests (batch: ActiveBatch): Option<List<PullRequest>> =
        match batch with
        | NoBatch ->
            None
        | Running batch ->
            batch
            |> RunnableBatch.toPullRequests
            |> Some
        | Merging batch ->
            batch
            |> MergeableBatch.toPullRequests
            |> Some

    let toPullRequestIds (batch: ActiveBatch): Option<List<PullRequestID>> =
        batch
        |> toPullRequests
        |> Option.map (List.map (fun pr -> pr.id))

    let toPlanned (batch: ActiveBatch): Option<PlannedBatch> =
        batch
        |> toPullRequestIds
        |> Option.map (fun ids -> PlannedBatch ids)

    let contains (id: PullRequestID) (batch: ActiveBatch): bool =
        batch
        |> toPullRequestIds
        |> Option.map (List.contains id)
        |> Option.defaultValue false

module MergeQueue =
    let empty: MergeQueue =
        { queue = AttemptQueue.empty
          sinBin = SinBin.empty
          activeBatch = NoBatch }

module PlannedBatch =
    let toPullRequestIds (PlannedBatch ids): List<PullRequestID> =
        ids

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

// TODO: remove unwrapping of AttemptQueue here?
let pickNextBatch (AttemptQueue queue): Option<RunnableBatch * AttemptQueue> =
    match queue with
    | [] -> None
    | (headPr, headA) :: tail ->
        let matching, remaining =
            tail |> List.partition (fun (_, a) -> a = headA)

        let batch =
            (headPr, headA) :: matching |> Batch

        Some(RunnableBatch batch, AttemptQueue remaining)

let bisect (RunnableBatch batch): Option<BisectedBatch * BisectedBatch> =
    // TODO: encapsulate the branch and midpoitn calc into Batch.splitInHalf
    if Batch.length batch <= 1 then
        None
    else
        let midpoint = (Batch.length batch) / 2
        Batch.splitAt midpoint batch
        |> fun (a, b) -> BisectedBatch a, BisectedBatch b
        |> fun (a, b) -> BisectedBatch.addBisectPathSegment true a, BisectedBatch.addBisectPathSegment false b
        |> Some

let completeBuild (RunnableBatch batch): ActiveBatch =
    MergeableBatch batch |> Merging

let failWithoutRetry (queue: AttemptQueue) (RunnableBatch batch): AttemptQueue * ActiveBatch =
    queue, NoBatch

let failWithRetry (existing: AttemptQueue) (BisectedBatch first) (BisectedBatch second): AttemptQueue * ActiveBatch =
    let queue =
        existing
        |> AttemptQueue.prepend second
        |> AttemptQueue.prepend first

    queue, NoBatch

let completeMerge (existing: AttemptQueue) (batch: MergeableBatch): AttemptQueue * ActiveBatch =
    existing, NoBatch

let failMerge (existing: AttemptQueue) (MergeableBatch batch): AttemptQueue * ActiveBatch =
    AttemptQueue.prepend batch existing, NoBatch

// SMELL: these were private before command split, are they real domain methods?
let updateShaInQueue (id: PullRequestID) (newValue: SHA) (queue: AttemptQueue) (sinBin: SinBin): AttemptQueue * SinBin =
    // get PR (and update SHA)
    let updatedPr =
        queue
        |> AttemptQueue.tryFind (fun ((PassingPullRequest pr), _) -> pr.id = id)
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

// SMELL: these were private before command split, are they real domain methods?
let updateShaInSinBin (id: PullRequestID) (newValue: SHA) (SinBin sinBin): SinBin =
    // TODO: a sha update should always clear the commit statuses
    sinBin
    |> List.map (fun (NaughtyPullRequest pr) ->
        if pr.id = id then NaughtyPullRequest { pr with sha = newValue }
        else NaughtyPullRequest pr)
    |> SinBin

// SMELL: these signatures are huge! Why?
// SMELL: these were private before command split, are they real domain methods?
let updateShaInRunningBatch (id: PullRequestID) (newValue: SHA) (RunnableBatch batch) (queue: AttemptQueue)
    (sinBin: SinBin): bool * AttemptQueue * SinBin =
    let inRunningBatch = batch |> Batch.contains id

    if inRunningBatch then
        // TODO: moving PRs back to queue should coincide with changing the CurrentBatch
        // ... it's not obvious that updateSha.. will move PRs into the queue
        // TODO: also, consider adding RunnableBatch.updateSha -> Batch
        let newQueue, newSinBin =
            updateShaInQueue id newValue (AttemptQueue.prepend batch queue) sinBin

        true, newQueue, newSinBin

    else
        let newQueue, newSinBin =
            updateShaInQueue id newValue queue sinBin

        false, newQueue, newSinBin


// SMELL: these signatures are huge! Why?
// SMELL: these were private before command split, are they real domain methods?
let updateStatusesInSinBin (id: PullRequestID) (buildSha: SHA) (statuses: CommitStatuses) (queue: AttemptQueue)
    (sinBin: SinBin): AttemptQueue * SinBin =
    // check to see if we should pull the matching commit out of the "sin bin"
    let matching =
        sinBin |> SinBin.tryFind (fun (NaughtyPullRequest pr) -> pr.id = id && pr.sha = buildSha)

    match matching with
    | Some(NaughtyPullRequest pr) ->
        // update PR's status
        let updated = { pr with statuses = statuses }
        let updatedSinBin = sinBin |> SinBin.removeById id

        match prepareForQueue updated with
        | Choice1Of2 passing ->
            let newQueue = AttemptQueue.append passing queue
            newQueue, updatedSinBin
        | Choice2Of2 naughty ->
            let newSinBin = SinBin.append naughty updatedSinBin
            queue, newSinBin
    | None ->
        queue, sinBin

let previewExecutionPlan (model: MergeQueue): ExecutionPlan =
    let rec splitIntoBatches (queue: AttemptQueue): List<PlannedBatch> =
        match queue with
        | AttemptQueue [] ->
            []
        | _ ->
            match pickNextBatch queue with
            | Some(batch, remainder) ->
                let batchIds =
                    batch
                    |> RunnableBatch.toPullRequests
                    |> List.map (fun pr -> pr.id)
                PlannedBatch batchIds :: (splitIntoBatches remainder)
            | None ->
                // SMELL: impossible code path, all non-empty queues have a next batch...
                // SMELL: how could execution get here and result is empty?
                []

    let active =
        model.activeBatch |> ActiveBatch.toPlanned

    let fromQueue =
        model.queue |> splitIntoBatches

    match active with
    | Some b -> b :: fromQueue
    | None -> fromQueue


// "Properties"

// Should these return DTOs?
let peekCurrentQueue (model: MergeQueue): List<PullRequest> =
    model.queue |> AttemptQueue.toPullRequests

let peekCurrentBatch (model: MergeQueue): Option<List<PullRequest>> =
    match model.activeBatch with
    | NoBatch -> None
    | Running batch ->
        batch
        |> RunnableBatch.toPullRequests
        |> Some
    | Merging batch ->
        batch
        |> MergeableBatch.toPullRequests
        |> Some

let peekSinBin (model: MergeQueue): List<PullRequest> =
    model.sinBin |> SinBin.toPullRequests
