module MergeQueue.Domain

open MergeQueue.DomainTypes

module PullRequest =
    let create (number: PullRequestNumber) (branchHead: SHA) (commitStatuses: CommitStatuses): PullRequest =
        { number = number
          sha = branchHead
          statuses = commitStatuses }

module CommitStatusState =
    let create (value: string): Result<CommitStatusState, string> =
        match value with
        | "Pending" -> Ok CommitStatusState.Pending
        | "Success" -> Ok CommitStatusState.Success
        | "Failure" -> Ok CommitStatusState.Failure
        | _ -> Error "value must be either 'Pending', 'Success', or 'Failure'"

module CommitStatus =
    let create (context: string, state: string): Result<CommitStatus, string> =
        state
        |> CommitStatusState.create
        |> Result.map (fun s ->
            { context = context
              state = s })

module SHA =
    let create (value: string): Result<SHA, string> =
        Ok(SHA value)

module PullRequestNumber =
    let value (PullRequestNumber id): int =
        id

    let create (value: int): Result<PullRequestNumber, string> =
        Ok(PullRequestNumber value)

module Batch =
    let toPullRequests (Batch batch): List<PullRequest> =
        batch |> List.map (fun ((PassingPullRequest pr), _) -> pr)

    let contains (number: PullRequestNumber) (batch: Batch): bool =
        batch
        |> toPullRequests
        |> List.map (fun pr -> pr.number)
        |> List.contains number

    let length (Batch batch): int =
        List.length batch

    let splitAt (index: int) (Batch batch): Batch * Batch =
        List.splitAt index batch |> (fun (a, b) -> Batch a, Batch b)

module RunnableBatch =
    let toPullRequests (RunnableBatch batch): List<PullRequest> =
        Batch.toPullRequests batch

    let contains (number: PullRequestNumber) (RunnableBatch batch): bool =
        batch |> Batch.contains number

module MergeableBatch =
    let toPullRequests (MergeableBatch batch): List<PullRequest> =
        Batch.toPullRequests batch

    let contains (number: PullRequestNumber) (MergeableBatch batch): bool =
        batch |> Batch.contains number

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

    let contains (number: PullRequestNumber) (AttemptQueue queue): bool =
        queue
        |> List.map (fun ((PassingPullRequest pr), _) -> pr.number)
        |> List.contains number

    let tryFind predicate (AttemptQueue queue) =
        queue |> List.tryFind predicate

    let toPullRequests (AttemptQueue queue): List<PullRequest> =
        queue |> List.map (fun ((PassingPullRequest pr), _) -> pr)

    let removeByNumber (number: PullRequestNumber) (AttemptQueue queue): AttemptQueue =
        queue
        |> List.filter (fun ((PassingPullRequest pr), _) -> pr.number <> number)
        |> AttemptQueue

module SinBin =
    let empty: SinBin =
        SinBin []

    let append (item: NaughtyPullRequest) (SinBin sinBin): SinBin =
        sinBin @ [ item ] |> SinBin

    let tryFind predicate (SinBin sinBin) =
        sinBin |> List.tryFind predicate

    let contains (number: PullRequestNumber) (SinBin sinBin): bool =
        sinBin
        |> List.map (fun (NaughtyPullRequest pr) -> pr.number)
        |> List.contains number

    let toPullRequests (SinBin sinBin): List<PullRequest> =
        sinBin |> List.map (fun (NaughtyPullRequest pr) -> pr)

    let removeByNumber (number: PullRequestNumber) (SinBin sinBin): SinBin =
        sinBin
        |> List.filter (fun (NaughtyPullRequest pr) -> pr.number <> number)
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

    let toPullRequestIds (batch: ActiveBatch): Option<List<PullRequestNumber>> =
        batch
        |> toPullRequests
        |> Option.map (List.map (fun pr -> pr.number))

    let toPlanned (batch: ActiveBatch): Option<PlannedBatch> =
        batch
        |> toPullRequestIds
        |> Option.map (fun ids -> PlannedBatch ids)

    let contains (number: PullRequestNumber) (batch: ActiveBatch): bool =
        batch
        |> toPullRequestIds
        |> Option.map (List.contains number)
        |> Option.defaultValue false

module MergeQueue =
    let empty: MergeQueue =
        { queue = AttemptQueue.empty
          sinBin = SinBin.empty
          activeBatch = NoBatch }

module PlannedBatch =
    let toPullRequestIds (PlannedBatch ids): List<PullRequestNumber> =
        ids

// Domain Logic

let getBuildStatus: GetBuildStatus =
    fun pr ->
        match pr.statuses with
        | [] -> BuildFailure
        | statuses ->
            let anyFailures = statuses |> List.tryFind (fun s -> s.state = CommitStatusState.Failure)
            let anyPending = statuses |> List.tryFind (fun s -> s.state = CommitStatusState.Pending)

            match anyFailures, anyPending with
            | Some _, _ -> BuildFailure
            | _, Some _ -> BuildPending
            | _, _ -> BuildSuccess

let prepareForQueue: PrepareForQueue =
    fun pr ->
        match getBuildStatus pr with
        | BuildSuccess -> PassingPullRequest pr |> Choice1Of2
        | BuildPending -> NaughtyPullRequest pr |> Choice2Of2
        | BuildFailure -> NaughtyPullRequest pr |> Choice2Of2

// TODO: remove unwrapping of AttemptQueue here?
let pickNextBatch: PickNextBatch =
    // SMELL: unwrapping the domain type, is ok here?
    fun (AttemptQueue queue) ->
        match queue with
        | [] -> None
        | (headPr, headA) :: tail ->
            let matching, remaining =
                tail |> List.partition (fun (_, a) -> a = headA)

            let batch =
                (headPr, headA) :: matching |> Batch

            Some(RunnableBatch batch, AttemptQueue remaining)

let bisect: Bisect =
    // SMELL: unwrapping the domain type, is ok here?
    fun (RunnableBatch batch) ->
        // TODO: encapsulate the branch and midpoint calc into Batch.splitInHalf
        if Batch.length batch <= 1 then
            None
        else
            let midpoint = (Batch.length batch) / 2
            Batch.splitAt midpoint batch
            |> fun (a, b) -> BisectedBatch a, BisectedBatch b
            |> fun (a, b) -> BisectedBatch.addBisectPathSegment true a, BisectedBatch.addBisectPathSegment false b
            |> Some

let completeBuild: CompleteBuild =
    // SMELL: does this always succeed?
    fun (RunnableBatch batch) -> MergeableBatch batch |> Merging

let failWithoutRetry: FailWithoutRetry =
    fun (RunnableBatch batch) queue -> queue, NoBatch

let failWithRetry: FailWithRetry =
    fun (BisectedBatch first) (BisectedBatch second) (existing: AttemptQueue) ->
        let queue =
            existing
            |> AttemptQueue.prepend second
            |> AttemptQueue.prepend first

        queue, NoBatch

let completeMerge: CompleteMerge =
    fun (batch: MergeableBatch) (existing: AttemptQueue) -> existing, NoBatch

let failMerge: FailMerge =
    fun (MergeableBatch batch) (existing: AttemptQueue) -> AttemptQueue.prepend batch existing, NoBatch

// SMELL: these were private before command split, are they real domain methods?
let updateShaInQueue: UpdateShaInQueue =
    fun (number: PullRequestNumber) (newValue: SHA) (queue: AttemptQueue) (sinBin: SinBin) ->
        // get PR (and update SHA)
        let updatedPr =
            queue
            |> AttemptQueue.tryFind (fun ((PassingPullRequest pr), _) -> pr.number = number)
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
        let newQueue = queue |> AttemptQueue.removeByNumber number

        newQueue, newSinBin

// SMELL: these were private before command split, are they real domain methods?
let updateShaInSinBin: UpdateShaInSinBin =
    fun (number: PullRequestNumber) (newValue: SHA) (SinBin sinBin) ->
        // TODO: a sha update should always clear the commit statuses
        sinBin
        |> List.map (fun (NaughtyPullRequest pr) ->
            if pr.number = number then NaughtyPullRequest { pr with sha = newValue }
            else NaughtyPullRequest pr)
        |> SinBin

// SMELL: these signatures are huge! Why?
// SMELL: these were private before command split, are they real domain methods?
let updateShaInRunningBatch: UpdateShaInRunningBatch =
    fun (number: PullRequestNumber) (newValue: SHA) (RunnableBatch batch) (queue: AttemptQueue) (sinBin: SinBin) ->
        let inRunningBatch = batch |> Batch.contains number

        if inRunningBatch then
            // TODO: moving PRs back to queue should coincide with changing the CurrentBatch
            // ... it's not obvious that updateSha.. will move PRs into the queue
            // TODO: also, consider adding RunnableBatch.updateSha -> Batch
            let newQueue, newSinBin =
                updateShaInQueue number newValue (AttemptQueue.prepend batch queue) sinBin

            true, newQueue, newSinBin

        else
            let newQueue, newSinBin =
                updateShaInQueue number newValue queue sinBin

            false, newQueue, newSinBin


// SMELL: these signatures are huge! Why?
// SMELL: these were private before command split, are they real domain methods?
let updateStatusesInSinBin: UpdateStatusesInSinBin =
    fun (number: PullRequestNumber) (buildSha: SHA) (statuses: CommitStatuses) (queue: AttemptQueue) (sinBin: SinBin) ->
        // check to see if we should pull the matching commit out of the "sin bin"
        let matching =
            sinBin |> SinBin.tryFind (fun (NaughtyPullRequest pr) -> pr.number = number && pr.sha = buildSha)

        match matching with
        | Some(NaughtyPullRequest pr) ->
            // update PR's status
            let updated = { pr with statuses = statuses }
            let updatedSinBin = sinBin |> SinBin.removeByNumber number

            match prepareForQueue updated with
            | Choice1Of2 passing ->
                let newQueue = AttemptQueue.append passing queue
                newQueue, updatedSinBin
            | Choice2Of2 naughty ->
                let newSinBin = SinBin.append naughty updatedSinBin
                queue, newSinBin
        | None ->
            queue, sinBin

let previewExecutionPlan: PreviewExecutionPlan =
    fun model ->
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
                        |> List.map (fun pr -> pr.number)
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

// "Domain services"

// PATTERN: Maybe a pattern of MergeQueue -> (Something) -> Result<Success * MergeQueue, Error>

let enqueue: Enqueue =
    fun (pullRequest: PullRequest) (model: MergeQueue) ->
        let isBuildFailing = (getBuildStatus pullRequest) = BuildFailure
        // TODO: Concept here, "locate pull request", multiple occurrences
        // TODO: Currently not checking to see if the pull request is currently running!
        let alreadyEnqueued = model.queue |> AttemptQueue.contains pullRequest.number
        let alreadySinBinned = model.sinBin |> SinBin.contains pullRequest.number
        let prepared = prepareForQueue pullRequest

        match isBuildFailing, alreadyEnqueued, alreadySinBinned, prepared with
        | true, _, _, _ ->
            Error RejectedFailingBuildStatus
        | _, true, _, _ ->
            Error AlreadyEnqueued
        | _, _, true, _ ->
            Error AlreadyEnqueued
        | false, false, false, Choice2Of2 naughty ->
            let newModel = { model with sinBin = SinBin.append naughty model.sinBin }
            Ok(EnqueueSuccess.SinBinned(newModel))
        | false, false, false, Choice1Of2 passing ->
            let newModel = { model with queue = AttemptQueue.append passing model.queue }
            Ok(EnqueueSuccess.Enqueued(newModel))

let dequeue: Dequeue =
    fun number model ->
        let isCurrent = model.activeBatch |> ActiveBatch.contains number
        let isEnqueued = model.queue |> AttemptQueue.contains number
        let isSinBinned = model.sinBin |> SinBin.contains number

        match isCurrent, isEnqueued, isSinBinned with
        | true, _, _ ->
            match model.activeBatch with
            | Running(RunnableBatch batch) ->
                let newQueue =
                    model.queue
                    |> AttemptQueue.prepend batch
                    |> AttemptQueue.removeByNumber number

                let newBatch = NoBatch

                let pullRequests = batch |> Batch.toPullRequests

                let newModel =
                    { model with
                          queue = newQueue
                          activeBatch = newBatch }

                let result = DequeuedAndAbortRunningBatch(newModel, pullRequests, number)
                Ok result

            | Merging _ ->
                Error RejectedInMergingBatch

            | NoBatch ->
                // SMELL: this is an impossible branch to get into...
                failwith "PullRequest cannot be in an empty batch"

        | _, true, _ ->
            let newQueue = model.queue |> AttemptQueue.removeByNumber number
            let newModel = { model with queue = newQueue }
            Ok(Dequeued newModel)

        | _, _, true ->
            let newSinBin = model.sinBin |> SinBin.removeByNumber number
            let newModel = { model with sinBin = newSinBin }
            Ok(Dequeued newModel)

        | false, false, false ->
            Error(NotFound)

let startBatch: StartBatch =
    fun model ->
        match model.activeBatch, model.queue with
        | Running _, _ -> Error AlreadyRunning
        | Merging _, _ -> Error AlreadyRunning
        | NoBatch, AttemptQueue [] -> Error EmptyQueue // TODO: Delete this case
        | NoBatch, queue ->
            match pickNextBatch queue with
            | Some(batch, remaining) ->
                let newModel =
                    { model with
                          activeBatch = Running batch
                          queue = remaining }

                let pullRequests = batch |> RunnableBatch.toPullRequests
                Ok(PerformBatchBuild(newModel, pullRequests))
            | None ->
                // SMELL: impossible code path, all non-empty queues have a next batch...
                // SMELL: how could execution get here and result is empty?
                Error EmptyQueue

let ingestBuildUpdate: IngestBuildUpdate =
    fun message model ->
        match model.activeBatch, message with
        | Running failed, Failure ->
            match bisect failed with
            | None ->
                let queue, nextActive = failWithoutRetry failed model.queue

                let newModel =
                    { model with
                          queue = queue
                          activeBatch = nextActive }

                let prs = failed |> RunnableBatch.toPullRequests
                ReportBuildFailureNoRetry (newModel, prs)

            | Some(first, second) ->
                let queue, nextActive = failWithRetry first second model.queue

                let newModel =
                    { model with
                          queue = queue
                          activeBatch = nextActive }

                let prs = failed |> RunnableBatch.toPullRequests
                ReportBuildFailureWithRetry (newModel, prs)

        | Running succeeded, Success targetHead ->
            let nextActive = completeBuild succeeded
            let newModel = { model with activeBatch = nextActive }
            let pullRequests = succeeded |> RunnableBatch.toPullRequests
            let result = PerformBatchMerge(newModel, pullRequests, targetHead)

            result
        | NoBatch, Failure ->
            NoChange
        | Merging _, Failure ->
            NoChange
        | NoBatch, Success _ ->
            NoChange
        | Merging _, Success _ ->
            NoChange

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
