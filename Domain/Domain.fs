module MergeQueue.Domain

open MergeQueue.DomainTypes

// Constructors
let emptyMergeQueue: MergeQueue =
    { queue = []
      sinBin = []
      batch = NoBatch }

let pullRequest (id: PullRequestID) (branchHead: SHA) (commitStatuses: CommitStatuses): PullRequest =
    { id = id
      sha = branchHead
      statuses = commitStatuses }

let pullRequestId (value: int): PullRequestID =
    PullRequestID value

let sha (value: string): SHA =
    SHA value

let commitStatus (context: string) (state: CommitStatusState): CommitStatus =
    { context = context
      state = state }

// "Getters"
let getPullRequestIDValue (PullRequestID id): int =
    id

// Helpers
let private removeAllFromQueue (toRemove: List<PullRequest>) (queue: AttemptQueue): AttemptQueue =
    let removeIds = toRemove |> List.map (fun pr -> pr.id)
    queue |> List.filter (fun ((PassingPullRequest pr), _) -> List.contains pr.id removeIds |> not)

let private inQueue (id: PullRequestID) (queue: AttemptQueue): bool =
    queue
    |> List.map fst
    |> List.map (fun (PassingPullRequest pr) -> pr.id)
    |> List.contains id

let private inSinBin (id: PullRequestID) (sinBin: SinBin): bool =
    sinBin
    |> List.map (fun (NaughtyPullRequest pr) -> pr.id)
    |> List.contains id

let private inBatch (id: PullRequestID) (batch: Batch): bool =
    batch
    |> List.map (fun pr -> pr.id)
    |> List.contains id

let private inRunningBatch (id: PullRequestID) (current: CurrentBatch): bool =
    match current with
    | Running batch ->
        inBatch id batch
    | Merging _ ->
        false
    | NoBatch ->
        false

// Domain Logic

let removeFromQueue (id: PullRequestID) (queue: AttemptQueue): AttemptQueue =
    queue |> List.filter (fun ((PassingPullRequest pr), _) -> pr.id <> id)

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

let addPullRequestToSinBin (pullRequest: NaughtyPullRequest) (sinBin: SinBin): SinBin =
    sinBin @ [ pullRequest ]

let pickNextBatch (queue: AttemptQueue): Option<Batch> =
    match queue with
    | [] -> None
    | head :: tail ->
        let matching =
            tail |> List.filter (fun (_, a) -> a = (head |> snd))

        let batch = head :: matching |> List.map (fun ((PassingPullRequest pr), _) -> pr)

        Some batch

let bisect (batch: Batch): Option<Batch * Batch> =
    if List.length batch <= 1 then
        None
    else
        let midpoint = (List.length batch) / 2
        List.splitAt midpoint batch |> Some

let completeBuild (batch: Batch): CurrentBatch =
    Merging batch

let private failWithoutRetry (batch: Batch) (queue: AttemptQueue) =
    let newQueue = queue |> removeAllFromQueue batch
    newQueue, NoBatch

let private failWithRetry (first: Batch) (second: Batch) (queue: AttemptQueue): AttemptQueue * CurrentBatch =
    let firstIds = first |> List.map (fun pr -> pr.id)
    let secondIds = second |> List.map (fun pr -> pr.id)

    let newQueue =
        queue
        |> List.map (fun ((PassingPullRequest pr), a) ->
            if List.contains pr.id firstIds then (PassingPullRequest pr, a @ [ true ])
            elif List.contains pr.id secondIds then (PassingPullRequest pr, a @ [ false ])
            else PassingPullRequest pr, a)
    newQueue, NoBatch

let private completeMerge (batch: Batch) (queue: AttemptQueue): AttemptQueue * CurrentBatch =
    let newQueue = queue |> removeAllFromQueue batch
    newQueue, NoBatch

let private failMerge (_batch: Batch): CurrentBatch =
    NoBatch

let private updateShaInQueue (id: PullRequestID) (newValue: SHA) (queue: AttemptQueue) (sinBin: SinBin): AttemptQueue * SinBin =
    // get PR (and update SHA)
    let updatedPr =
        queue
        |> List.tryFind (fun ((PassingPullRequest pr), _) -> pr.id = id)
        |> Option.map (fun ((PassingPullRequest pr), _) -> { pr with sha = newValue })

    // we know it is naughty because it *should* have empty statuses
    // which implies it is naughty, but we don't call PrepareForQueue

    // TODO: a sha update should always clear the commit statuses

    // move to the sin bin
    let newSinBin =
        match updatedPr with
        | None -> sinBin
        | Some item -> sinBin @ [ NaughtyPullRequest item ]

    // from the queue
    let newQueue =
        queue |> List.filter (fun ((PassingPullRequest pr), _) -> pr.id <> id)

    newQueue, newSinBin

let private updateShaInSinBin (id: PullRequestID) (newValue: SHA) (sinBin: SinBin): SinBin =
    // TODO: a sha update should always clear the commit statuses
    sinBin
    |> List.map (fun (NaughtyPullRequest pr) ->
        if pr.id = id then NaughtyPullRequest { pr with sha = newValue }
        else NaughtyPullRequest pr)

// SMELL: these signatures are huge! Why?
let private updateShaInRunningBatch (id: PullRequestID) (newValue: SHA) (batch: Batch) (queue: AttemptQueue)
    (sinBin: SinBin): bool * AttemptQueue * SinBin =
    let inRunningBatch = batch |> inBatch id

    if inRunningBatch then
        let newQueue, newSinBin =
            updateShaInQueue id newValue queue sinBin

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
        sinBin |> List.tryFind (fun (NaughtyPullRequest pr) -> pr.id = id && pr.sha = buildSha)

    match matching with
    | Some(NaughtyPullRequest pr) ->
        // update PR's status
        let updated = { pr with statuses = statuses }
        let updatedSinBin =
            sinBin |> List.where (fun (NaughtyPullRequest p) -> p <> pr)

        match prepareForQueue updated with
        | Choice1Of2 passing ->
            let newQueue = queue @ [ passing, [] ]
            newQueue, updatedSinBin
        | Choice2Of2 naughty ->
            let newSinBin = updatedSinBin @ [ naughty ]
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
    | DequeuedAndAbortRunningBatch of Batch * PullRequestID
    | RejectedInMergingBatch
    | NotFound

let dequeue (id: PullRequestID) (model: MergeQueue): DequeueResult * MergeQueue =
    let isEnqueued = model.queue |> inQueue id
    let isSinBinned = model.sinBin |> inSinBin id

    let result, newModel =
        match isEnqueued, isSinBinned with
        | true, _ ->
            match model.batch with
            | Running batch ->
                let result, newBatch =
                    if inBatch id batch then DequeuedAndAbortRunningBatch(batch, id), NoBatch
                    else Dequeued, model.batch

                let newQueue = model.queue |> removeFromQueue id

                let newModel =
                    { model with
                          queue = newQueue
                          batch = newBatch }
                result, newModel

            | Merging batch ->
                let result, newQueue =
                    if inBatch id batch then RejectedInMergingBatch, model.queue
                    else Dequeued, model.queue |> removeFromQueue id

                let newModel = { model with queue = newQueue }
                result, newModel

            | NoBatch ->
                let newQueue = model.queue |> removeFromQueue id
                Dequeued, { model with queue = newQueue }

        | _, true ->
            let newSinBin = model.sinBin |> List.where (fun (NaughtyPullRequest pr) -> pr.id <> id)
            Dequeued, { model with sinBin = newSinBin }

        | false, false ->
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
    | _, [] -> EmptyQueue, model
    | Running _, _ -> AlreadyRunning, model
    | Merging _, _ -> AlreadyRunning, model
    | NoBatch, queue ->
        let nextBatch = queue |> pickNextBatch
        match nextBatch with
        | Some batch ->
            PerformBatchBuild batch, { model with batch = Running batch }
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
            ReportBuildFailureNoRetry batch, newModel

        | Some(first, second) ->
            let newQueue, newBatch = model.queue |> failWithRetry first second

            let newModel =
                { model with
                      queue = newQueue
                      batch = newBatch }
            ReportBuildFailureWithRetry batch, newModel

    | Running succeeded, Success targetHead ->
        let newBatch = completeBuild succeeded
        let newState = { model with batch = newBatch }
        let result = PerformBatchMerge(succeeded, targetHead)
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

        MergeComplete merged, newModel
    | Merging batch, MergeMessage.Failure ->
        let newBatch = failMerge batch
        let newModel = { model with batch = newBatch }
        ReportMergeFailure batch, newModel
    | _, _ ->
        IngestMergeResult.NoOp, model

type UpdatePullRequestResult =
    | NoOp
    | AbortRunningBatch of Batch * PullRequestID
    | AbortMergingBatch of Batch * PullRequestID

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
            AbortRunningBatch(batch, id), newModel

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
            // fast fail the current batch, an unsafe PR could be about to merge into target
            let newQueue = modelWithNewSinBin.queue |> removeAllFromQueue batch

            let newModel =
                { modelWithNewSinBin with
                      queue = newQueue
                      batch = NoBatch }

            AbortMergingBatch(batch, id), newModel
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
    model.queue |> List.map (fun ((PassingPullRequest pr), _) -> pr)

let peekCurrentBatch (model: MergeQueue): Option<Batch> =
    match model.batch with
    | NoBatch -> None
    | Running batch -> Some batch
    | Merging batch -> Some batch

let peekSinBin (model: MergeQueue): List<PullRequest> =
    model.sinBin |> List.map (fun (NaughtyPullRequest pr) -> pr)

let previewExecutionPlan (model: MergeQueue): ExecutionPlan =
    let rec splitIntoBatches (queue: AttemptQueue): List<PlannedBatch> =
        match queue with
        | [] ->
            []
        | _ ->
            let nextBatch = pickNextBatch queue
            match nextBatch with
            | Some batch ->
                let batchIds = batch |> List.map (fun pr -> pr.id)
                let remainder = removeAllFromQueue batch queue
                PlannedBatch batchIds :: (splitIntoBatches remainder)
            | None ->
                // SMELL: impossible code path, all non-empty queues have a next batch...
                // SMELL: how could execution get here and result is empty?
                []

    let current =
        match model.batch with
        | NoBatch -> None
        | Running batch -> Some batch
        | Merging batch -> Some batch

    let remainder =
        match current with
        | None -> model.queue
        | Some batch -> removeAllFromQueue batch model.queue

    match current with
    | None -> remainder |> splitIntoBatches
    | Some batch ->
        let batchIds = batch |> List.map (fun pr -> pr.id)
        PlannedBatch batchIds :: (remainder |> splitIntoBatches)
