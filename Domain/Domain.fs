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
    queue |> List.filter (fun (pr, _) -> List.contains pr.id removeIds |> not)

let private inQueue (id: PullRequestID) (queue: AttemptQueue): bool =
    queue
    |> List.map fst
    |> List.map (fun pr -> pr.id)
    |> List.contains id

let private inSinBin (id: PullRequestID) (sinBin: SinBin): bool =
    sinBin
    |> List.map (fun pr -> pr.id)
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
    queue |> List.filter (fun (pr, _) -> pr.id <> id)

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

let addPullRequestToQueue (pullRequest: PullRequest) (queue: AttemptQueue): AttemptQueue =
    queue @ [ pullRequest, [] ]

let addPullRequestToSinBin (pullRequest: PullRequest) (sinBin: SinBin): SinBin =
    sinBin @ [ pullRequest ]

let pickNextBatch (queue: AttemptQueue): Batch =
    match queue with
    | [] -> failwith "Should not be called on an empty queue"
    | head :: tail ->
        let matching =
            tail |> List.filter (fun (_, a) -> a = (head |> snd))

        head :: matching |> List.map fst

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

let private failWithRetry (first: Batch) (second: Batch) (queue: AttemptQueue) =
    let firstIds = first |> List.map (fun pr -> pr.id)
    let secondIds = second |> List.map (fun pr -> pr.id)

    let newQueue =
        queue
        |> List.map (fun (pr, a) ->
            if List.contains pr.id firstIds then (pr, a @ [ true ])
            elif List.contains pr.id secondIds then (pr, a @ [ false ])
            else pr, a)
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
        |> List.tryFind (fun (pr, _) -> pr.id = id)
        |> Option.map (fun (pr, _) -> { pr with sha = newValue })

    // move to the sin bin
    let newSinBin =
        match updatedPr with
        | None -> sinBin
        | Some item -> sinBin @ [ item ]

    // from the queue
    let newQueue =
        queue |> List.filter (fun (pr, _) -> pr.id <> id)

    newQueue, newSinBin

let private updateShaInSinBin (id: PullRequestID) (newValue: SHA) (sinBin: SinBin): SinBin =
    sinBin
    |> List.map (fun pr ->
        if pr.id = id then { pr with sha = newValue }
        else pr)

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
        sinBin |> List.tryFind (fun pr -> pr.id = id && pr.sha = buildSha)

    match matching with
    | Some pr ->
        // update PR's status
        let updated = { pr with statuses = statuses }
        let updatedSinBin =
            sinBin |> List.where (fun p -> p <> pr)

        match getBuildStatus updated with
        | BuildSuccess ->
            let newQueue = queue @ [ updated, [] ]
            newQueue, updatedSinBin
        | BuildFailure ->
            let newSinBin = updatedSinBin @ [ updated ]
            queue, newSinBin
        | BuildPending ->
            let newSinBin = updatedSinBin @ [ updated ]
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
    let buildStatus = getBuildStatus pullRequest
    let alreadyEnqueued = model.queue |> inQueue pullRequest.id
    let alreadySinBinned = model.sinBin |> inSinBin pullRequest.id

    match buildStatus, alreadyEnqueued, alreadySinBinned with
    | BuildFailure, _, _ -> RejectedFailingBuildStatus, model
    | _, true, _ -> AlreadyEnqueued, model
    | _, _, true -> AlreadyEnqueued, model
    | BuildPending, false, false ->
        let newModel = { model with sinBin = addPullRequestToSinBin pullRequest model.sinBin }
        SinBinned, newModel
    | BuildSuccess, false, false ->
        let newModel = { model with queue = addPullRequestToQueue pullRequest model.queue }
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
            let newSinBin = model.sinBin |> List.where (fun pr -> pr.id <> id)
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
        let batch = queue |> pickNextBatch
        PerformBatchBuild batch, { model with batch = Running batch }

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
    model.queue |> List.map fst

let peekCurrentBatch (model: MergeQueue): Option<Batch> =
    match model.batch with
    | NoBatch -> None
    | Running batch -> Some batch
    | Merging batch -> Some batch

let peekSinBin (model: MergeQueue): SinBin =
    model.sinBin

let previewExecutionPlan (model: MergeQueue): ExecutionPlan =
    let rec splitIntoBatches (queue: AttemptQueue): List<Batch> =
        match queue with
        | [] ->
            []
        | _ ->
            let batch = pickNextBatch queue
            let remainder = removeAllFromQueue batch queue
            batch :: (splitIntoBatches remainder)

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
    | Some batch -> batch :: (remainder |> splitIntoBatches)
