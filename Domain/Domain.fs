module MergeQueue.Domain

// Models
type SHA = SHA of string

type PullRequestID = PullRequestID of int

// Are CommitStatusState and CommitStatus words in our domain? Or borrowed from GitHub's API...
type CommitStatusState =
    | Pending
    | Success
    | Failure

type CommitStatus =
    { context: string
      state: CommitStatusState }

type CommitStatuses = List<CommitStatus>

type PullRequest =
    { id: PullRequestID
      sha: SHA
      statuses: CommitStatuses }

type BuildStatus =
    | Pending
    | Success
    | Failure

type Batch = List<PullRequest>

type private CurrentBatch =
    | NoBatch
    | Running of Batch
    | Merging of Batch

type private BisectPath = List<bool>

type private AttemptQueue = List<PullRequest * BisectPath>

type private SinBin = List<PullRequest>

type private MergeQueueModel =
    { queue: AttemptQueue
      sinBin: SinBin
      batch: CurrentBatch }

type State = private MergeQueueState of MergeQueueModel

type ExecutionPlan = List<Batch>

// Constructors
let emptyMergeQueue: State =
    MergeQueueState
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

// Domain Logic
let private removeFromQueue (toRemove: List<PullRequest>) (queue: AttemptQueue): AttemptQueue =
    let removeIds = toRemove |> List.map (fun pr -> pr.id)
    queue |> List.filter (fun (pr, _) -> List.contains pr.id removeIds |> not)

let private getBuildStatus (pullRequest: PullRequest): BuildStatus =
    match pullRequest.statuses with
    | [] -> BuildStatus.Failure
    | statuses ->
        let anyFailures = statuses |> List.tryFind (fun s -> s.state = CommitStatusState.Failure)
        let anyPending = statuses |> List.tryFind (fun s -> s.state = CommitStatusState.Pending)

        match anyFailures, anyPending with
        | Some _, _ -> BuildStatus.Failure
        | _, Some _ -> BuildStatus.Pending
        | _, _ -> BuildStatus.Success

let private enqueuePullRequest (pullRequest: PullRequest) (queue: AttemptQueue): AttemptQueue =
    queue @ [ pullRequest, [] ]

let private sinBinPullRequest (pullRequest: PullRequest) (sinBin: SinBin): SinBin =
    sinBin @ [ pullRequest ]

let private pickNextBatch (queue: AttemptQueue): Batch =
    match queue with
    | [] -> failwith "Should not be called on an empty queue"
    | head :: tail ->
        let matching =
            tail |> List.filter (fun (_, a) -> a = (head |> snd))

        head :: matching |> List.map fst

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

let private bisect (batch: Batch): Option<Batch * Batch> =
    if List.length batch <= 1 then
        None
    else
        let midpoint = (List.length batch) / 2
        List.splitAt midpoint batch |> Some

let private completeBuild (batch: Batch): CurrentBatch =
    Merging batch

let private failWithoutRetry (batch: Batch) (queue: AttemptQueue) =
    let newQueue = queue |> removeFromQueue batch
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
    let newQueue = queue |> removeFromQueue batch
    newQueue, NoBatch

let private failMerge (_batch: Batch): CurrentBatch =
    NoBatch

let private movePullRequestToSinBin (id: PullRequestID) (newValue: SHA) (queue: AttemptQueue) (sinBin: SinBin): AttemptQueue * SinBin =
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

// Note: these signatures are huge! Why?
let private updateShaInQueueWhenBatchRunning (id: PullRequestID) (newValue: SHA) (batch: Batch) (queue: AttemptQueue)
    (sinBin: SinBin): bool * AttemptQueue * SinBin =
    let inRunningBatch = batch |> inBatch id

    if inRunningBatch then
        let newQueue, newSinBin =
            movePullRequestToSinBin id newValue queue sinBin

        true, newQueue, newSinBin

    else
        let newQueue, newSinBin =
            movePullRequestToSinBin id newValue queue sinBin

        false, newQueue, newSinBin

// Note: these signatures are huge! Why?
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
        | BuildStatus.Success ->
            let newQueue = queue @ [ updated, [] ]
            newQueue, updatedSinBin
        | BuildStatus.Failure ->
            let newSinBin = updatedSinBin @ [ updated ]
            queue, newSinBin
        | BuildStatus.Pending ->
            let newSinBin = updatedSinBin @ [ updated ]
            queue, newSinBin
    | None ->
        queue, sinBin


// Commands
type EnqueueResult =
    | Enqueued
    | SinBinned
    | RejectedNeedAllStatusesSuccess
    | AlreadyEnqueued

let enqueue (pullRequest: PullRequest) (MergeQueueState model): EnqueueResult * State =
    let buildStatus = getBuildStatus pullRequest
    let alreadyEnqueued = model.queue |> inQueue pullRequest.id
    let alreadySinBinned = model.sinBin |> inSinBin pullRequest.id

    match buildStatus, alreadyEnqueued, alreadySinBinned with
    | BuildStatus.Failure, _, _ -> RejectedNeedAllStatusesSuccess, MergeQueueState model
    | _, true, _ -> AlreadyEnqueued, MergeQueueState model
    | _, _, true -> AlreadyEnqueued, MergeQueueState model
    | BuildStatus.Pending, false, false ->
        let newModel = { model with sinBin = sinBinPullRequest pullRequest model.sinBin }
        SinBinned, MergeQueueState newModel
    | BuildStatus.Success, false, false ->
        let newModel = { model with queue = enqueuePullRequest pullRequest model.queue }
        Enqueued, MergeQueueState newModel

type StartBatchResult =
    | PerformBatchBuild of List<PullRequest>
    | AlreadyRunning
    | EmptyQueue

let startBatch (MergeQueueState model): StartBatchResult * State =
    match model.batch, model.queue with
    | _, [] -> EmptyQueue, MergeQueueState model
    | Running _, _ -> AlreadyRunning, MergeQueueState model
    | Merging _, _ -> AlreadyRunning, MergeQueueState model
    | NoBatch, queue ->
        let batch = queue |> pickNextBatch
        PerformBatchBuild batch, MergeQueueState { model with batch = Running batch }

type BuildMessage =
    | Success of SHA
    | Failure

type IngestBuildResult =
    | NoOp
    | PerformBatchMerge of List<PullRequest> * SHA
    | ReportBuildFailureWithRetry of List<PullRequest>
    | ReportBuildFailureNoRetry of List<PullRequest>

let ingestBuildUpdate (message: BuildMessage) (MergeQueueState model): IngestBuildResult * State =
    match model.batch, message with
    | Running batch, Failure ->
        match bisect batch with
        | None ->
            let newQueue, newBatch = model.queue |> failWithoutRetry batch

            let newModel =
                { model with
                      queue = newQueue
                      batch = newBatch }
            ReportBuildFailureNoRetry batch, MergeQueueState newModel

        | Some(first, second) ->
            let newQueue, newBatch = model.queue |> failWithRetry first second

            let newModel =
                { model with
                      queue = newQueue
                      batch = newBatch }
            ReportBuildFailureWithRetry batch, MergeQueueState newModel

    | Running succeeded, Success targetHead ->
        let newBatch = completeBuild succeeded
        let newState = MergeQueueState { model with batch = newBatch }
        let result = PerformBatchMerge(succeeded, targetHead)
        result, newState

    | NoBatch, Failure ->
        NoOp, MergeQueueState model
    | Merging _, Failure ->
        NoOp, MergeQueueState model
    | NoBatch, Success _ ->
        NoOp, MergeQueueState model
    | Merging _, Success _ ->
        NoOp, MergeQueueState model

type MergeMessage =
    | Success
    | Failure

type IngestMergeResult =
    | NoOp
    | MergeComplete of List<PullRequest>
    | ReportMergeFailure of List<PullRequest>

let ingestMergeUpdate (message: MergeMessage) (MergeQueueState model): IngestMergeResult * State =
    match model.batch, message with
    | Merging merged, MergeMessage.Success ->
        let newQueue, newBatch = model.queue |> completeMerge merged

        let newModel =
            { model with
                  queue = newQueue
                  batch = newBatch }

        MergeComplete merged, MergeQueueState newModel
    | Merging batch, MergeMessage.Failure ->
        let newBatch = failMerge batch
        let newModel = { model with batch = newBatch }
        ReportMergeFailure batch, MergeQueueState newModel
    | _, _ ->
        IngestMergeResult.NoOp, MergeQueueState model

type UpdatePullRequestResult =
    | NoOp
    | AbortRunningBatch of Batch * PullRequestID
    | AbortMergingBatch of Batch * PullRequestID

let updatePullRequestSha (id: PullRequestID) (newValue: SHA) (MergeQueueState model): UpdatePullRequestResult * State =
    let newSinBin = model.sinBin |> updateShaInSinBin id newValue
    let modelWithNewSinBin = { model with sinBin = newSinBin }

    match modelWithNewSinBin.batch with
    | Running batch ->
        let abortRunningBatch, newQueue, newSinBin =
            updateShaInQueueWhenBatchRunning id newValue batch modelWithNewSinBin.queue modelWithNewSinBin.sinBin

        if abortRunningBatch then
            let newModel =
                { modelWithNewSinBin with
                      queue = newQueue
                      batch = NoBatch
                      sinBin = newSinBin }
            AbortRunningBatch(batch, id), MergeQueueState newModel

        else
            let newModel =
                { modelWithNewSinBin with
                      queue = newQueue
                      sinBin = newSinBin }
            NoOp, MergeQueueState newModel
    | NoBatch ->
        let newQueue, newSinBin =
            movePullRequestToSinBin id newValue modelWithNewSinBin.queue modelWithNewSinBin.sinBin

        let newModel =
            { modelWithNewSinBin with
                  queue = newQueue
                  sinBin = newSinBin }
        NoOp, MergeQueueState newModel

    | Merging batch ->
        let inMergingBatch = batch |> inBatch id

        if inMergingBatch then
            // fast fail the current batch, an unsafe PR could be about to merge into target
            let newQueue = modelWithNewSinBin.queue |> removeFromQueue batch

            let newModel =
                { modelWithNewSinBin with
                      queue = newQueue
                      batch = NoBatch }

            AbortMergingBatch(batch, id), MergeQueueState newModel
        else
            let newQueue, newSinBin =
                movePullRequestToSinBin id newValue modelWithNewSinBin.queue modelWithNewSinBin.sinBin

            let newModel =
                { modelWithNewSinBin with
                      queue = newQueue
                      sinBin = newSinBin }
            NoOp, MergeQueueState newModel

let updateStatuses (id: PullRequestID) (buildSha: SHA) (statuses: CommitStatuses) (MergeQueueState model): State =
    // check to see if we should pull the matching commit out of the "sin bin"
    let newQueue, newSinBin =
        updateStatusesInSinBin id buildSha statuses model.queue model.sinBin

    MergeQueueState
        { model with
              queue = newQueue
              sinBin = newSinBin }

// "Properties"

// Should these return DTOs?
let peekCurrentQueue (MergeQueueState model): List<PullRequest> =
    model.queue |> List.map fst

let peekCurrentBatch (MergeQueueState model): Option<Batch> =
    match model.batch with
    | NoBatch -> None
    | Running batch -> Some batch
    | Merging batch -> Some batch

let peekSinBin (MergeQueueState model): SinBin =
    model.sinBin

let previewBatches (MergeQueueState model): ExecutionPlan =
    let rec splitIntoBatches (queue: AttemptQueue): List<Batch> =
        match queue with
        | [] ->
            []
        | _ ->
            let batch = pickNextBatch queue
            let remainder = removeFromQueue batch queue
            batch :: (splitIntoBatches remainder)

    let current =
        match model.batch with
        | NoBatch -> None
        | Running batch -> Some batch
        | Merging batch -> Some batch

    let remainder =
        match current with
        | None -> model.queue
        | Some batch -> removeFromQueue batch model.queue

    match current with
    | None -> remainder |> splitIntoBatches
    | Some batch -> batch :: (remainder |> splitIntoBatches)
