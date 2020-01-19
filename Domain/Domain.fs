module MergeQueue.Domain

// Models
type SHA = SHA of string

type PullRequestID = PullRequestID of int

type CommitStatusState =
    | Success
    | NoSuccess

type CommitStatus =
    { context: string
      state: CommitStatusState }

type CommitStatuses = List<CommitStatus>

type PullRequest =
    { id: PullRequestID
      sha: SHA
      statuses: CommitStatuses }

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

// misc
let private removeFromQueue (toRemove: List<PullRequest>) (queue: AttemptQueue): AttemptQueue =
    queue |> List.filter (fun (pr, _) -> List.contains pr toRemove |> not)

// Commands
type EnqueueResult =
    | Success
    | RejectedNeedAllStatusesSuccess
    | AlreadyEnqueued

let private passesBuild (pullRequest: PullRequest): bool =
    match pullRequest.statuses with
    | [] -> false
    | statuses ->
        statuses |> List.forall (fun s -> s.state = CommitStatusState.Success)

let enqueue (pullRequest: PullRequest) (MergeQueueState model): EnqueueResult * State =
    let passedBuild = pullRequest |> passesBuild

    let alreadyEnqueued =
        model.queue
        |> List.map fst
        |> List.contains pullRequest

    match passedBuild, alreadyEnqueued with
    | false, _ -> RejectedNeedAllStatusesSuccess, MergeQueueState model
    | _, true -> AlreadyEnqueued, MergeQueueState model
    | _, false -> Success, MergeQueueState { model with queue = model.queue @ [ pullRequest, [] ] }

type StartBatchResult =
    | PerformBatchBuild of List<PullRequest>
    | AlreadyRunning
    | EmptyQueue

let private selectBatch (queue: AttemptQueue): Batch =
    match queue with
    | [] -> failwith "Should not be called on an empty queue"
    | head :: tail ->
        let matching =
            tail |> List.filter (fun (_, a) -> a = (head |> snd))

        head :: matching |> List.map fst

let startBatch (MergeQueueState model): StartBatchResult * State =
    match model.batch, model.queue with
    | _, [] -> EmptyQueue, MergeQueueState model
    | Running _, _ -> AlreadyRunning, MergeQueueState model
    | Merging _, _ -> AlreadyRunning, MergeQueueState model
    | NoBatch, queue ->
        let batch = queue |> selectBatch
        PerformBatchBuild batch, MergeQueueState { model with batch = Running batch }

type BuildMessage =
    | Success of SHA
    | Failure

type IngestBuildResult =
    | NoOp
    | PerformBatchMerge of List<PullRequest> * SHA
    | BuildFailureWithRetry of List<PullRequest>
    | BuildFailureNoRetry of List<PullRequest>

let private bisect (batch: Batch): Option<Batch * Batch> =
    if List.length batch <= 1 then
        None
    else
        let midpoint = (List.length batch) / 2
        List.splitAt midpoint batch |> Some

let ingestBuildUpdate (message: BuildMessage) (MergeQueueState model): IngestBuildResult * State =
    match model.batch, message with
    | NoBatch, Failure ->
        NoOp, MergeQueueState { model with batch = NoBatch } // The record update doesn't change the value...
    | Running batch, Failure ->
        match bisect batch with
        | None ->
            // no way to bisect -> nothing more to attempt
            let queue = model.queue |> removeFromQueue batch
            BuildFailureNoRetry batch,
            MergeQueueState
                { model with
                      queue = queue
                      batch = NoBatch }
        | Some(first, second) ->
            // update attempts
            let newQueue =
                model.queue
                |> List.map (fun (pr, a) ->
                    if List.contains pr first then (pr, a @ [ true ])
                    elif List.contains pr second then (pr, a @ [ false ])
                    else pr, a)
            BuildFailureWithRetry batch,
            MergeQueueState
                { model with
                      queue = newQueue
                      batch = NoBatch }

    | Merging _, Failure ->
        NoOp, MergeQueueState model
    | NoBatch, Success _ ->
        NoOp, MergeQueueState model
    | Running runningBatch, Success targetHead ->
        let result = PerformBatchMerge(runningBatch, targetHead)
        let state = MergeQueueState { model with batch = Merging runningBatch }
        (result, state)
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
    | Merging merging, MergeMessage.Success ->
        let newQueue = model.queue |> removeFromQueue merging

        let state =
            MergeQueueState
                { model with
                      queue = newQueue
                      batch = NoBatch }

        MergeComplete merging, state
    | Merging batch, MergeMessage.Failure ->
        let state = MergeQueueState { model with batch = NoBatch }
        ReportMergeFailure batch, state
    | _, _ ->
        IngestMergeResult.NoOp, MergeQueueState model

type UpdatePullRequestResult =
    | NoOp
    | AbortRunningBatch of Batch * PullRequestID
    | AbortMergingBatch of Batch * PullRequestID

let private updateShaForEnqueuedPr (newValue: SHA) (id: PullRequestID) (model: MergeQueueModel): AttemptQueue =
    // remove offending PR for now
    let updateCorrespondingSha (pr, a) =
        if pr.id = id then { pr with sha = newValue }, a
        else pr, a

    model.queue |> List.map updateCorrespondingSha

let private movePullRequestToSinBin (id: PullRequestID) (newValue: SHA) (MergeQueueState model): AttemptQueue * SinBin =
    // get PR (and update SHA)
    let updatedPr =
        model.queue
        |> List.tryFind (fun (pr, _) -> pr.id = id)
        |> Option.map (fun (pr, _) -> { pr with sha = newValue })

    // move to the sin bin
    let newSinBin =
        match updatedPr with
        | None -> model.sinBin
        | Some item -> model.sinBin @ [ item ]

    // from the queue
    let newQueue =
        model.queue |> List.filter (fun (pr, _) -> pr.id <> id)

    newQueue, newSinBin

let updatePullRequestSha (id: PullRequestID) (newValue: SHA) (MergeQueueState model): UpdatePullRequestResult * State =
    // update SHAs in sin bin first
    let newSinBin =
        model.sinBin
        |> List.map (fun pr ->
            if pr.id = id then { pr with sha = newValue }
            else pr)

    let modelWithNewSinBin = { model with sinBin = newSinBin }

    match modelWithNewSinBin.batch with
    | Running batch ->
        let inRunningBatch =
            batch
            |> List.map (fun pr -> pr.id)
            |> List.contains id

        if inRunningBatch then
            let newQueue, newSinBin = movePullRequestToSinBin id newValue (MergeQueueState modelWithNewSinBin)
            AbortRunningBatch(batch, id),
            MergeQueueState
                { modelWithNewSinBin with
                      queue = newQueue
                      batch = NoBatch
                      sinBin = newSinBin }
        else
            let newQueue, newSinBin = movePullRequestToSinBin id newValue (MergeQueueState modelWithNewSinBin)

            let newModel =
                { modelWithNewSinBin with
                      queue = newQueue
                      sinBin = newSinBin }
            NoOp, MergeQueueState newModel
    | NoBatch ->
        let newQueue, newSinBin = movePullRequestToSinBin id newValue (MergeQueueState modelWithNewSinBin)
        NoOp,
        MergeQueueState
            { modelWithNewSinBin with
                  queue = newQueue
                  sinBin = newSinBin }
    | Merging batch ->
        let inMergingBatch =
            batch
            |> List.map (fun pr -> pr.id)
            |> List.contains id

        if inMergingBatch then
            // fast fail the current batch, an unsafe PR could be about to merge into target
            let newQueue = modelWithNewSinBin.queue |> List.filter (fun (pr, _) -> List.contains pr batch |> not)
            AbortMergingBatch(batch, id),
            MergeQueueState
                { modelWithNewSinBin with
                      queue = newQueue
                      batch = NoBatch }
        else
            let newQueue, newSinBin = movePullRequestToSinBin id newValue (MergeQueueState modelWithNewSinBin)

            let newModel =
                { modelWithNewSinBin with
                      queue = newQueue
                      sinBin = newSinBin }
            NoOp, MergeQueueState newModel

let updateStatuses (id: PullRequestID) (buildSha: SHA) (statuses: CommitStatuses) (MergeQueueState model): State =
    // check to see if we should pull the matching commit out of the "sin bin"
    let matching =
        model.sinBin |> List.tryFind (fun pr -> pr.id = id && pr.sha = buildSha)

    match matching with
    | Some pr ->
        // update PR's status
        let updated = { pr with statuses = statuses }
        let updatedSinBin =
            model.sinBin |> List.where (fun p -> p <> pr)

        match passesBuild updated with
        | true ->
            let newQueue = model.queue @ [ updated, [] ]
            MergeQueueState
                { model with
                      queue = newQueue
                      sinBin = updatedSinBin }
        | false ->
            let newSinBin = updatedSinBin @ [ updated ]
            MergeQueueState { model with sinBin = newSinBin }
    | None ->
        MergeQueueState model

// "Properties"
// Should these be DTOs?
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
            let batch = selectBatch queue
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
