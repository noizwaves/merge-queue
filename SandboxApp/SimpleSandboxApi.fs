module MergeQueue.SimpleSandboxApi

open Suave
open Suave.Operators
open Newtonsoft.Json
open Newtonsoft.Json.Serialization
open MergeQueue.DomainTypes
open MergeQueue.Domain
open MergeQueue.DbTypes
open MergeQueue.Commands
open MergeQueue.Commands.Enqueue
open MergeQueue.Commands.Dequeue
open MergeQueue.Commands.StartBatch
open MergeQueue.Commands.IngestBuild
open MergeQueue.Commands.IngestMerge

let private toJson v =
    let jsonSerializerSettings = JsonSerializerSettings()
    jsonSerializerSettings.ContractResolver <- CamelCasePropertyNamesContractResolver()
    JsonConvert.SerializeObject(v, jsonSerializerSettings)

type PullRequestDTO =
    { number: int }

type BatchDTO = List<PullRequestDTO>

type PlanDTO = List<BatchDTO>

type ViewMergeQueueDTO =
    { current: Option<BatchDTO>
      plan: PlanDTO
      sinBin: List<PullRequestDTO> }

let view (load: Load) _request: WebPart =
    let state = load()

    let sinBin =
        state
        |> peekSinBin
        |> List.map (fun pr -> { number = pr.number |> PullRequestNumber.value })

    let current =
        state
        |> peekCurrentBatch
        |> Option.map (fun batch -> batch |> List.map (fun pr -> { number = pr.number |> PullRequestNumber.value }))

    let plan =
        state
        |> previewExecutionPlan
        |> List.map (fun batch ->
            batch
            |> PlannedBatch.toPullRequestIds
            |> List.map (fun number -> { number = number |> PullRequestNumber.value }))

    let dto =
        { sinBin = sinBin
          current = current
          plan = plan }

    dto
    |> toJson
    |> Successful.OK
    >=> Writers.setHeader "Content-Type" "application/json"

let enqueue (load: Load) (save: Save) id: WebPart =
    let enqueue' = enqueue load save

    // TODO: Turn the imperative expressions into a >=> pipe
    let cmd =
        { number = id
          sha = "00001234"
          statuses = [ "circleci", "Success" ] }

    let result = enqueue' cmd

    let response =
        match result with
        | Ok(Success.Enqueued) -> "Enqueued"
        | Ok(Success.SinBinned) -> "Sin binned"
        | Error(Error.EnqueueError EnqueueError.RejectedFailingBuildStatus) ->
            "Rejected (failing build status)"
        | Error(Error.EnqueueError EnqueueError.AlreadyEnqueued) -> "Already enqueued"
        | Error(Enqueue.Error.ValidationError help) -> sprintf "Validation error: %s" help

    response
    |> toJson
    |> Successful.OK
    >=> Writers.setHeader "Content-Type" "application/json"

let fireAndForget (load: Load) (save: Save) id: WebPart =
    let enqueue' = Enqueue.enqueue load save

    let cmd =
        { number = id
          sha = "00001234"
          statuses = [ "circleci", "Pending" ] }

    let result = enqueue' cmd

    let response =
        match result with
        | Ok(Success.Enqueued) -> "Enqueued"
        | Ok(Success.SinBinned) -> "Sin binned"
        | Error(Enqueue.Error.EnqueueError EnqueueError.RejectedFailingBuildStatus) ->
            "Rejected (failing build status)"
        | Error(Enqueue.Error.EnqueueError EnqueueError.AlreadyEnqueued) -> "Already enqueued"
        | Error(Enqueue.Error.ValidationError help) -> sprintf "Validation error: %s" help

    response
    |> toJson
    |> Successful.OK
    >=> Writers.setHeader "Content-Type" "application/json"

let dequeue (load: Load) (save: Save) id: WebPart =
    let dequeue' = dequeue load save
    let cmd: Dequeue.Command = { number = id }

    let result = dequeue' cmd

    let response =
        match result with
        | Ok(Success.Dequeued) -> "Dequeued"
        | Ok(Success.DequeuedAndAbortRunningBatch _) -> "Dequeued (a running batch was cancelled)"
        | Error(Dequeue.Error.DequeueError DequeueError.RejectedInMergingBatch) -> "Rejected (in a merging batch)"
        | Error(Dequeue.Error.DequeueError DequeueError.NotFound) -> "Not found"
        | Error(Dequeue.Error.ValidationError help) -> sprintf "Validation error: %s" help

    response
    |> toJson
    |> Successful.OK
    >=> Writers.setHeader "Content-Type" "application/json"

let start (load: Load) (save: Save) _request: WebPart =
    let startBatch' = startBatch load save
    let cmd = ()

    let result = startBatch' cmd

    let response =
        match result with
        | Ok (PerformBatchBuild _) -> "Starting batch build"
        | Error (StartBatchError AlreadyRunning) -> "A batch is already running"
        | Error (StartBatchError EmptyQueue) -> "Queue is empty, no batch to start"

    response
    |> toJson
    |> Successful.OK
    >=> Writers.setHeader "Content-Type" "application/json"

let finish (load: Load) (save: Save) _request: WebPart =
    let ingestBuildUpdate' = ingestBuildUpdate load save
    let ingestMergeUpdate' = ingestMergeUpdate load save

    let result = ingestBuildUpdate' { message = BuildMessage.Success "12345678" }

    let response =
        match result with
        | Ok IngestBuildSuccess.NoChange -> "NoOp"
        | Ok (PerformBatchMerge _) ->
            // HACK: do the merge if we should it...
            ingestMergeUpdate' { message = (MergeMessage.Success) } |> ignore
            "Batch finished"
        | Ok (ReportBuildFailureWithRetry _) -> "Batch failed"
        | Ok (ReportBuildFailureNoRetry _) -> "Batch failed"
        | Error (IngestBuild.Error.ValidationError help)  -> sprintf "Validation error: %s" help

    response
    |> toJson
    |> Successful.OK
    >=> Writers.setHeader "Content-Type" "application/json"
