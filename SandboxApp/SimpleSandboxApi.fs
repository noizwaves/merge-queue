module MergeQueue.SimpleSandboxApi

open Suave
open Suave.Operators
open Newtonsoft.Json
open Newtonsoft.Json.Serialization
open MergeQueue.DomainTypes
open MergeQueue.Domain
open MergeQueue.DbTypes
open MergeQueue.Commands

let private toJson v =
    let jsonSerializerSettings = JsonSerializerSettings()
    jsonSerializerSettings.ContractResolver <- CamelCasePropertyNamesContractResolver()
    JsonConvert.SerializeObject(v, jsonSerializerSettings)

type PullRequestDTO =
    { id: int }

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
        |> List.map (fun pr -> { id = PullRequestID.value pr.id })

    let current =
        state
        |> peekCurrentBatch
        |> Option.map (fun batch -> batch |> List.map (fun pr -> { id = PullRequestID.value pr.id }))

    let plan =
        state
        |> previewExecutionPlan
        |> List.map (fun batch ->
            batch
            |> PlannedBatch.toPullRequestIds
            |> List.map PullRequestID.value
            |> List.map (fun id -> { id = id }))

    let dto =
        { sinBin = sinBin
          current = current
          plan = plan }

    dto
    |> toJson
    |> Successful.OK
    >=> Writers.setHeader "Content-Type" "application/json"

let enqueue (load: Load) (save: Save) id: WebPart =
    let enqueue' = Commands.enqueue load save
    let cmd = { number = id; sha = "00001234"; statuses = [ "circleci", "Success" ] }

    let result = enqueue' cmd

    let response =
        match result with
        | Enqueued -> "Enqueued"
        | SinBinned -> "Sin binned"
        | RejectedFailingBuildStatus -> "Rejected (failing build status)"
        | AlreadyEnqueued -> "Already enqueued"

    response
    |> toJson
    |> Successful.OK
    >=> Writers.setHeader "Content-Type" "application/json"

let fireAndForget (load: Load) (save: Save) id: WebPart =
    let enqueue' = Commands.enqueue load save
    let cmd = { number = id; sha = "00001234"; statuses = [ "circleci", "Pending" ]}

    let result = enqueue' cmd

    let response =
        match result with
        | Enqueued -> "Enqueued"
        | SinBinned -> "Sin binned"
        | RejectedFailingBuildStatus -> "Rejected (failing build status)"
        | AlreadyEnqueued -> "Already enqueued"

    response
    |> toJson
    |> Successful.OK
    >=> Writers.setHeader "Content-Type" "application/json"

let dequeue (load: Load) (save: Save) id: WebPart =
    let dequeue' = Commands.dequeue load save
    let result = dequeue' (PullRequestID.create id)

    let response =
        match result with
        | Dequeued -> "Dequeued"
        | DequeuedAndAbortRunningBatch _ -> "Dequeued (a running batch was cancelled)"
        | RejectedInMergingBatch -> "Rejected (in a merging batch)"
        | NotFound -> "Not found"

    response
    |> toJson
    |> Successful.OK
    >=> Writers.setHeader "Content-Type" "application/json"

let start (load: Load) (save: Save) _request: WebPart =
    let startBatch' = startBatch load save
    let result = startBatch'()

    let response =
        match result with
        | PerformBatchBuild _ -> "Starting batch build"
        | AlreadyRunning -> "A batch is already running"
        | EmptyQueue -> "Queue is empty, no batch to start"

    response
    |> toJson
    |> Successful.OK
    >=> Writers.setHeader "Content-Type" "application/json"

let finish (load: Load) (save: Save) _request: WebPart =
    let ingestBuildUpdate' = Commands.ingestBuildUpdate load save
    let ingestMergeUpdate' = Commands.ingestMergeUpdate load save

    let result = ingestBuildUpdate' (BuildMessage.Success(SHA.create "12345678"))

    let response =
        match result with
        | IngestBuildResult.NoOp -> "NoOp"
        | PerformBatchMerge _ ->
            // HACK: do the merge if we should it...
            ingestMergeUpdate' (MergeMessage.Success) |> ignore
            "Batch finished"
        | ReportBuildFailureWithRetry _ -> "Batch failed"
        | ReportBuildFailureNoRetry _ -> "Batch failed"

    response
    |> toJson
    |> Successful.OK
    >=> Writers.setHeader "Content-Type" "application/json"
