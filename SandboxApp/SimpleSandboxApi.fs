module MergeQueue.SimpleSandboxApi

open Suave
open Suave.Operators
open Newtonsoft.Json
open Newtonsoft.Json.Serialization
open MergeQueue.DomainTypes
open MergeQueue.Domain
open MergeQueue.DbTypes

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
        |> List.map (fun pr -> { id = PullRequestID.getValue pr.id })

    let current =
        state
        |> peekCurrentBatch
        |> Option.map (fun batch -> batch |> List.map (fun pr -> { id = PullRequestID.getValue pr.id }))

    let plan =
        state
        |> previewExecutionPlan
        |> List.map (fun batch ->
            batch
            |> PlannedBatch.toPullRequestIds
            |> List.map PullRequestID.getValue
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
    let result, state =
        load()
        |> Domain.enqueue
            (PullRequest.pullRequest (PullRequestID.create id) (SHA.create "00001234")
                 [ CommitStatus.create "circleci" CommitStatusState.Success ])

    save state

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
    let result, state =
        load()
        |> Domain.enqueue
            (PullRequest.pullRequest (PullRequestID.create id) (SHA.create "00001234")
                 [ CommitStatus.create "circleci" CommitStatusState.Pending ])

    save state

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
    let result, state =
        load() |> Domain.dequeue (PullRequestID.create id)

    save state

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
    let result, state =
        load() |> startBatch

    save state

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
    let result, state =
        load() |> Domain.ingestBuildUpdate (BuildMessage.Success(SHA.create "12345678"))

    let response =
        match result with
        | IngestBuildResult.NoOp -> "NoOp"
        | PerformBatchMerge _ -> "Batch finished"
        | ReportBuildFailureWithRetry _ -> "Batch failed"
        | ReportBuildFailureNoRetry _ -> "Batch failed"

    // HACK: do the merge if we should it...
    match result with
    | PerformBatchMerge _ ->
        let mergedState =
            state
            |> Domain.ingestMergeUpdate (MergeMessage.Success)
            |> snd

        save mergedState
    | _ ->
        save state

    response
    |> toJson
    |> Successful.OK
    >=> Writers.setHeader "Content-Type" "application/json"
