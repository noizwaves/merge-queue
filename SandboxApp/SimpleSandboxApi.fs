module MergeQueue.SimpleSandboxApi

open Suave
open Suave.Operators
open Newtonsoft.Json
open Newtonsoft.Json.Serialization
open MergeQueue.DomainTypes
open MergeQueue.Domain


let private toJson v =
    let jsonSerializerSettings = JsonSerializerSettings()
    jsonSerializerSettings.ContractResolver <- CamelCasePropertyNamesContractResolver()
    JsonConvert.SerializeObject(v, jsonSerializerSettings)

type GetState = unit -> MergeQueue

type UpdateState = MergeQueue -> unit

type PullRequestDTO =
    { id: int }

type BatchDTO = List<PullRequestDTO>

type PlanDTO = List<BatchDTO>

type ViewMergeQueueDTO =
    { current: Option<BatchDTO>
      plan: PlanDTO
      sinBin: List<PullRequestDTO> }

let view (fetch: GetState) _request: WebPart =
    let state = fetch()

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

let enqueue (fetch: GetState) (store: UpdateState) id: WebPart =
    let result, state =
        fetch()
        |> Domain.enqueue
            (PullRequest.pullRequest (PullRequestID.create id) (SHA.create "00001234") [ CommitStatus.create "circleci" CommitStatusState.Success ])

    store state

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

let fireAndForget (fetch: GetState) (store: UpdateState) id: WebPart =
    let result, state =
        fetch()
        |> Domain.enqueue
            (PullRequest.pullRequest (PullRequestID.create id) (SHA.create "00001234") [ CommitStatus.create "circleci" CommitStatusState.Pending ])

    store state

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

let dequeue (fetch: GetState) (store: UpdateState) id: WebPart =
    let result, state =
        fetch() |> Domain.dequeue (PullRequestID.create id)

    store state

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

let start (fetch: GetState) (store: UpdateState) _request: WebPart =
    let result, state =
        fetch() |> startBatch

    store state

    let response =
        match result with
        | PerformBatchBuild _ -> "Starting batch build"
        | AlreadyRunning -> "A batch is already running"
        | EmptyQueue -> "Queue is empty, no batch to start"

    response
    |> toJson
    |> Successful.OK
    >=> Writers.setHeader "Content-Type" "application/json"

let finish (fetch: GetState) (store: UpdateState) _request: WebPart =
    let result, state =
        fetch() |> Domain.ingestBuildUpdate (BuildMessage.Success(SHA.create "12345678"))

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

        store mergedState
    | _ ->
        store state

    response
    |> toJson
    |> Successful.OK
    >=> Writers.setHeader "Content-Type" "application/json"
