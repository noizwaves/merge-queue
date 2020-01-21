module MergeQueue.SimpleSandboxApi

open Suave
open Suave.Operators
open MergeQueue.Domain
open Newtonsoft.Json
open Newtonsoft.Json.Serialization


let private toJson v =
    let jsonSerializerSettings = JsonSerializerSettings()
    jsonSerializerSettings.ContractResolver <- CamelCasePropertyNamesContractResolver()
    JsonConvert.SerializeObject(v, jsonSerializerSettings)

type GetState = unit -> State

type UpdateState = State -> unit

type PullRequestDTO =
    { id: int }

type BatchDTO = List<PullRequestDTO>

type PlanDTO = List<BatchDTO>

type ViewMergeQueueDTO =
    { queue: List<PullRequestDTO>
      sinBin: List<PullRequestDTO>
      current: Option<BatchDTO>
      plan: PlanDTO }

let view (fetch: GetState) _request: WebPart =
    let state = fetch()

    let queue =
        state
        |> peekCurrentQueue
        |> List.map (fun pr -> { id = getPullRequestIDValue pr.id })

    let sinBin =
        state
        |> peekSinBin
        |> List.map (fun pr -> { id = getPullRequestIDValue pr.id })

    let current =
        state
        |> peekCurrentBatch
        |> Option.map (fun batch -> batch |> List.map (fun pr -> { id = getPullRequestIDValue pr.id }))

    let plan =
        state
        |> previewExecutionPlan
        |> List.map (fun batch -> batch |> List.map (fun pr -> { id = getPullRequestIDValue pr.id }))

    let dto =
        { queue = queue
          sinBin = sinBin
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
            (pullRequest (pullRequestId id) (sha "00001234") [ commitStatus "circleci" CommitStatusState.Success ])

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
            (pullRequest (pullRequestId id) (sha "00001234") [ commitStatus "circleci" CommitStatusState.Pending ])

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
        fetch() |> Domain.dequeue (pullRequestId id)

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
        fetch() |> Domain.ingestBuildUpdate (BuildMessage.Success(sha "12345678"))

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
