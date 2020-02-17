module MergeQueue.Workflows

open MergeQueue.DomainTypes
open MergeQueue.DomainServiceTypes
open MergeQueue.Domain
open MergeQueue.DbTypes

let rec private consolidateResultList<'a, 'b> (results: List<Result<'a, 'b>>): Result<List<'a>, 'b> =
    match results with
    | [] ->
        Ok []
    | Error b :: tail ->
        Error b
    | Ok a :: tail ->
        tail
        |> consolidateResultList
        |> Result.map (fun aa -> a :: aa)

// TODO: tidy this up with computation expressions and composition
// TODO: could this be inlined to Enqueue module?
let private toPullRequestDomain (number: int) (sha: string) (statuses: List<string * string>): Result<PullRequest, string> =
    let number' = PullRequestNumber.create number
    let sha' = SHA.create sha

    let statuses' =
        statuses
        |> List.map CommitStatus.create
        |> consolidateResultList

    match number', sha', statuses' with
    | Error err, _, _ -> Error err
    | _, Error err, _ -> Error err
    | _, _, Error err -> Error err
    | Ok number, Ok sha, Ok statuses -> Ok(PullRequest.create number sha statuses)

module Common =
    // Input is a func: a' -> Result<b', e'>
    // Produces a func: Result<a', e'> -> Result<b', e'>
    let bind func =
        fun result ->
            match result with
            | Ok value -> func value
            | Error err -> Error err

    let tee func =
        fun value ->
            func value |> ignore
            value

    let switch func =
        fun value -> func value |> Ok

    // Input is a func: unit -> a'
    // Produces a func: Result<b', e'> -> Result<a' * b', e'>
    let carrySecond func =
        fun result ->
            match result with
            | Ok value -> Ok(func(), value)
            | Error error -> Error error

    // Input is a func: a' -> b'
    // Produces: Result<c' * a', e'> -> Result<b', e'>
    let mapSecond func =
        Result.map (snd >> func)

    // Input is a func: a' -> b'
    // Produces: Result<a' * c', e'> -> Result<b', e'>
    let mapFirst func =
        Result.map (fst >> func)

module Enqueue =
    open WorkflowTypes.Enqueue

    /// Validation
    type private ValidatePullRequest = Command -> Result<PullRequest, string>

    let private validatePullRequest: ValidatePullRequest =
        fun command -> toPullRequestDomain command.number command.sha command.statuses

    /// Loading the MergeQueue
    type private LoadMergeQueue = PullRequest -> Command<PullRequest>

    let private loadMergeQueue (load: Load): LoadMergeQueue =
        fun command -> load() |> Command.create command

    /// Enqueue step
    type private EnqueueStep = DomainService<PullRequest, EnqueueSuccess, EnqueueError>

    let private enqueueStep: EnqueueStep =
        enqueue

    /// Save changes to the MergeQueue
    type private SaveMergeQueue = AggregateSuccess<EnqueueSuccess> -> unit

    // the final workflow
    let enqueue (load: Load) (save: Save): EnqueueWorkflow =
        let validatePullRequest = validatePullRequest >> (Result.mapError Error.ValidationError)
        let loadMergeQueue = loadMergeQueue load
        let enqueueStep = enqueueStep >> (Result.mapError Error.EnqueueError)
        let saveMergeQueue = AggregateSuccess.aggregate >> save

        fun command ->
            command
            |> validatePullRequest
            |> Result.map loadMergeQueue
            |> Common.bind enqueueStep
            |> Common.tee (Result.map saveMergeQueue)
            |> Result.map AggregateSuccess.success

module Dequeue =
    open WorkflowTypes.Dequeue

    /// Validation
    type private ValidateDequeue = Command -> Result<PullRequestNumber, string>

    let private validateDequeue: ValidateDequeue =
        fun command -> command.number |> PullRequestNumber.create

    /// Loading
    type private LoadMergeQueue = PullRequestNumber -> Command<PullRequestNumber>

    let private loadMergeQueue (load: Load): LoadMergeQueue =
        fun command -> load() |> Command.create command

    /// Dequeue step
    type private DequeueStep = DomainService<PullRequestNumber, DequeueSuccess, DequeueError>

    let private dequeueStep: DequeueStep =
        dequeue

    /// Save
    type private SaveMergeQueue = AggregateSuccess<DequeueSuccess> -> unit

    // the final workflow
    let dequeue (load: Load) (save: Save): DequeueWorkflow =
        let validateDequeue = validateDequeue >> Result.mapError ValidationError
        let loadMergeQueue = loadMergeQueue load
        let dequeueStep = dequeueStep >> Result.mapError DequeueError
        let saveMergeQueue: SaveMergeQueue = (AggregateSuccess.aggregate >> save)

        fun command ->
            command
            |> validateDequeue
            |> Result.map loadMergeQueue
            |> Common.bind dequeueStep
            |> Common.tee (Result.map saveMergeQueue)
            |> Result.map AggregateSuccess.success

module StartBatch =
    open WorkflowTypes.StartBatch
    // Implementation

    /// Load
    type private LoadMergeQueue = unit -> Command<unit>

    let private loadMergeQueue (load: Load): LoadMergeQueue =
        fun command -> load() |> Command.create command

    /// Start the batch
    type private StartBatchStep = DomainService<unit, StartBatchSuccess, StartBatchError>

    let private startBatchStep: StartBatchStep =
        startBatch

    /// Save
    type private SaveMergeQueue = AggregateSuccess<StartBatchSuccess> -> unit

    // SMELL: what calls this? synchronous after some other call?
    // maybe make start batch private, and call it inside enqueue && updateStatus?
    let startBatch (load: Load) (save: Save): StartBatchWorkflow =
        let loadMergeQueue = loadMergeQueue load
        let startBatchStep = startBatchStep >> Result.mapError StartBatchError
        let saveMergeQueue: SaveMergeQueue = (AggregateSuccess.aggregate >> save)

        fun command ->
            command
            |> Common.switch (loadMergeQueue)
            |> Common.bind startBatchStep
            |> Common.tee (Result.map saveMergeQueue)
            |> Result.map AggregateSuccess.success

module IngestBuild =
    open WorkflowTypes.IngestBuild

    /// Validation
    type private ValidateCommand = Command -> Result<BuildMessage, string>

    let private validateCommand: ValidateCommand =
        fun command ->
            // TODO: make this impl less gross
            match command.message with
            | UnvalidatedBuildMessage.Success(shaValue) ->
                shaValue
                |> SHA.create
                |> Result.map BuildMessage.Success
            | UnvalidatedBuildMessage.Failure ->
                Ok BuildMessage.Failure

    /// Loading
    type private LoadMergeQueue = BuildMessage -> Command<BuildMessage>

    let private loadMergeQueue (load: Load): LoadMergeQueue =
        fun command -> load() |> Command.create command

    /// Ingest
    type private IngestBuildStep = DomainService<BuildMessage, IngestBuildSuccess, IngestBuildError>

    let private ingestBuildStep: IngestBuildStep =
        ingestBuildUpdate

    /// Save
    type private SaveMergeQueue = AggregateSuccess<IngestBuildSuccess> -> unit

    /// The final workflow
    let ingestBuildUpdate (load: Load) (save: Save): IngestBuildWorkflow =
        let validateCommand = validateCommand >> Result.mapError ValidationError
        let loadMergeQueue = loadMergeQueue load
        let ingestBuildStep = ingestBuildStep >> Result.mapError IngestBuildError
        let saveMergeQueue: SaveMergeQueue = (AggregateSuccess.aggregate >> save)

        fun command ->
            command
            |> validateCommand
            |> Result.map loadMergeQueue
            |> Result.bind ingestBuildStep
            |> Common.tee (Result.map saveMergeQueue)
            |> Result.map AggregateSuccess.success

module IngestMerge =
    open WorkflowTypes.IngestMerge

    // Validation
    type private ValidateCommand = Command -> MergeMessage

    let private validateCommand: ValidateCommand =
        fun command ->
            let message =
                match command.message with
                | Success -> MergeMessage.Success
                | Failure -> MergeMessage.Failure

            message

    // Load merge queue
    type private LoadMergeQueue = MergeMessage -> Command<MergeMessage>

    let private loadMergeQueue (load: Load): LoadMergeQueue =
        fun command -> load() |> Command.create command

    // Ingest merge update
    type private IngestMergeStep = DomainService<MergeMessage, IngestMergeSuccess, IngestMergeError>

    let private ingestMergeStep: IngestMergeStep =
        ingestMergeUpdate

    // Save merge queue
    type private SaveMergeQueue = AggregateSuccess<IngestMergeSuccess> -> unit

    // the final workflow
    let ingestMergeUpdate (load: Load) (save: Save): IngestMergeWorkflow =
        let loadMergeQueue = loadMergeQueue load
        let ingestMergeStep = ingestMergeStep >> Result.mapError IngestMergeError
        let saveMergeQueue: SaveMergeQueue = (AggregateSuccess.aggregate >> save)

        fun command ->
            command
            |> Common.switch validateCommand
            |> Result.map loadMergeQueue
            |> Result.bind ingestMergeStep
            |> Common.tee (Result.map saveMergeQueue)
            |> Result.map AggregateSuccess.success

module UpdatePullRequest =
    open WorkflowTypes.UpdatePullRequest

    // Validation
    type private ValidateCommand = Command -> Result<PullRequestUpdate, string>

    let private validateCommand: ValidateCommand =
        fun command ->
            let number' = PullRequestNumber.create command.number
            let newSha' = SHA.create command.sha

            match number', newSha' with
            | Ok number, Ok newValue ->
                Ok
                    { number = number
                      sha = newValue }
            | Error error, _ ->
                Error(sprintf "Validation failed: %s" error)
            | _, Error error ->
                Error(sprintf "Validation failed: %s" error)

    // Load merge queue
    type private LoadMergeQueue = PullRequestUpdate -> Command<PullRequestUpdate>

    let private loadMergeQueue (load: Load): LoadMergeQueue =
        fun command -> load() |> Command.create command

    // Update the SHA
    type private UpdateStep = Command<PullRequestUpdate> -> AggregateSuccess<UpdatePullRequestSuccess>

    // Save merge queue
    type private SaveMergeQueue = AggregateSuccess<UpdatePullRequestSuccess> -> unit

    // Implementation
    let updatePullRequestSha (load: Load) (save: Save): UpdatePullRequestWorkflow =
        let validateCommand = validateCommand >> Result.mapError ValidationError
        let loadMergeQueue = loadMergeQueue load
        let updateStep: UpdateStep = updatePullRequest
        let saveMergeQueue: SaveMergeQueue = (AggregateSuccess.aggregate >> save)

        fun command ->
            command
            |> validateCommand
            |> Result.map loadMergeQueue
            |> Result.map updateStep
            |> Common.tee (Result.map saveMergeQueue)
            |> Result.map AggregateSuccess.success

module UpdateStatuses =
    open WorkflowTypes.UpdateStatuses

    // Validation
    type private ValidateCommand = Command -> Result<StatusUpdate, string>

    let private validateCommand: ValidateCommand =
        // TODO: chain validation with further processing and return errors
        fun command ->
            let number' = PullRequestNumber.create command.number
            let buildSha' = SHA.create command.sha

            let statuses' =
                command.statuses
                |> List.map CommitStatus.create
                |> consolidateResultList

            match number', buildSha', statuses' with
            | Ok number, Ok buildSha, Ok statuses ->
                Ok
                    { number = number
                      sha = buildSha
                      statuses = statuses }
            | Error error, _, _ ->
                Error error
            | _, Error error, _ ->
                Error error
            | _, _, Error error ->
                Error error

    // Load merge queue
    type private LoadMergeQueue = StatusUpdate -> Command<StatusUpdate>

    let private loadMergeQueue (load: Load): LoadMergeQueue =
        fun command -> load() |> Command.create command

    // Update the statuses
    type private UpdateStep = Command<StatusUpdate> -> AggregateSuccess<UpdateStatusesSuccess>

    // Save
    type private SaveMergeQueue = AggregateSuccess<UpdateStatusesSuccess> -> unit

    // Implementation
    let updateStatuses (load: Load) (save: Save): UpdateStatusWorkflow =
        let validateCommand = validateCommand >> Result.mapError ValidationError
        let loadMergeQueue = loadMergeQueue load
        let updateStep: UpdateStep = updateStatuses
        let saveMergeQueue: SaveMergeQueue = (AggregateSuccess.aggregate >> save)

        fun command ->
            command
            |> validateCommand
            |> Result.map loadMergeQueue
            |> Result.map updateStep
            |> Common.tee (Result.map saveMergeQueue)
            |> Result.map AggregateSuccess.success
