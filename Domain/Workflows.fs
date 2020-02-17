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
    type private StepInput =
        { pullRequest: PullRequest
          mergeQueue: MergeQueue }

    type private LoadMergeQueue = PullRequest -> StepInput

    let private loadMergeQueue (load: Load): LoadMergeQueue =
        fun pr ->
            let model = load()
            { pullRequest = pr
              mergeQueue = model }

    /// Enqueue step
    type private EnqueueStep = StepInput -> Result<EnqueueSuccess * MergeQueue, EnqueueError>

    let private enqueueStep: EnqueueStep =
        fun input -> input.mergeQueue |> Domain.enqueue input.pullRequest

    /// Save changes to the MergeQueue
    type private SaveMergeQueue = MergeQueue -> unit

    let private saveMergeQueue (save: Save): SaveMergeQueue =
        fun model -> save model

    // the final workflow
    let enqueue (load: Load) (save: Save): EnqueueWorkflow =
        let loadMergeQueue = loadMergeQueue load
        let saveMergeQueue = saveMergeQueue save

        let validatePullRequest = validatePullRequest >> (Result.mapError Error.ValidationError)
        let enqueueStep = enqueueStep >> (Result.mapError Error.EnqueueError)

        fun command ->
            command
            |> validatePullRequest
            |> Result.map loadMergeQueue
            |> Common.bind enqueueStep
            |> Common.tee (Common.mapSecond saveMergeQueue)
            |> Common.mapFirst id

module Dequeue =
    open WorkflowTypes.Dequeue

    /// Validation
    type private ValidateDequeue = Command -> Result<PullRequestNumber, string>

    let private validateDequeue: ValidateDequeue =
        fun command -> command.number |> PullRequestNumber.create

    /// Loading
    type private StepInput =
        { command: PullRequestNumber
          mergeQueue: MergeQueue }

    type private LoadMergeQueue = PullRequestNumber -> StepInput

    let private loadMergeQueue (load: Load): LoadMergeQueue =
        fun command ->
            let mergeQueue = load()
            { command = command
              mergeQueue = mergeQueue }

    /// Dequeue step
    type private DequeueStep = StepInput -> Result<DequeueSuccess * MergeQueue, DequeueError>

    let private dequeueStep: DequeueStep =
        fun input -> input.mergeQueue |> Domain.dequeue input.command

    /// Save
    type private SaveMergeQueue = MergeQueue -> unit

    let private saveMergeQueue (save: Save): SaveMergeQueue =
        fun mergeQueue -> save mergeQueue

    // the final workflow
    let dequeue (load: Load) (save: Save): DequeueWorkflow =
        let validateDequeue = validateDequeue >> Result.mapError ValidationError
        let loadMergeQueue = loadMergeQueue load
        let dequeueStep = dequeueStep >> Result.mapError DequeueError
        let saveMergeQueue = saveMergeQueue save

        fun command ->
            command
            |> validateDequeue
            |> Result.map loadMergeQueue
            |> Common.bind dequeueStep
            |> Common.tee (Common.mapSecond saveMergeQueue)
            |> Common.mapFirst id

module StartBatch =
    open WorkflowTypes.StartBatch
    // Implementation

    /// Load
    type private StepInput =
        { command: unit
          mergeQueue: MergeQueue }

    type private LoadMergeQueue = unit -> StepInput

    let private loadMergeQueue (load: Load): LoadMergeQueue =
        fun command ->
            let mergeQueue = load()
            { command = command
              mergeQueue = mergeQueue }

    /// Start the batch
    type private StartBatchStep = StepInput -> Result<StartBatchSuccess * MergeQueue, StartBatchError>

    let private toInput (mergeQueue: MergeQueue): StepInput =
        { command = ()
          mergeQueue = mergeQueue }

    let private startBatchStep: StartBatchStep =
        fun input -> input.mergeQueue |> startBatch()

    /// Save
    type private SaveMergeQueue = MergeQueue -> unit

    let private saveMergeQueue (save: Save): SaveMergeQueue =
        fun mergeQueue -> save mergeQueue

    // SMELL: what calls this? synchronous after some other call?
    // maybe make start batch private, and call it inside enqueue && updateStatus?
    let startBatch (load: Load) (save: Save): StartBatchWorkflow =
        let loadMergeQueue = loadMergeQueue load
        let startBatchStep = startBatchStep >> Result.mapError StartBatchError
        let saveMergeQueue = saveMergeQueue save

        fun command ->
            command
            |> Common.switch (loadMergeQueue)
            |> Common.bind startBatchStep
            |> Common.tee (Common.mapSecond saveMergeQueue)
            |> Common.mapFirst id

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
    type private StepInput =
        { command: BuildMessage
          mergeQueue: MergeQueue }

    type private LoadMergeQueue = BuildMessage -> StepInput

    let private loadMergeQueue (load: Load): LoadMergeQueue =
        fun command ->
            let model = load()
            { command = command
              mergeQueue = model }

    /// Ingest
    type private IngestBuildStep = StepInput -> Result<IngestBuildSuccess * MergeQueue, IngestBuildError>

    let private ingestBuildStep: IngestBuildStep =
        fun input ->
            let message = input.command
            let model = input.mergeQueue

            ingestBuildUpdate message model

    /// Save
    type private SaveMergeQueue = MergeQueue -> unit

    let private saveMergeQueue (save: Save): SaveMergeQueue =
        fun mergeQueue -> save mergeQueue

    /// The final workflow
    let ingestBuildUpdate (load: Load) (save: Save): IngestBuildWorkflow =
        let validateCommand = validateCommand >> Result.mapError ValidationError
        let loadMergeQueue = loadMergeQueue load
        let ingestBuildStep = ingestBuildStep >> Result.mapError IngestBuildError
        let saveMergeQueue = saveMergeQueue save

        fun command ->
            command
            |> validateCommand
            |> Result.map loadMergeQueue
            |> Result.bind ingestBuildStep
            |> Common.tee (Common.mapSecond saveMergeQueue)
            |> Common.mapFirst id

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
    type private StepInput =
        { command: MergeMessage
          mergeQueue: MergeQueue }

    type private LoadMergeQueue = MergeMessage -> StepInput

    let private loadMergeQueue (load: Load): LoadMergeQueue =
        fun command ->
            let mergeQueue = load()
            { command = command
              mergeQueue = mergeQueue }

    // Ingest merge update
    type private IngestMergeStep = StepInput -> Result<IngestMergeSuccess * MergeQueue, IngestMergeError>

    let private ingestMergeStep: IngestMergeStep =
        fun input -> input.mergeQueue |> ingestMergeUpdate input.command

    // Save merge queue
    type private SaveMergeQueue = MergeQueue -> unit

    let private saveMergeQueue (save: Save): SaveMergeQueue =
        fun mergeQueue -> save mergeQueue

    let ingestMergeUpdate (load: Load) (save: Save): IngestMergeWorkflow =
        let loadMergeQueue = loadMergeQueue load
        let ingestMergeStep = ingestMergeStep >> Result.mapError IngestMergeError
        let saveMergeQueue = saveMergeQueue save

        fun command ->
            command
            |> Common.switch validateCommand
            |> Result.map loadMergeQueue
            |> Result.bind ingestMergeStep
            |> Common.tee (Common.mapSecond saveMergeQueue)
            |> Common.mapFirst id

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
    type private StepInput =
        { command: PullRequestUpdate
          mergeQueue: MergeQueue }

    type private LoadMergeQueue = PullRequestUpdate -> StepInput

    let private loadMergeQueue (load: Load): LoadMergeQueue =
        fun command ->
            let mergeQueue = load()
            { command = command
              mergeQueue = mergeQueue }

    // Update the SHA
    type private UpdateStep = StepInput -> (UpdatePullRequestSuccess * MergeQueue)

    let private updateStep: UpdateStep =
        fun input -> input.mergeQueue |> updatePullRequest input.command

    // Save merge queue
    type private SaveMergeQueue = MergeQueue -> unit

    let private saveMergeQueue (save: Save): SaveMergeQueue =
        fun mergeQueue -> save mergeQueue

    // Implementation
    let updatePullRequestSha (load: Load) (save: Save): UpdatePullRequestWorkflow =
        let validateCommand = validateCommand >> Result.mapError ValidationError
        let loadMergeQueue = loadMergeQueue load
        let saveMergeQueue = saveMergeQueue save

        fun command ->
            command
            |> validateCommand
            |> Result.map loadMergeQueue
            |> Result.map updateStep
            |> Common.tee (Common.mapSecond saveMergeQueue)
            |> Common.mapFirst id

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
    type private StepInput =
        { command: StatusUpdate
          mergeQueue: MergeQueue }

    type private LoadMergeQueue = StatusUpdate -> StepInput

    let private loadMergeQueue (load: Load): LoadMergeQueue =
        fun command ->
            let mergeQueue = load()
            { command = command
              mergeQueue = mergeQueue }

    // Update the statuses
    type private UpdateStep = StepInput -> (UpdateStatusesSuccess * MergeQueue)

    let private updateStep: UpdateStep =
        fun input -> input.mergeQueue |> updateStatuses input.command

    // Save
    type private SaveMergeQueue = MergeQueue -> unit

    let private saveMergeQueue (save: Save): SaveMergeQueue =
        fun mergeQueue -> save mergeQueue

    // Implementation
    let updateStatuses (load: Load) (save: Save): UpdateStatusWorkflow =
        let validateCommand = validateCommand >> Result.mapError ValidationError
        let loadMergeQueue = loadMergeQueue load
        let saveMergeQueue = saveMergeQueue save

        fun command ->
            command
            |> validateCommand
            |> Result.map loadMergeQueue
            |> Result.map updateStep
            |> Common.tee (Common.mapSecond saveMergeQueue)
            |> Common.mapFirst id
