module MergeQueue.Workflows

open MergeQueue.DomainTypes
open MergeQueue.DomainServiceTypes
open MergeQueue.Domain
open MergeQueue.DbTypes
open MergeQueue.GitHubTypes

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

    let rec consolidateErrors (results: List<Result<'a, 'b>>): Result<List<'a>, 'b> =
        match results with
        | [] ->
            Ok []
        | Error b :: tail ->
            Error b
        | Ok a :: tail ->
            tail
            |> consolidateErrors
            |> Result.map (fun aa -> a :: aa)

module Enqueue =
    // Types
    type Command =
        { repoOwner: string
          repoName: string
          number: int }

    // TODO: convert to some kind of command DTO instead of using the domain type?
    type Success = EnqueueSuccess

    type Error =
        | RemoteServiceError of string
        | ValidationError of string
        | EnqueueError of EnqueueError

    type EnqueueResult = Result<Success, Error>

    type EnqueueWorkflow = Command -> EnqueueResult

    // Fetch additional information
    type private ExpandedCommand =
        { number: int
          sha: string
          statuses: List<string * string> }

    type private FetchAdditionalInfo = Command -> Result<ExpandedCommand, string>

    let private fetchAdditionalInfo (lookup: LookUpPullRequestDetails): FetchAdditionalInfo =
        let toIdentifier (command: Command): PullRequestIdentifier =
            { repoOwner = command.repoOwner
              repoName = command.repoName
              number = command.number }

        let toStateString (state: State) =
            match state with
            | State.Pending -> "Pending"
            | State.Success -> "Success"
            | State.Failure -> "Failure"
            | State.Expected -> failwith "'Expected' is an unhandled state in the current domain"
            | State.Error_ -> failwith "'Error' is an unhandled state in the current domain"

        let toAdditionalInfo number (details: PullRequestDetails): ExpandedCommand =
            let statuses =
                details.statuses |> List.map (fun (context, state) -> context, state |> toStateString)
            { number = number
              sha = details.sha
              statuses = statuses }

        fun command ->
            command
            |> toIdentifier
            |> lookup
            |> Async.RunSynchronously
            |> Result.map (toAdditionalInfo command.number)

    /// Validation
    type private ValidatePullRequest = ExpandedCommand -> Result<PullRequest, string>

    let private validatePullRequest: ValidatePullRequest =
        // TODO: tidy this up with computation expressions and composition
        fun command ->
            let number' = PullRequestNumber.create command.number
            let sha' = SHA.create command.sha

            let statuses' =
                command.statuses
                |> List.map CommitStatus.create
                |> Common.consolidateErrors

            match number', sha', statuses' with
            | Error err, _, _ -> Error err
            | _, Error err, _ -> Error err
            | _, _, Error err -> Error err
            | Ok number, Ok sha, Ok statuses -> Ok(PullRequest.create number sha statuses)

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
    let enqueue (load: Load) (save: Save) (lookupPr: LookUpPullRequestDetails): EnqueueWorkflow =
        let fetchAdditionalInfo = fetchAdditionalInfo lookupPr >> Result.mapError RemoteServiceError
        let validatePullRequest = validatePullRequest >> (Result.mapError Error.ValidationError)
        let loadMergeQueue = loadMergeQueue load
        let enqueueStep = enqueueStep >> (Result.mapError Error.EnqueueError)
        let saveMergeQueue = AggregateSuccess.aggregate >> save

        fun command ->
            command
            |> fetchAdditionalInfo
            |> Result.bind validatePullRequest
            |> Result.map loadMergeQueue
            |> Common.bind enqueueStep
            |> Common.tee (Result.map saveMergeQueue)
            |> Result.map AggregateSuccess.success

module Dequeue =
    // Types
    type Command =
        { number: int }

    type Success = DequeueSuccess

    type Error =
        | ValidationError of string
        | DequeueError of DequeueError

    type DequeueResult = Result<Success, Error>

    type DequeueWorkflow = Command -> DequeueResult

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
    // Types
    type Command = unit

    type Success = StartBatchSuccess

    type Error = StartBatchError of StartBatchError

    type StartBatchResult = Result<Success, Error>

    type StartBatchWorkflow = Command -> StartBatchResult

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
    // Types
    // TODO: Make this more like a DTO (i.e. not a DU?)
    type UnvalidatedBuildMessage =
        | Success of string // the SHA of the failed builds commit
        | Failure // TODO: how do we know which SHA failed? do we care?

    type Command =
        { message: UnvalidatedBuildMessage }

    // SMELL: Success type name clashes with Success constructor for BuildMessage
    type Success = IngestBuildSuccess

    // TODO: Validation error goes here
    type Error =
        | ValidationError of string
        | IngestBuildError of IngestBuildError

    type IngestBuildResult = Result<Success, Error>

    type IngestBuildWorkflow = Command -> IngestBuildResult

    /// Validation
    type private ValidateCommand = Command -> Result<BuildProgress, string>

    let private validateCommand: ValidateCommand =
        fun command ->
            // TODO: make this impl less gross
            match command.message with
            | UnvalidatedBuildMessage.Success(shaValue) ->
                shaValue
                |> SHA.create
                |> Result.map BuildProgress.BuildSuccess
            | UnvalidatedBuildMessage.Failure ->
                Ok BuildProgress.BuildFailure

    /// Loading
    type private LoadMergeQueue = BuildProgress -> Command<BuildProgress>

    let private loadMergeQueue (load: Load): LoadMergeQueue =
        fun command -> load() |> Command.create command

    /// Ingest
    type private IngestBuildStep = DomainService<BuildProgress, IngestBuildSuccess, IngestBuildError>

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
    // Types
    // TODO: This should be a DTO that represents the Batch merged result
    type UnvalidatedMergeMessage =
        | Success
        | Failure

    type Command =
        { message: UnvalidatedMergeMessage }

    type Success = IngestMergeSuccess

    type Error = IngestMergeError of IngestMergeError

    type IngestMergeResult = Result<IngestMergeSuccess, Error>

    type IngestMergeWorkflow = Command -> IngestMergeResult

    // Validation
    type private ValidateCommand = Command -> MergeProgress

    let private validateCommand: ValidateCommand =
        fun command ->
            let message =
                match command.message with
                | Success -> MergeProgress.Success
                | Failure -> MergeProgress.Failure

            message

    // Load merge queue
    type private LoadMergeQueue = MergeProgress -> Command<MergeProgress>

    let private loadMergeQueue (load: Load): LoadMergeQueue =
        fun command -> load() |> Command.create command

    // Ingest merge update
    type private IngestMergeStep = DomainService<MergeProgress, IngestMergeSuccess, IngestMergeError>

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
    // Types
    type Command =
        { number: int
          sha: string }

    type Success = UpdatePullRequestSuccess

    type Error = ValidationError of string

    type UpdatePullRequestResult = Result<Success, Error>

    type UpdatePullRequestWorkflow = Command -> UpdatePullRequestResult

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
    // Types
    type Command =
        { number: int
          sha: string
          statuses: List<string * string> }

    type Success = UpdateStatusesSuccess

    type Error = ValidationError of string

    type UpdateStatusResult = Result<Success, Error>

    type UpdateStatusWorkflow = Command -> UpdateStatusResult

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
                |> Common.consolidateErrors

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
