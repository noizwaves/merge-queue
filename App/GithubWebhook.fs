module MergeQueue.GitHubWebhook

open FSharp.Data
open Suave
open Suave.Operators

type private IssueCommentJsonBodyProvider = JsonProvider<"""{
  "issue": {
    "number": 1
  },
  "comment": {
    "body": "enqueue"
  },
  "repository": {
    "name": "some-name",
    "owner": {
      "login": "some-owner"
    }
  }
}""">

type private IssueCommentJsonBody =
    { Number: int
      Comment: string
      RepoName: string
      RepoOwner: string }

type private DeserializeDtoFromRequest = HttpRequest -> Result<IssueCommentJsonBody, string>

let private deserializeDtoFromRequest: DeserializeDtoFromRequest =
    // Touch all important fields within the try/catch block
    // as invalid json will raise exception at runtime. Dumb?
    let toDto (parsed: IssueCommentJsonBodyProvider.Root) =
        { Number = parsed.Issue.Number
          Comment = parsed.Comment.Body
          RepoName = parsed.Repository.Name
          RepoOwner = parsed.Repository.Owner.Login }

    fun request ->
        try
            request.rawForm
            |> System.Text.Encoding.UTF8.GetString
            |> IssueCommentJsonBodyProvider.Parse
            |> toDto
            |> Ok

        with error ->
            Error "Deserialization error"

type private IntendedCommand =
    | EnqueueCommand of IssueCommentJsonBody
    | DequeueCommand of IssueCommentJsonBody

type private DetermineIntendedCommand = IssueCommentJsonBody -> Result<IntendedCommand, string>

let private determineIntendedCommand: DetermineIntendedCommand =
    fun issueComment ->
        match issueComment.Comment with
        | "enqueue" ->
            EnqueueCommand(issueComment) |> Ok
        | "dequeue" ->
            DequeueCommand(issueComment) |> Ok
        | _ ->
            "no matching command" |> Error

type private ProcessIntendedCommand = Result<IntendedCommand, string> -> WebPart

let private processEnqueue load save lookup (issueComment: IssueCommentJsonBody): WebPart =
    let comment: Workflows.Enqueue.Command =
        { number = issueComment.Number
          repoOwner = issueComment.RepoOwner
          repoName = issueComment.RepoName }

    let asSuccess (success: DomainServiceTypes.EnqueueSuccess) =
        match success with
        | DomainServiceTypes.EnqueueSuccess.Enqueued ->
            "Enqueued" |> Successful.OK
        | DomainServiceTypes.EnqueueSuccess.SinBinned ->
            "Sin Binned" |> Successful.OK

    let asError (error: Workflows.Enqueue.Error) =
        match error with
        | Workflows.Enqueue.Error.EnqueueError DomainServiceTypes.EnqueueError.AlreadyEnqueued ->
            "Already enqueued" |> Successful.OK // TODO: More appropriate code
        | Workflows.Enqueue.Error.EnqueueError DomainServiceTypes.EnqueueError.RejectedFailingBuildStatus ->
            "Rejected: PR has a failing build status" |> Successful.OK // TODO: More appropriate code
        | Workflows.Enqueue.Error.ValidationError message ->
            message |> Successful.OK // TODO: More appropriate code
        | Workflows.Enqueue.Error.RemoteServiceError message ->
            message |> Successful.OK // TODO: More appropriate code

    comment
    |> Workflows.Enqueue.enqueue load save lookup
    |> Result.fold asSuccess asError

let private processDequeue load save lookup (issueComment: IssueCommentJsonBody): WebPart =
    let comment: Workflows.Dequeue.Command =
        { number = issueComment.Number
          repoOwner = issueComment.RepoOwner
          repoName = issueComment.RepoName }

    let asSuccess (success: DomainServiceTypes.DequeueSuccess) =
        match success with
        | DomainServiceTypes.DequeueSuccess.Dequeued ->
            "Dequeued" |> Successful.OK
        | DomainServiceTypes.DequeueSuccess.DequeuedAndAbortRunningBatch _ ->
            "Dequeued and aborted a running batch" |> Successful.OK

    let asError (error: Workflows.Dequeue.Error) =
        match error with
        | Workflows.Dequeue.Error.DequeueError DomainServiceTypes.DequeueError.NotFound ->
            "Not found in queue" |> Successful.OK // TODO: More appropriate code
        | Workflows.Dequeue.Error.DequeueError DomainServiceTypes.DequeueError.RejectedInMergingBatch ->
            "Rejected: PR is currently being merged" |> Successful.OK // TODO: More appropriate code
        | Workflows.Dequeue.Error.ValidationError message ->
            message |> Successful.OK // TODO: More appropriate code

    comment
    |> Workflows.Dequeue.dequeue load save
    |> Result.fold asSuccess asError

let private processIntendedCommand load save lookup: ProcessIntendedCommand =
    fun command ->
        match command with
        | Ok(EnqueueCommand issueComment) ->
            issueComment |> processEnqueue load save lookup
        | Ok(DequeueCommand issueComment) ->
            issueComment |> processDequeue load save lookup
        | Error message ->
            message |> Successful.OK

type private IssueCommentHandler = HttpRequest -> WebPart

let private issueCommentHandler (load: MergeQueue.DbTypes.Load) (save: MergeQueue.DbTypes.Save)
    (lookup: MergeQueue.GitHubTypes.LookUpPullRequestDetails): IssueCommentHandler =
    fun request ->
        request
        |> deserializeDtoFromRequest
        |> Result.bind determineIntendedCommand
        |> processIntendedCommand load save lookup

let private gitHubEvent (eventType: string) =
    let inner (ctx: HttpContext) =
        async {
            let gitHubEvent = Suave.Headers.getFirstHeader "x-github-event" ctx

            let result =
                match gitHubEvent with
                | Some value ->
                    if value = eventType then Some ctx
                    else None
                | None -> None
            return result
        }
    inner

let handle fetch store lookup =
    choose [ gitHubEvent "issue_comment" >=> request (issueCommentHandler fetch store lookup) ]
