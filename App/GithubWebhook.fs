module MergeQueue.GitHubWebhook

open FSharp.Data
open Suave
open Suave.Operators

type private IssueCommentJsonBodyProvider = JsonProvider<"""{
  "issue": {
    "number": 1,
    "pull_request": {
      "url": "https://api.github.com/repos/Codertocat/Hello-World/pulls/1"
    }
  },
  "comment": {
    "body": "enqueue"
  }
}""">

type private IssueCommentJsonBody =
    { Number: int
      Comment: string }

type private DeserializeDtoFromRequest = HttpRequest -> Result<IssueCommentJsonBody, string>

let private deserializeDtoFromRequest: DeserializeDtoFromRequest =
    // Touch all important fields within the try/catch block
    // as invalid json will raise exception at runtime. Dumb?
    let toDto (parsed: IssueCommentJsonBodyProvider.Root) =
        { Number = parsed.Issue.Number
          Comment = parsed.Comment.Body }

    fun request ->
        try
            request.rawForm
            |> System.Text.Encoding.UTF8.GetString
            |> IssueCommentJsonBodyProvider.Parse
            |> toDto
            |> Ok

        with error ->
            Error "Deserialization error"

type private IntendedCommand = EnqueueCommand of IssueCommentJsonBody

type private DetermineIntendedCommand = IssueCommentJsonBody -> Result<IntendedCommand, string>

let private determineIntendedCommand: DetermineIntendedCommand =
    fun issueComment ->
        match issueComment.Comment with
        | "enqueue" ->
            EnqueueCommand(issueComment) |> Ok
        | _ ->
            "no matching command" |> Error

type private ProcessIntendedCommand = Result<IntendedCommand, string> -> WebPart

let private processEnqueue load save lookup (issueComment: IssueCommentJsonBody): WebPart =
    // TODO: We need to get the SHA and Statuses from the GitHub API
    let comment: Workflows.Enqueue.Command =
        { number = issueComment.Number
          sha = "???"
          statuses = [ "???", "???" ] }

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

let private processIntendedCommand load save lookup: ProcessIntendedCommand =
    fun command ->
        match command with
        | Ok(EnqueueCommand issueComment) ->
            issueComment |> processEnqueue load save lookup
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
