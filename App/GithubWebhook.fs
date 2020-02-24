module MergeQueue.GitHubWebhook

open FSharp.Data
open Suave
open Suave.Operators

module IssueComment =
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

    type IssueCommentHandler = HttpRequest -> WebPart

    let issueCommentHandler (load: MergeQueue.DbTypes.Load) (save: MergeQueue.DbTypes.Save)
        (lookup: MergeQueue.GitHubTypes.LookUpPullRequestDetails): IssueCommentHandler =
        fun request ->
            request
            |> deserializeDtoFromRequest
            |> Result.bind determineIntendedCommand
            |> processIntendedCommand load save lookup

module PullRequest =
    type private PullRequestJsonBodyProvider = JsonProvider<"""{
      "action": "some-action",
      "pull_request": {
        "number": 1,
        "head": {
          "sha": "some-sha-value"
        }
      }
    }""">

    type private PullRequestJsonBody =
        { Action: string
          Number: int
          Sha: string }

    type private DeserializeDtoFromRequest = HttpRequest -> Result<PullRequestJsonBody, string>

    let private deserializeDtoFromRequest: DeserializeDtoFromRequest =
        // Touch all important fields within the try/catch block
        // as invalid json will raise exception at runtime. Dumb?
        let toDto (parsed: PullRequestJsonBodyProvider.Root) =
            { Action = parsed.Action
              Number = parsed.PullRequest.Number
              Sha = parsed.PullRequest.Head.Sha }

        fun request ->
            try
                request.rawForm
                |> System.Text.Encoding.UTF8.GetString
                |> PullRequestJsonBodyProvider.Parse
                |> toDto
                |> Ok

            with error ->
                Error "Deserialization error"

    type private IntendedCommand = UpdatePullRequest of PullRequestJsonBody

    type private DetermineIntendedCommand = PullRequestJsonBody -> Result<IntendedCommand, string>

    let private determineIntendedCommand: DetermineIntendedCommand =
        fun pullRequest ->
            match pullRequest.Action with
            | "synchronize" ->
                UpdatePullRequest(pullRequest) |> Ok
            | _ ->
                "no matching command" |> Error

    let private processUpdatePullRequest load save (pullRequest: PullRequestJsonBody): WebPart =
        let command: Workflows.UpdatePullRequest.Command =
            { number = pullRequest.Number
              sha = pullRequest.Sha }

        let asSuccess (success: DomainServiceTypes.UpdatePullRequestSuccess) =
            match success with
            | DomainServiceTypes.UpdatePullRequestSuccess.NoChange ->
                "No change" |> Successful.OK
            | DomainServiceTypes.UpdatePullRequestSuccess.AbortRunningBatch _ ->
                "Change affects running batch, it should be aborted" |> Successful.OK
            | DomainServiceTypes.UpdatePullRequestSuccess.AbortMergingBatch _ ->
                "Change affects merging batch, it should be aborted somehow??" |> Successful.OK

        let asError (error: Workflows.UpdatePullRequest.Error) =
            match error with
            | Workflows.UpdatePullRequest.Error.ValidationError message ->
                "Already enqueued" |> Successful.OK // TODO: More appropriate code

        command
        |> Workflows.UpdatePullRequest.updatePullRequestSha load save
        |> Result.fold asSuccess asError

    type private ProcessIntendedCommand = Result<IntendedCommand, string> -> WebPart

    let private processIntendedCommand load save: ProcessIntendedCommand =
        fun command ->
            match command with
            | Ok(UpdatePullRequest pullRequest) ->
                pullRequest |> processUpdatePullRequest load save
            | Error message ->
                message |> Successful.OK

    type PullRequestHandler = HttpRequest -> WebPart

    let pullRequestHandler (load: MergeQueue.DbTypes.Load) (save: MergeQueue.DbTypes.Save): PullRequestHandler =
        fun request ->
            request
            |> deserializeDtoFromRequest
            |> Result.bind determineIntendedCommand
            |> processIntendedCommand load save

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
    choose
        [ gitHubEvent "issue_comment" >=> request (IssueComment.issueCommentHandler fetch store lookup)
          gitHubEvent "pull_request" >=> request (PullRequest.pullRequestHandler fetch store) ]
