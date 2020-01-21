module MergeQueue.GitHubWebhook

open Suave
open Suave.Operators

open Newtonsoft.Json
open Newtonsoft.Json.Serialization

type private NestedPullRequestBody =
    { url: string }

type private IssueJsonBody =
    { number: int
      pull_request: NestedPullRequestBody }

type private CommonJsonBody =
    { body: string }

type private IssueCommentJsonBody =
    { issue: IssueJsonBody
      comment: CommonJsonBody }

type private PullRequestComment =
    { number: int
      body: string }

let private fromJson<'a> s =
    let jsonSerializerSettings = JsonSerializerSettings()
    jsonSerializerSettings.ContractResolver <- CamelCasePropertyNamesContractResolver()

    try
        JsonConvert.DeserializeObject<'a>(s, jsonSerializerSettings) |> Some
    with
    | :? Newtonsoft.Json.JsonSerializationException ->
        None
    | :? Newtonsoft.Json.JsonReaderException ->
        None

let private issueCommentHandler (fetch: SimpleSandboxApi.GetState) (store: SimpleSandboxApi.UpdateState)
    (request: HttpRequest): WebPart =
    let issueCommonJson =
        request.rawForm
        |> System.Text.Encoding.UTF8.GetString
        |> fromJson<IssueCommentJsonBody>


    match issueCommonJson with
    | None ->
        RequestErrors.BAD_REQUEST "Failed to parse expected JSON structure"
    | Some jsonObj ->
        // TODO: determine if issue is a pull request

        let comment =
            { number = jsonObj.issue.number
              body = jsonObj.comment.body }

        // TODO: determine PRs sha and commit statuses
        let pullRequest =
            Domain.pullRequest (Domain.pullRequestId comment.number) (Domain.sha "unknown...")
                [ Domain.commitStatus "unknown..." Domain.CommitStatusState.Success ]

        match comment.body with
        | "enqueue" ->
            let result, state =
                fetch() |> Domain.enqueue pullRequest

            state |> store

            match result with
            | Domain.EnqueueResult.Enqueued ->
                "Enqueued" |> Successful.OK
            | Domain.EnqueueResult.AlreadyEnqueued ->
                "Already enqueued" |> Successful.OK
            | Domain.EnqueueResult.SinBinned ->
                "Sin binned" |> Successful.OK
            | Domain.EnqueueResult.RejectedFailingBuildStatus ->
                "Rejected: PR has a failing build status" |> Successful.OK
        | _ ->
            "Comment did not match to any known commands" |> Successful.OK

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

let handle fetch store =
    choose [ gitHubEvent "issue_comment" >=> request (issueCommentHandler fetch store) ]
