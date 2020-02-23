module MergeQueue.GitHubService

open FSharp.Data
open MergeQueue.GitHubTypes

type private PullRequestDetailsDtoProvider = JsonProvider<"""{
    "data": {
        "repository": {
            "pullRequest": {
                "headRef": {
                    "target": {
                        "history": {
                            "edges": [
                                {
                                    "node": {
                                        "oid": "dd298ac3aca097f2619799820d36c36b3db7afb5",
                                        "status": {
                                            "contexts": [
                                                {
                                                    "context": "ci/circleci: build",
                                                    "state": "SUCCESS"
                                                }
                                            ]
                                        }
                                    }
                                }
                            ]
                        }
                    }
                }
            }
        }
    }
}""">

let lookUpPullRequestDetails (apiToken: string) (username: string): LookUpPullRequestDetails =
    fun id ->
        let bearerToken = sprintf "Bearer %s" apiToken
        // TODO: use real owner and name here
        // TODO: better representation of query as a multiline string
        let body =
            sprintf
                """{"query":"query { \n  repository(owner: \"%s\", name: \"%s\") {\n    pullRequest(number: %i) {\n      headRef {\n        target {\n          ... on Commit {\n            history(first: 1) {\n              edges {\n                node {\n                  oid\n                  status {\n                    contexts {\n                      context\n                      state\n                    }\n                  }\n                }\n              }\n            }\n          }\n        }\n      }\n    }\n  }\n}","variables":{}}"""
                id.repoOwner id.repoName id.number

        let toSuccessfulBodyString (response: HttpResponse): Result<string, string> =
            match response.Body, response.StatusCode with
            | HttpResponseBody.Text text, 200 ->
                Ok text
            | HttpResponseBody.Binary _, _ ->
                Error "Received binary response from GitHub GraphQL API"
            | _, code ->
                Error(sprintf "Received unexpected status code of %i from GitHub GraphQL API" code)

        let toPullRequestDetails (body: string): Result<PullRequestDetails, string> =
            // TODO: Handle how this can fail
            let result = PullRequestDetailsDtoProvider.Parse body

            let toState stateStr =
                match stateStr with
                | "PENDING" -> State.Pending
                | "SUCCESS" -> State.Success
                | "FAILURE" -> State.Failure
                | "EXPECTED" -> State.Expected
                | "ERROR" -> State.Error_
                | unexpected -> failwithf "Unexpected state from GitHub for commit status of '%s'" unexpected

            // TODO: result.Data.Repository.PullRequest can be null if PR is not found, check for errors
            let statuses =
                result.Data.Repository.PullRequest.HeadRef.Target.History.Edges.[0].Node.Status.Contexts
                |> Array.toList
                |> List.map (fun context -> context.Context, context.State |> toState)

            Ok
                { sha = result.Data.Repository.PullRequest.HeadRef.Target.History.Edges.[0].Node.Oid
                  statuses = statuses }

        let result =
            Http.AsyncRequest
                ("https://api.github.com/graphql", httpMethod = "POST",
                 headers =
                     [ "Accept", "application/json"
                       "Authorization", bearerToken
                       "User-Agent", username
                       "Content-Type", "application/json" ], body = TextRequest body)

        result
        |> Async.map toSuccessfulBodyString
        |> Async.map (Result.bind toPullRequestDetails)
