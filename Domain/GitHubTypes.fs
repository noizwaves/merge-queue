namespace MergeQueue

module GitHubTypes =
    type State =
        | Expected
        | Error_
        | Failure
        | Pending
        | Success

    type PullRequestDetails =
        { sha: string
          statuses: List<string * State> }

    type PullRequestIdentifier =
        { repoOwner: string
          repoName: string
          number: int }

    type LookUpPullRequestDetails = PullRequestIdentifier -> Async<Result<PullRequestDetails, string>>
