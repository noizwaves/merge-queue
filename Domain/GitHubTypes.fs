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
    
    type LookUpPullRequestDetails = int -> Async<Result<PullRequestDetails, string>>
