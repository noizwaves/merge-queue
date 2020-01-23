namespace MergeQueue

module DomainTypes =
    type SHA = SHA of string

    type PullRequestID = PullRequestID of int

    // SMELL: Are CommitStatusState and CommitStatus words in our domain? Or borrowed from GitHub's API...
    type CommitStatusState =
        | Pending
        | Success
        | Failure

    type CommitStatus =
        { context: string
          state: CommitStatusState }

    type CommitStatuses = List<CommitStatus>

    type PullRequest =
        { id: PullRequestID
          sha: SHA
          statuses: CommitStatuses }

    type BuildStatus =
        | BuildPending
        | BuildSuccess
        | BuildFailure

    type Batch = List<PullRequest>

    type CurrentBatch =
        | NoBatch
        | Running of Batch
        | Merging of Batch

    type BisectPath = List<bool>

    type AttemptQueue = List<PullRequest * BisectPath>

    type SinBin = List<PullRequest>

    type MergeQueueModel =
        { queue: AttemptQueue
          sinBin: SinBin
          batch: CurrentBatch }

    type State = MergeQueueState of MergeQueueModel

    type ExecutionPlan = List<Batch>
