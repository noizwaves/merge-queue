namespace MergeQueue

open System.Collections

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

    type MergeQueue =
        { queue: AttemptQueue
          sinBin: SinBin
          batch: CurrentBatch }

    // A batch from this list should not be a batch that can be used for other functions
    // so more like a List<PreviewBatch> or List<PlannedBatch>
    type ExecutionPlan = List<Batch>

    // Function types

    // Is this really something that happens in the domain? maybe is a private impl?
    type RemoveAllFromQueue = List<PullRequest> -> AttemptQueue -> AttemptQueue

    type RemoveFromQueue = PullRequestID -> AttemptQueue -> AttemptQueue

    // Only PullRequest.statuses field is used
    type GetBuildStatus = PullRequest -> BuildStatus

    type AddToQueue = PullRequest -> AttemptQueue -> AttemptQueue

    type AddToSinBin = PullRequest -> SinBin -> SinBin

    // SMELL: These methods suspiciously return bools and seem dangerous to expose in the domain types
    type InQueue = PullRequestID -> AttemptQueue -> bool

    type InSinBin = PullRequestID -> SinBin -> bool

    type InBatch = PullRequestID -> Batch -> bool

    type InRunningBatch = PullRequestID -> CurrentBatch -> bool

    // SMELL: Empty AttemptQueue will not result in a batch... so Option<Batch>
    type PickNextBatch = AttemptQueue -> Batch

    type Bisect = Batch -> Option<Batch * Batch>

    // Surely we cannot complete just *any*, maybe only running batches
    type CompleteBuild = Batch -> CurrentBatch

    // only makes sense to do this on batches that failed to build
    type FailWithoutRetry = Batch -> AttemptQueue -> (AttemptQueue * CurrentBatch)

    type FailWithRetry = Batch -> AttemptQueue -> (AttemptQueue * CurrentBatch)

    type CompleteMerge = Batch -> AttemptQueue -> (AttemptQueue * CurrentBatch)

    type FailMerge = Batch -> CurrentBatch

    type MovePullRequestToSinBin = PullRequestID -> SHA -> AttemptQueue -> SinBin -> (AttemptQueue * SinBin)

    type UpdateShaInSinBin = PullRequestID -> SHA -> SinBin -> SinBin

    // These feel kinda like application services...
    // these may move pull requests SinBin <> AttemptQueue
    type UpdateShaInQueueWhenBatchRunning = PullRequestID -> SHA -> Batch -> AttemptQueue -> SinBin -> (bool * AttemptQueue * SinBin)

    type UpdateStatusesInSinBin = PullRequestID -> SHA -> CommitStatuses -> AttemptQueue -> SinBin -> (AttemptQueue * SinBin)

    // Command-land down here

    // State is really just convenience for AttemptQueue * SinBin
    // State only changes some of the time...
    type Enqueue = PullRequest -> MergeQueue -> MergeQueue

    // State is really just convenience for AttemptQueue * SinBin
    // State only changes some of the time
    type Dequeue = PullRequestID -> MergeQueue -> MergeQueue

    // feels more like = IdleQueue -> Option<Batch>, no reason to allow running or merging queues to be started
    // Maybe it shouldn't be a command
    type StartBatch = MergeQueue -> Option<Batch>

    //
    type PreviewExecutionPlan = MergeQueue -> ExecutionPlan

// ? UpdateRunningBatch / ingestBuildUpdate
// ? UpdateMergingBatch / ingestMergeUpdate
// ? UpdatePullRequestSha / ingestPullRequestShaUpdate
// ? UpdateStatuses / ingestPullRequestStatusUpdate
