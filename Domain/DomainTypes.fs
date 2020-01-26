namespace MergeQueue

open System.Collections

module DomainTypes =
    type SHA = SHA of string

    // TODO: Rename to Number, this is GitHub vocab
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
        | Running of Batch // running === building in some places, like build Status
        | Merging of Batch

    type BisectPath = List<bool>

    type AttemptQueue = List<PullRequest * BisectPath>

    type SinBin = List<PullRequest>

    // aggregate root fo sure
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

    // SMELL: Empty AttemptQueue will not result in a batch... so Option<Batch>
    type PickNextBatch = AttemptQueue -> Batch

    type Bisect = Batch -> Option<Batch * Batch>

    //
    // Moving batches through the batch workflow
    //

    // Surely we cannot complete just *any*, maybe only running batches
    // We won't ever spit out Running or NoBatch...
    type CompleteBuild = Batch -> CurrentBatch

    // only makes sense to do this on batches that failed to build
    type FailWithoutRetry = Batch -> AttemptQueue -> (AttemptQueue * CurrentBatch)

    // only makes sense to do this on batches that failed to build
    type FailWithRetry = Batch -> AttemptQueue -> (AttemptQueue * CurrentBatch)

    type CompleteMerge = Batch -> AttemptQueue -> (AttemptQueue * CurrentBatch)

    type FailMerge = Batch -> CurrentBatch


    // These feel kinda like application services...
    // these may move pull requests SinBin <> AttemptQueue
    // Does there just need to be an UpdateSha? UpdateSha = PullRequestNumber -> SHA -> MergeQueue -> MergeQueue

    // result is keep PR in Sin Bin
    type UpdateShaInSinBin = PullRequestID -> SHA -> SinBin -> SinBin
    // result is Maybe move PR to Sin Bin
    type UpdateShaInQueue = PullRequestID -> SHA -> AttemptQueue -> SinBin -> (AttemptQueue * SinBin)
    // result is maybe cancel the current batch
    type UpdateShaInRunningBatch = PullRequestID -> SHA -> Batch -> AttemptQueue -> SinBin -> (bool * AttemptQueue * SinBin)


    type UpdateStatusesInSinBin = PullRequestID -> SHA -> CommitStatuses -> AttemptQueue -> SinBin -> (AttemptQueue * SinBin)



    // Command-land or use case land down here

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

    // SMELL: These methods suspiciously return bools and seem dangerous to expose in the domain types
    type InQueue = PullRequestID -> AttemptQueue -> bool

    type InSinBin = PullRequestID -> SinBin -> bool

    type InBatch = PullRequestID -> Batch -> bool

    type InRunningBatch = PullRequestID -> CurrentBatch -> bool

// ? UpdateRunningBatch / ingestBuildUpdate
// ? UpdateMergingBatch / ingestMergeUpdate
// ? UpdatePullRequestSha / ingestPullRequestShaUpdate
// ? UpdateStatuses / ingestPullRequestStatusUpdate
