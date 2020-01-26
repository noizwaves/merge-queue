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

    type BisectPath = List<bool>

    type PassingPullRequest = PassingPullRequest of PullRequest

    type AttemptQueue = AttemptQueue of List<PassingPullRequest * BisectPath>

    type NaughtyPullRequest = NaughtyPullRequest of PullRequest

    type SinBin = SinBin of List<NaughtyPullRequest>

    type Batch = Batch of List<PassingPullRequest * BisectPath>

    type RunnableBatch = RunnableBatch of Batch

    type MergeableBatch = MergeableBatch of Batch

    type BisectedBatch = BisectedBatch of Batch

    type ActiveBatch =
        | NoBatch
        | Running of RunnableBatch
        | Merging of MergeableBatch

    // aggregate root fo sure
    type MergeQueue =
        { queue: AttemptQueue
          sinBin: SinBin
          activeBatch: ActiveBatch }

    // A batch from this list should not be a batch that can be used for other functions
    // so more like a List<PreviewBatch> or List<PlannedBatch>
    type PlannedBatch = PlannedBatch of List<PullRequestID>

    type ExecutionPlan = List<PlannedBatch>

    // Function types


    // Only PullRequest.statuses field is used... DeriveBuildStatus?
    type GetBuildStatus = PullRequest -> BuildStatus

    type PrepareForQueue = PullRequest -> Choice<PassingPullRequest, NaughtyPullRequest>

    type AddToQueue = PassingPullRequest -> AttemptQueue -> AttemptQueue

    type RemoveFromQueue = PullRequestID -> AttemptQueue -> AttemptQueue

    type AddToSinBin = NaughtyPullRequest -> SinBin -> SinBin

    type PickNextBatch = AttemptQueue -> Option<RunnableBatch * AttemptQueue>

    type Bisect = RunnableBatch -> Option<BisectedBatch * BisectedBatch>

    //
    // Moving batches through the batch workflow
    //

    // Surely we cannot complete just *any*, maybe only running batches
    // We won't ever spit out Running or NoBatch...
    type CompleteBuild = RunnableBatch -> MergeableBatch

    // only makes sense to do this on batches that failed to build
    // Batch argument should represent the retry-ability
    // Do we need to change current batch all the time in these methods?
    type FailWithoutRetry = RunnableBatch -> AttemptQueue -> (AttemptQueue * ActiveBatch)

    // SMLELL: Do we need to change current batch all the time in these methods?
    type FailWithRetry = BisectedBatch -> BisectedBatch -> AttemptQueue -> (AttemptQueue * ActiveBatch)

    type CompleteMerge = MergeableBatch -> AttemptQueue -> (AttemptQueue * ActiveBatch)

    type FailMerge = MergeableBatch -> ActiveBatch


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
    type StartBatch = MergeQueue -> MergeQueue

    type UpdateSha = PullRequestID * SHA -> (MergeQueue -> MergeQueue)

    type UpdateStatuses = PullRequestID * SHA * CommitStatuses -> (MergeQueue -> MergeQueue)

    //
    type PreviewExecutionPlan = MergeQueue -> ExecutionPlan

    // SMELL: These methods suspiciously return bools and seem dangerous to expose in the domain types
    // AHA: these are like validation!!!!! used by the commands
    type InQueue = PullRequestID -> AttemptQueue -> bool

    type InSinBin = PullRequestID -> SinBin -> bool

    type InBatch = PullRequestID -> Batch -> bool

    type InRunningBatch = PullRequestID -> ActiveBatch -> bool

// ? UpdateRunningBatch / ingestBuildUpdate
// ? UpdateMergingBatch / ingestMergeUpdate
// ? UpdatePullRequestSha / ingestPullRequestShaUpdate
// ? UpdateStatuses / ingestPullRequestStatusUpdate
