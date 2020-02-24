namespace MergeQueue

module DomainTypes =
    type SHA = SHA of string

    type PullRequestNumber = PullRequestNumber of int

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
        { number: PullRequestNumber
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
    type PlannedBatch = PlannedBatch of List<PullRequestNumber>

    type ExecutionPlan = List<PlannedBatch>

    // Function types


    // Only PullRequest.statuses field is used... DeriveBuildStatus?
    type GetBuildStatus = PullRequest -> BuildStatus

    type PrepareForQueue = PullRequest -> Choice<PassingPullRequest, NaughtyPullRequest>

    // SMELL: this doesn't always succeed...
    type AddToQueue = PassingPullRequest -> AttemptQueue -> AttemptQueue

    type RemoveFromQueue = PullRequestNumber -> AttemptQueue -> AttemptQueue

    type AddToSinBin = NaughtyPullRequest -> SinBin -> SinBin

    type PickNextBatch = AttemptQueue -> Option<RunnableBatch * AttemptQueue>

    type Bisect = RunnableBatch -> Option<BisectedBatch * BisectedBatch>

    //
    // Moving batches through the batch workflow
    //

    // Surely we cannot complete just *any*, maybe only running batches
    // We won't ever spit out Running or NoBatch...
    type CompleteBuild = RunnableBatch -> ActiveBatch

    // only makes sense to do this on batches that failed to build
    // Batch argument should represent the retry-ability
    // Do we need to change current batch all the time in these methods?
    type FailWithoutRetry = RunnableBatch -> AttemptQueue -> (AttemptQueue * ActiveBatch)

    type FailWithRetry = BisectedBatch -> BisectedBatch -> AttemptQueue -> (AttemptQueue * ActiveBatch)

    // SMELL: does this always succeed? What does it's success depend on?
    type CompleteMerge = MergeableBatch -> AttemptQueue -> (AttemptQueue * ActiveBatch)

    type FailMerge = MergeableBatch -> AttemptQueue -> (AttemptQueue * ActiveBatch)


    // These feel kinda like application services... or implementation helpers
    // these may move pull requests SinBin <> AttemptQueue
    // Does there just need to be an UpdateSha? UpdateSha = PullRequestNumber -> SHA -> MergeQueue -> MergeQueue

    // result is keep PR in Sin Bin
    type UpdateShaInSinBin = PullRequestNumber -> SHA -> SinBin -> SinBin
    // result is Maybe move PR to Sin Bin
    type UpdateShaInQueue = PullRequestNumber -> SHA -> AttemptQueue -> SinBin -> (AttemptQueue * SinBin)
    // result is maybe cancel the current batch
    type UpdateShaInRunningBatch = PullRequestNumber -> SHA -> RunnableBatch -> AttemptQueue -> SinBin -> (bool * AttemptQueue * SinBin)

    type UpdateStatusesInSinBin = PullRequestNumber -> SHA -> CommitStatus -> AttemptQueue -> SinBin -> (AttemptQueue * SinBin)
