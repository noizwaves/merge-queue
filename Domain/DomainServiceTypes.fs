namespace MergeQueue

open DomainTypes

module DomainServiceTypes =
    type DomainService<'a, 'b, 'error> = 'a -> MergeQueue -> Result<'b * MergeQueue, 'error>

    type EnqueueSuccess =
        | Enqueued
        | SinBinned

    type EnqueueError =
        | RejectedFailingBuildStatus
        | AlreadyEnqueued

    type Enqueue = DomainService<PullRequest, EnqueueSuccess, EnqueueError>

    // TODO: make a type for AbortedBatch
    // TODO: Remove PullRequestNumber from success tuple
    type DequeueSuccess =
        | Dequeued
        | DequeuedAndAbortRunningBatch of List<PullRequest> * PullRequestNumber

    type DequeueError =
        | RejectedInMergingBatch
        | NotFound

    type Dequeue = DomainService<PullRequestNumber, DequeueSuccess, DequeueError>

    // feels more like = IdleQueue -> Option<Batch>, no reason to allow running or merging queues to be started
    // Maybe it shouldn't be a command
    type StartBatchSuccess = PerformBatchBuild of List<PullRequest>

    type StartBatchError =
        | AlreadyRunning
        | EmptyQueue

    type StartBatch = DomainService<unit, StartBatchSuccess, StartBatchError>

    // TODO: make names less general, more specific to build operation
    // it's actually an update, not a message
    type BuildMessage =
        | Success of SHA
        | Failure

    type IngestBuildSuccess =
        | PerformBatchMerge of List<PullRequest> * SHA
        | ReportBuildFailureWithRetry of List<PullRequest>
        | ReportBuildFailureNoRetry of List<PullRequest>

    type IngestBuildError = NotCurrentlyBuilding

    type IngestBuildUpdate = DomainService<BuildMessage, IngestBuildSuccess, IngestBuildError>

    type MergeMessage =
        | Success
        | Failure

    type IngestMergeSuccess =
        | MergeComplete of List<PullRequest>
        | ReportMergeFailure of List<PullRequest>

    type IngestMergeError = NotCurrentlyMerging

    type IngestMergeUpdate = DomainService<MergeMessage, IngestMergeSuccess, IngestMergeError>

    // TODO: Expand with the other successful outcomes, like moved to Sin Bin
    type UpdatePullRequestSuccess =
        | NoChange
        | AbortRunningBatch of List<PullRequest> * PullRequestNumber
        | AbortMergingBatch of List<PullRequest> * PullRequestNumber

    type PullRequestUpdate =
        { number: PullRequestNumber
          sha: SHA }

    type UpdatePullRequest = PullRequestUpdate -> MergeQueue -> (UpdatePullRequestSuccess * MergeQueue)

    type UpdateStatusesSuccess = NoChange

    type StatusUpdate =
        { number: PullRequestNumber
          sha: SHA
          statuses: CommitStatuses }

    type UpdateStatuses = StatusUpdate -> MergeQueue -> (UpdateStatusesSuccess * MergeQueue)

    type PreviewExecutionPlan = MergeQueue -> ExecutionPlan


