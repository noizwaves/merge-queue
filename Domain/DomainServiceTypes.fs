namespace MergeQueue

open DomainTypes

module DomainServiceTypes =
    type Command<'a> =
        { command: 'a
          aggregate: MergeQueue }

    type AggregateSuccess<'b> =
        { success: 'b
          aggregate: MergeQueue }

    type DomainService<'a, 'b, 'error> = Command<'a> -> Result<AggregateSuccess<'b>, 'error>

    /// Enqueue
    /// - add a PR to the queue
    type EnqueueSuccess =
        | Enqueued
        | SinBinned

    type EnqueueError =
        | RejectedFailingBuildStatus
        | AlreadyEnqueued

    type Enqueue = DomainService<PullRequest, EnqueueSuccess, EnqueueError>

    /// Dequeue
    /// - remove a PR from the merge queue
    type DequeueSuccess =
        | Dequeued
        // TODO: make a type for AbortedBatch
        | DequeuedAndAbortRunningBatch of List<PullRequest> * PullRequestNumber

    type DequeueError =
        | RejectedInMergingBatch
        | NotFound

    type Dequeue = DomainService<PullRequestNumber, DequeueSuccess, DequeueError>

    /// Start Batch
    /// - Start a batch building
    type StartBatchSuccess = BatchStarted of List<PullRequest>

    type StartBatchError =
        | AlreadyRunning
        | EmptyQueue

    type StartBatch = DomainService<unit, StartBatchSuccess, StartBatchError>

    /// Ingest Build Update
    /// - when a Batch's build finishes
    type BuildProgress =
        | BuildSuccess of SHA
        | BuildFailure

    type IngestBuildSuccess =
        | SuccessfullyBuilt of List<PullRequest> * SHA
        | BuildFailureWillRetry of List<PullRequest>
        | BuildFailureWontRetry of List<PullRequest>

    type IngestBuildError = NotCurrentlyBuilding

    type IngestBuildProgress = DomainService<BuildProgress, IngestBuildSuccess, IngestBuildError>

    /// Ingest Merge Update
    /// - when a Batch's merge finishes
    type MergeProgress =
        | Success
        | Failure

    type IngestMergeSuccess =
        | SuccessfullyMerged of List<PullRequest>
        | MergeFailure of List<PullRequest>

    type IngestMergeError = NotCurrentlyMerging

    type IngestMergeUpdate = DomainService<MergeProgress, IngestMergeSuccess, IngestMergeError>

    /// Update Pull Request
    /// - when a PR's branch changes
    type PullRequestUpdate =
        { number: PullRequestNumber
          sha: SHA }

    type UpdatePullRequestSuccess =
        // TODO: Expand with the other successful outcomes, like moved to Sin Bin
        | NoChange
        | AbortRunningBatch of List<PullRequest> * PullRequestNumber
        | AbortMergingBatch of List<PullRequest> * PullRequestNumber

    type UpdatePullRequest = Command<PullRequestUpdate> -> AggregateSuccess<UpdatePullRequestSuccess>

    /// Update Statuses
    /// - when the status of a PR's branch changes
    type UpdateStatusesSuccess = NoChange

    type StatusUpdate =
        { number: PullRequestNumber
          sha: SHA
          status: CommitStatus }

    type UpdateStatus = Command<StatusUpdate> -> AggregateSuccess<UpdateStatusesSuccess>

    type PreviewExecutionPlan = MergeQueue -> ExecutionPlan
