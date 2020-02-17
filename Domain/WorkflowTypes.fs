namespace MergeQueue.WorkflowTypes

open MergeQueue.DomainServiceTypes

module Enqueue =
    type Command =
        { number: int
          sha: string
          statuses: List<string * string> }

    // TODO: convert to some kind of command DTO instead of using the domain type?
    type Success = EnqueueSuccess

    type Error =
        | ValidationError of string
        | EnqueueError of EnqueueError

    type EnqueueResult = Result<Success, Error>

    type EnqueueWorkflow = Command -> EnqueueResult
 
 module Dequeue =
    type Command =
        { number: int }

    type Success = DequeueSuccess

    type Error =
        | ValidationError of string
        | DequeueError of DequeueError

    type DequeueResult = Result<Success, Error>

    type DequeueWorkflow = Command -> DequeueResult

module StartBatch =
    type Command = unit

    type Success = StartBatchSuccess

    type Error = StartBatchError of StartBatchError

    type StartBatchResult = Result<Success, Error>

    type StartBatchWorkflow = Command -> StartBatchResult

module IngestBuild =
    // TODO: Make this more like a DTO (i.e. not a DU?)
    type UnvalidatedBuildMessage =
        | Success of string // the SHA of the failed builds commit
        | Failure // TODO: how do we know which SHA failed? do we care?

    type Command =
        { message: UnvalidatedBuildMessage }

    // SMELL: Success type name clashes with Success constructor for BuildMessage
    type Success = IngestBuildSuccess

    // TODO: Validation error goes here
    type Error =
        | ValidationError of string
        | IngestBuildError of IngestBuildError

    type IngestBuildResult = Result<Success, Error>

    type IngestBuildWorkflow = Command -> IngestBuildResult

module IngestMerge =
    // TODO: This should be a DTO that represents the Batch merged result
    type UnvalidatedMergeMessage =
        | Success
        | Failure

    type Command =
        { message: UnvalidatedMergeMessage }

    type Success = IngestMergeSuccess

    type Error = IngestMergeError of IngestMergeError

    type IngestMergeResult = Result<IngestMergeSuccess, Error>

    type IngestMergeWorkflow = Command -> IngestMergeResult

module UpdatePullRequest =
    type Command =
        { number: int
          sha: string }

    type Success = UpdatePullRequestSuccess

    type Error = ValidationError of string

    type UpdatePullRequestResult = Result<Success, Error>

    type UpdatePullRequestWorkflow = Command -> UpdatePullRequestResult

module UpdateStatuses =
    type Command =
        { number: int
          sha: string
          statuses: List<string * string> }

    type Success = UpdateStatusesSuccess

    type Error = ValidationError of string

    type UpdateStatusResult = Result<Success, Error>

    type UpdateStatusWorkflow = Command -> UpdateStatusResult
