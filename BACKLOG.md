Done
1.  create an empty queue
1.  enqueue a PR
1.  enqueue a second PR
1.  start a batch
1.  finish a batch
1.  receive build step
1.  generate command for merging PRs
1.  generate command for building staging branch
1.  handle updates to PRs
1.  merge is able to fail if target head has changed since build
1.  bisecting
1.  retrying
1.  preview batches
1.  check GitHub build status of PR when enqueue
1.  prevent updated branches from starting until statuses have succeeded again
1.  fire and forget PRs
1.  dequeue a PR

1.  Refactor domain into Types & Functions & "Services"
    1.  Split `SimpleSandboxApi.fs` into UI(web) and use cases(non web)
    1.  change commands and some domain functions to from Try & Maybe -> X to Do -> Result<X>
    1.  somehow return "Domain Events" out of commands, like BatchStarted, PullRequestEnqueued, etc
    1.  GOAL(?): domain functions are not unwrapping types

Backlog
1.  view where my PR is
1.  add assertions on the thrown away results
1.  failed single batch PRs are not dequeued only sin binned
1.  cancel a batch
1.  check reviews accepted of PR when enqueue
1.  check target branch of PR matches Merge Queue target branch
1.  priority
1.  max batch size
1.  paused
1.  target branch is updated

Icebox
1.  list events by PR
1.  improve robustness around missed messages? when a build goes quiet?
1.  handle mismatch between build/merge batches and merge queue state?
    1. this is in some kind of lost message scenario, domain should worry about this?
    
Ideas
1.  BuildBatchCommand / StartBatchBuildEvent / BatchBuildStartedEvent?
1.  MergeBatchCommand / StartBatchMergeEvent / BatchMergeStartedEvent?
1.  bootstrap system from Open PRs & most recent "merge queue" comment!
    -   recovery or init
    -   either "enqueue" or "dequeue"
    -   result is an alternative way to construct an initial MergeQueueState

Read
- https://fsharpforfunandprofit.com/posts/concurrency-actor-model/
- https://fsharpforfunandprofit.com/posts/recipe-part3/
