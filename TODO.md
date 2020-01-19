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

Backlog
1.  cancel a PR
1.  check reviews accepted of PR when enqueue
1.  check target branch of PR matches Merge Queue target branch
1.  priority
1.  accept status for PRs as well as staging branch builds
1.  max batch size
1.  paused
1.  target branch is updated

Icebox
1.  dequeue an unstarted PR
1.  dequeue a running PR
1.  list events by PR
1.  improve robustness around missed messages? when a build goes quiet?
1.  handle mismatch between build/merge batches and merge queue state?
    1. this is in some kind of lost message scenario, domain should worry about this?