# merge-queue

A theoretical merge queue implementation

## Requirements

1.  dotnet core 3.1.101

## Using

### Using the sandbox web server

1.  `dotnet run -p SandboxApp/SandboxApp.fsproj`

### Interactive play with the domain

Launch an F# Interactive session and play with the domain

1.  `dotnet fsi`
1.  `#load "Domain/Domain.fs";;`
1.  `open MergeQueue.Domain;;`
1.  Start with an empty batch via
    ```
    emptyMergeQueue;;
    ```
1.  Add a PR to the queue and start a batch via
    ```
    it
    |> enqueue (pullRequest (pullRequestId 1) (sha "00001111"))
    |> snd
    |> startBatch;;
    ```
1.  Add another PR to the queue via
    ```
    it
    |> snd
    |> enqueue (pullRequest (pullRequestId 22) (sha "00002222"));;
    ```
1.  Fail the current batch via
    ```
    it
    |> snd
    |> ingestBuildUpdate (BuildMessage.Failure);;
    ```
1.  Etc...
1.  Leave the session via `#quit;;`

Useful functions include:
-   `peekCurrentQueue`
-   `peekCurrentBatch`
-   `previewBatches`

## Tests

`dotnet test`
