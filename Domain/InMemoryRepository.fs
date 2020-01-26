namespace MergeQueue

module InMemoryRepository =
    open MergeQueue.DomainTypes
    open MergeQueue.Domain

    type private TData =
        { mutable item: MergeQueue }

    type T = private T of TData

    let create (_: unit): T =
        T { item = MergeQueue.empty }

    let save (T t) (value: MergeQueue): unit =
        t.item <- value

    let load (T t) (_: unit): MergeQueue =
        t.item
