namespace MergeQueue

open MergeQueue.DomainTypes

module DbTypes =
    type Load = unit -> MergeQueue

    type Save = MergeQueue -> unit
