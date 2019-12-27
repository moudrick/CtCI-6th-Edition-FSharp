namespace ctci.Contracts

[<AbstractClass>]
type Question() =
    abstract member Run : unit -> unit
    member this.Name with get() = this.GetType().ToString()
