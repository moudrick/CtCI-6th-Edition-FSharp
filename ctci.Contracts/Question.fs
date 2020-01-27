namespace ctci.Contracts

[<AbstractClass>]
type Question() =
    abstract member DemoRun : unit -> unit
    member this.Name with get() = this.GetType().ToString()
