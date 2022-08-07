//module Benchmark

open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open Clamour.Pcg

[<MemoryDiagnoser>]
type RandomBench() =
    let seed = 0xFFFFFFFFFFFFFFC5UL // 2^64-59, largest 64bit prime

    let r = System.Random()
    let stream = new Stream(seed)
    let mutable explicit_state = Oneseq.init seed

    [<Benchmark>]
    member _.Pcg_stream_xs_rr() = stream.next ()

    [<Benchmark>]
    member _.Pcg_fast_xs_rr() = Oneseq.next &explicit_state

    [<Benchmark(Baseline = true)>]
    member _.System_random() = r.Next()

let defaultSwitch () =
    BenchmarkSwitcher [| typeof<RandomBench> |]
//typeof<OtherBench> |]

[<EntryPoint>]
let main args =
    defaultSwitch().Run args |> ignore
    0
