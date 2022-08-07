//module Benchmark

open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open Clamour.Pcg

module List =
    let inline myLength xs =
        let rec loop xs acc =
            match xs with
            | [] -> acc
            | _ :: tail -> loop tail (acc + 1)

        loop xs 0

[<MemoryDiagnoser>]
type RandomBench() =
    let seed = 18446744073709551557UL

    let r = System.Random()
    let stream = new Stream(seed, 1UL)
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
