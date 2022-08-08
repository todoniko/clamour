//module Benchmark

open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open Clamour.Pcg

[<MemoryDiagnoser>]
type RandomBench() =
    let seed = 0xFFFFFFFFFFFFFFC5UL // 2^64-59, largest 64bit prime

    let r = System.Random()
    let stream = new Stream(seed)
    let uniq = new Uniq(seed)
    let mutable explicit_state = Oneseq.init seed

    [<Benchmark>]
    member _.Pcg_stream_xs_rr() = stream.next ()

    [<Benchmark>]
    member _.Pcg_uniq_rxs_m_xs() = uniq.next ()

    [<Benchmark(Baseline = true)>]
    member _.Pcg_fast_xs_rr() = Oneseq.next &explicit_state

    [<Benchmark>]
    member _.System_random() = r.Next()

[<MemoryDiagnoser>]
type Permuters() =
    let primes =
        Array.map (* 2^64 - k *)
            (fun k -> 0xFFFFFFFFFFFFFFFFUL - (uint64 k))
            [| 59
               83
               95
               179
               189
               257
               279
               323
               353
               363 |]

    [<Benchmark>]
    member _.permute_xs_rr() = Array.map xsh_rr primes

    [<Benchmark(Baseline = true)>]
    member _.permute_rxs_m_xs() = Array.map rxs_m_xs primes

let defaultSwitch () =
    BenchmarkSwitcher [| typeof<RandomBench>
                         typeof<Permuters> |]
//typeof<OtherBench> |]

[<EntryPoint>]
let main args =
    defaultSwitch().Run args |> ignore
    0
