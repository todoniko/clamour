namespace Clamour.Sfc

[<AutoOpen>]
module Mixers =
    let inline mix32 (a: byref<uint32>) (b: byref<uint32>) (c: byref<uint32>) (counter: byref<uint32>) : uint32 =
        let temp = a + b + counter in
        counter <- 1u + counter // {BARREL_SHIFT = 21, RSHIFT = 9, LSHIFT = 3}; good sets include {21,9,3},{15,8,3};
        a <- b ^^^ (b >>> 9)
        b <- c + (c <<< 3)
        c <- ((c <<< 21) ||| (c >>> 11)) + temp // 11 = 32 - 21
        temp

    let inline mix64 (a: byref<uint64>) (b: byref<uint64>) (c: byref<uint64>) (counter: byref<uint64>) : uint64 =
        let temp = a + b + counter in
        counter <- counter + 1UL // {BARREL_SHIFT = 24, RSHIFT = 11, LSHIFT = 3};//good sets include {24,11,3},{25,12,3}
        a <- b ^^^ (b >>> 11)
        b <- c + (c <<< 3)
        c <- ((c <<< 24) ||| (c >>> 40)) + temp // 40 = 64 - 24
        temp


type Sfc32(seed1: uint32, seed2: uint32, seed3: uint32) =
    let mutable a = seed1
    let mutable b = seed2
    let mutable c = seed3
    let mutable counter = 1u

    do
        while counter <= 15u do
            mix32 &a &b &c &counter |> ignore

    member _.next() : uint32 = mix32 &a &b &c &counter

    new(seed: uint64) = Sfc32(0u, uint32 (seed >>> 0), uint32 (seed >>> 32))

type Sfc64(seed1: uint64, seed2: uint64, seed3: uint64) =
    let mutable a = seed1
    let mutable b = seed2
    let mutable c = seed3
    let mutable counter = 1UL

    do
        while counter <= 18UL do
            mix64 &a &b &c &counter |> ignore

    member _.next() : uint64 = mix64 &a &b &c &counter

    new(seed: uint64) = Sfc64(seed, seed, seed)
