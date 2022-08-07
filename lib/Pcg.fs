﻿namespace Clamour.Pcg

[<AutoOpen>]
module Permute =
    let inline xsh_rr (state: uint64) : uint32 =
        let xst = uint32 (((state >>> 18) ^^^ state) >>> 27) in
        let rot = uint32 (state >>> 59) in

        (xst >>> (int32) rot)
        ||| (xst <<< ((-(int32) rot) &&& 31))

module Oneseq =
    let init seed = seed * 0x5851F42D4C957F2DUL

    let inline next (state: byref<uint64>) =
        let current = state in
        state <- current * 0x5851F42D4C957F2DUL
        xsh_rr current

type Stream(seed: uint64, stream: uint64) =
    let inc = stream <<< 1 ||| 1UL // Must be odd
    let mutable state = (inc + seed) * 0x5851F42D4C957F2DUL + inc

    member _.next() =
        let pcg = xsh_rr state
        state <- state * 0x5851F42D4C957F2DUL + inc
        pcg