namespace Clamour.Pcg

[<AutoOpen>]
module Permute =
    let inline xsh_rr (state: uint64) : uint32 =
        let xst = uint32 (((state >>> 18) ^^^ state) >>> 27) in
        let rot = uint32 (state >>> 59) in

        (xst >>> (int32) rot)
        ||| (xst <<< ((-(int32) rot) &&& 31))

    let inline rxs_m_xs (state: uint64) : uint64 =
        let shift = int (state >>> 59) + 5 in

        let word =
            ((state >>> shift) ^^^ state)
            * 0xAEF17502108EF2D9UL in

        (word >>> 43) ^^^ word

module Oneseq =
    let init seed = seed * 0x5851F42D4C957F2DUL

    let inline next (state: byref<uint64>) =
        let current = state in
        state <- current * 0x5851F42D4C957F2DUL
        xsh_rr current

[<Sealed>]
type Stream(seed: uint64, ?stream: uint64) =
    let inc =
        match stream with
        | Some s -> s <<< 1 ||| 1UL
        | None -> 0x14057B7EF767814FUL // Must be odd

    let mutable state = (inc + seed) * 0x5851F42D4C957F2DUL + inc

    member _.next() =
        let pcg = xsh_rr state
        state <- state * 0x5851F42D4C957F2DUL + inc
        pcg

    member _.split() =
        let stream = (inc >>> 1) + 1UL in new Stream(state, stream)

[<Sealed>]
type Uniq(seed: uint64, ?stream: uint64) =
    let inc =
        match stream with
        | Some s -> s <<< 1 ||| 1UL
        | None -> 0x14057B7EF767814FUL // Must be odd

    let mutable state = (inc + seed) * 0x5851F42D4C957F2DUL + inc

    member _.next() =
        let pcg = rxs_m_xs state
        state <- state * 0x5851F42D4C957F2DUL + inc
        pcg
