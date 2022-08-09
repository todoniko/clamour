namespace Clamour.Pcg

[<AutoOpen>]
module Permute =
    [<Literal>] // multiplier for permutation `rxs_m_xs`
    let per_mul = 0xAEF17502108EF2D9UL

    [<Literal>] // multiplier for generators LCG/MCG
    let gen_mul = 0x5851F42D4C957F2DUL

    [<Literal>] // default increment for LCG
    let def_inc = 0x14057B7EF767814FUL

    // xorshift -> random rotate
    let inline xsh_rr (state: uint64) : uint32 =
        let xst = uint32 (((state >>> 18) ^^^ state) >>> 27) in
        let rot = uint32 (state >>> 59) in

        (xst >>> (int32) rot)
        ||| (xst <<< ((-(int32) rot) &&& 31))

    // random xorshift -> multiply -> xorshift
    let inline rxs_m_xs (state: uint64) : uint64 =
        let shift = int (state >>> 59) + 5 in

        let word = ((state >>> shift) ^^^ state) * per_mul in

        (word >>> 43) ^^^ word

// Explicit state prng with underlying MCG, must be the fastes of the bunch
module Oneseq =
    let init seed = seed * gen_mul

    let inline next (state: byref<uint64>) =
        let current = state in
        state <- current * gen_mul
        xsh_rr current

[<Sealed>] // Splitable prng with LCG under the hood
type Stream(seed: uint64, ?stream: uint64) =
    let inc =
        match stream with
        | Some s -> s <<< 1 ||| 1UL
        | None -> def_inc // Must be odd

    let mutable state = (inc + seed) * gen_mul + inc

    member _.next() =
        let pcg = xsh_rr state
        state <- state * gen_mul + inc
        pcg

    member _.split() =
        let stream = (inc >>> 1) + 1UL in new Stream(state, stream)

[<Sealed>] // Splitable prng with LCG inside and no output repeats
type Uniq(seed: uint64, ?stream: uint64) =
    let inc =
        match stream with
        | Some s -> s <<< 1 ||| 1UL
        | None -> def_inc // Must be odd

    let mutable state = (inc + seed) * gen_mul + inc

    member _.next() =
        let pcg = rxs_m_xs state
        state <- state * gen_mul + inc
        pcg

    member _.split() =
        let stream = (inc >>> 1) + 1UL in new Uniq(state, stream)
