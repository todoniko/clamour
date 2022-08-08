namespace Clamour.Sfc

type Sfc32(seed1: uint32, seed2: uint32, seed3: uint32) as this =
    let mutable a = seed1
    let mutable b = seed2
    let mutable c = seed3
    let mutable counter = 1u

    do
        while counter <= 15u do
            this.next ()

    member _.next() : uint32 =
        let temp = a + b + counter in
        counter <- 1u + counter // {BARREL_SHIFT = 21, RSHIFT = 9, LSHIFT = 3}; good sets include {21,9,3},{15,8,3};
        a <- b ^^^ (b >>> 9)
        b <- c + (c <<< 3)
        c <- ((c <<< 21) ||| (c >>> 11)) + temp // 11 = 32 - 21
        temp

    new(seed: uint64) = Sfc32(0u, uint32 (seed >>> 0), uint32 (seed >>> 32))
