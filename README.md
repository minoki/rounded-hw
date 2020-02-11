# rounded-hw

This package provides directed rounding and interval arithmetic for built-in floating point types (i.e. `Float`, `Double`).
Unlike [rounded](https://hackage.haskell.org/package/rounded), this package does not depend on an external C library.

The standard `Double` and `Float` are supported.
In addition, `LongDouble` from [long-double](https://hackage.haskell.org/package/long-double) package is supported on x86.

# API overview

* type `RoundingMode` = `TowardNearest` | `TowardNegInf` | `TowardInf` | `TowardZero`
* type `Rounded r a`
    * `a` is typically `Double` or `Float`.
    * Methods of `Num` and `Fractional` are overloaded to honor the specified rounding mode.
    * Complex expressions like `x - y * z` does not yield correctly-rounded result in general.
    * In particular, `-0.1 :: Rounded r Double` doesn't yield the correct value if `r = TowardInf, TowardNegInf`. Use `fromRational (-0.1)` or `NegativeLiterals` in that case.
    * `Floating` is not supported for now.
* type `Interval a`
    * Provides interval arithmetic using directed rounding.
    * `a` is typically `Double` or `Float`.

# Backends

Backends for directed rounding are:

* Pure Haskell (via `Rational`)
    * Very slow, but does not depend on FFI and therefore can be used on non-native backends.
    * This implementation is always available via a newtype in `Numeric.Rounded.Hardware.Backend.ViaRational`.
* C FFI
    * One of the technologies below is used:
        * C99 (`fesetround`)
        * SSE2 (`_mm_setcsr`)
        * AVX512 (`_mm_*_round_*`)
        * x87 FSTCW, FLDCW (for x87 long double)
    * On x86_64, `foreign import prim` is used to provide faster interval addition/subtraction.

By default, C FFI is used and an appropriate technology is detected.
To disable use of C FFI, set `pure-hs` flag when building.
