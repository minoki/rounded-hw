# rounded-hw: Rounding control for built-in floating-point types

This package provides directed rounding and interval arithmetic for built-in floating-point types (i.e. `Float`, `Double`).
Unlike [rounded](https://hackage.haskell.org/package/rounded), this package does not depend on an external C library.

In addition to `Float` and `Double`, `LongDouble` from [long-double](https://hackage.haskell.org/package/long-double) package is supported on x86.

# API overview

## Controlling the rounding direction

The type `RoundingMode` represents the four rounding directions.

The type `Rounded (r :: RoundingMode) a` is a wrapper for `a`, with instances honoring the rounding direction given by `r`.

```haskell
module Numeric.Rounded.Hardware where

data RoundingMode
  = ToNearest     -- ^ Round to the nearest value (IEEE754 roundTiesToEven)
  | TowardNegInf  -- ^ Round downward (IEEE754 roundTowardNegative)
  | TowardInf     -- ^ Round upward (IEEE754 roundTowardPositive)
  | TowardZero    -- ^ Round toward zero (IEEE754 roundTowardZero)

newtype Rounded (r :: RoundingMode) a = Rounded { getRounded :: a }

instance ... => Num (Rounded r a)
instance ... => Fractional (Rounded r a)
instance ... => Real (Rounded r a)
instance ... => RealFrac (Rounded r a)
```

## Interval arithmetic

This library also provides basic interval types. See `Numeric.Rounded.Hardware.Interval` and `Numeric.Rounded.Hardware.Interval.NonEmpty`.

# Usage

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE HexFloatLiterals #-}
import Numeric
import Numeric.Rounded.Hardware

main = do
  putStrLn $ showHFloat (1 + 0x1p-100 :: Double) "" -- -> 0x1p0
  putStrLn $ showHFloat (1 + 0x1p-100 :: Rounded TowardInf Double) "" -- -> 0x1.0000000000001p0
```

# Backends

There are several options to control the rounding direction.

* Pure Haskell (via `Rational`)
    * Very slow, but does not depend on FFI and therefore can be used on non-native backends.
    * This implementation is always available via a newtype in `Numeric.Rounded.Hardware.Backend.ViaRational`.
* C FFI
    * One of the technologies below is used:
        * C99 (`fesetround`)
        * SSE2 (`_mm_setcsr`)
        * AVX512 EVEX encoding (`_mm_*_round_*`)
        * x87 Control Word (for x87 long double)
        * AArch64 FPCR
    * On x86_64, `foreign import prim` is used to provide faster interval addition/subtraction.

By default, C FFI is used and an appropriate technology is detected.
To disable use of C FFI, set `pure-hs` flag when building.

The name of the backend used can be obtained with `Numeric.Rounded.Hardware.Backend.backendName`.

```haskell
>>> backendName (Proxy :: Proxy Double)
"FastFFI+SSE2"
```
