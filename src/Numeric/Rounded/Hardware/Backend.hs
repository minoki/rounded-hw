{-|
Module: Numeric.Rounded.Hardware.Backend

Although popular CPUs allow program to control the rounding direction of floating-point operations,
such feature is not directly accessible to Haskell.

Several options are available to control the rounding direction, including

    * Emulate the operations using 'Rational'.
    * Emulate the desired rounding behavior using the default rounding direction (i.e. round to nearest).
    * Provide the rounding-direction-controlled operations in C or assembly, and use FFI to call them from Haskell.

        * C FFI is portable, but has limitations (e.g. cannot return multiple values directly).
        * GHC-specific @foreign import prim@ can return multiple values efficiently, but cannot be implemented in C.

This library implements the first and third options, in "Numeric.Rounded.Hardware.Backend.ViaRational" and "Numeric.Rounded.Hardware.Backend.C"/"Numeric.Rounded.Hardware.Backend.FastFFI" respectively.

The default implementation for 'Float' and 'Double' depends on the platform and package flags.
To help the programmer identify which implementation is used, this module provides a function to obtain the name of implementation.

To disable use of FFI, enable the package flag @pure-hs@.
-}
module Numeric.Rounded.Hardware.Backend (backendName) where
import           Numeric.Rounded.Hardware.Internal (backendName)
