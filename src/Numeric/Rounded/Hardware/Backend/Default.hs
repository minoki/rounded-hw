{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
module Numeric.Rounded.Hardware.Backend.Default where
import Numeric.Rounded.Hardware.Class
import           Numeric.Rounded.Hardware.Backend.ViaRational
#ifdef USE_FFI
import qualified Numeric.Rounded.Hardware.Backend.C as C
#ifdef USE_GHC_PRIM
import qualified Numeric.Rounded.Hardware.Backend.FastFFI as FastFFI
#endif
#endif

#ifdef USE_FFI
#ifdef USE_GHC_PRIM
type DoubleImpl = FastFFI.CDouble
#else
type DoubleImpl = C.CDouble
#endif
#else
type DoubleImpl = ViaRational Double
#endif

deriving via DoubleImpl instance RoundedRing Double
deriving via DoubleImpl instance RoundedField Double
#ifdef USE_FFI
deriving via DoubleImpl instance RoundedSqrt Double
#endif
