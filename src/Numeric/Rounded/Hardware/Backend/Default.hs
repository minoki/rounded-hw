{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-unused-imports #-}
module Numeric.Rounded.Hardware.Backend.Default () where
import           Numeric.Rounded.Hardware.Base.Class
import           Numeric.Rounded.Hardware.Backend.ViaRational
#ifdef USE_FFI
import qualified Numeric.Rounded.Hardware.Backend.C as C
#ifdef USE_GHC_PRIM
import qualified Numeric.Rounded.Hardware.Backend.FastFFI as FastFFI
#endif
#endif

#ifdef USE_FFI
#ifdef USE_GHC_PRIM
type FloatImpl = C.CFloat -- TODO: Provide FastFFI.CFloat
type DoubleImpl = FastFFI.CDouble
#else
type FloatImpl = C.CFloat
type DoubleImpl = C.CDouble
#endif
#else
type FloatImpl = ViaRational Float
type DoubleImpl = ViaRational Double
#endif

deriving via FloatImpl instance RoundedRing Float
deriving via FloatImpl instance RoundedFractional Float
#ifdef USE_FFI
deriving via FloatImpl instance RoundedSqrt Float
#endif

deriving via DoubleImpl instance RoundedRing Double
deriving via DoubleImpl instance RoundedFractional Double
#ifdef USE_FFI
deriving via DoubleImpl instance RoundedSqrt Double
#endif
