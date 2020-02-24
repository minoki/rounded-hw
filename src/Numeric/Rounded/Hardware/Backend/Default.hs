{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-unused-imports #-}
module Numeric.Rounded.Hardware.Backend.Default
  () where
import           Numeric.Rounded.Hardware.Internal.Class
import qualified Numeric.Rounded.Hardware.Backend.ViaRational as VR
#ifdef USE_FFI
import qualified Numeric.Rounded.Hardware.Backend.C as C
#ifdef USE_GHC_PRIM
import qualified Numeric.Rounded.Hardware.Backend.FastFFI as FastFFI
#endif
#ifdef USE_X87_LONG_DOUBLE
import           Numeric.Rounded.Hardware.Backend.X87LongDouble ()
#endif
#endif
import qualified Data.Vector.Storable as VS
import           Unsafe.Coerce
import           Data.Coerce

#ifdef USE_FFI
#ifdef USE_GHC_PRIM
type FloatImpl = C.CFloat -- TODO: Provide FastFFI.CFloat
type DoubleImpl = FastFFI.CDouble
#else
type FloatImpl = C.CFloat
type DoubleImpl = C.CDouble
#endif
#else
type FloatImpl = VR.ViaRational Float
type DoubleImpl = VR.ViaRational Double
#endif

deriving via FloatImpl instance RoundedRing Float
deriving via FloatImpl instance RoundedFractional Float
deriving via FloatImpl instance RoundedSqrt Float

deriving via DoubleImpl instance RoundedRing Double
deriving via DoubleImpl instance RoundedFractional Double
deriving via DoubleImpl instance RoundedSqrt Double

-- orphaned rules
{-# RULES
"fromIntegral/a->Rounded ToNearest Float"
  forall x. fromIntegral x = Rounded (roundedFromInteger ToNearest (fromIntegral x)) :: Rounded 'ToNearest Float
"fromIntegral/a->Rounded TowardInf Float"
  forall x. fromIntegral x = Rounded (roundedFromInteger TowardInf (fromIntegral x)) :: Rounded 'TowardInf Float
"fromIntegral/a->Rounded TowardNegInf Float"
  forall x. fromIntegral x = Rounded (roundedFromInteger TowardNegInf (fromIntegral x)) :: Rounded 'TowardNegInf Float
"fromIntegral/a->Rounded TowardZero Float"
  forall x. fromIntegral x = Rounded (roundedFromInteger TowardZero (fromIntegral x)) :: Rounded 'TowardZero Float
"fromIntegral/a->Rounded ToNearest Double"
  forall x. fromIntegral x = Rounded (roundedFromInteger ToNearest (fromIntegral x)) :: Rounded 'ToNearest Double
"fromIntegral/a->Rounded TowardInf Double"
  forall x. fromIntegral x = Rounded (roundedFromInteger TowardInf (fromIntegral x)) :: Rounded 'TowardInf Double
"fromIntegral/a->Rounded TowardNegInf Double"
  forall x. fromIntegral x = Rounded (roundedFromInteger TowardNegInf (fromIntegral x)) :: Rounded 'TowardNegInf Double
"fromIntegral/a->Rounded TowardZero Double"
  forall x. fromIntegral x = Rounded (roundedFromInteger TowardZero (fromIntegral x)) :: Rounded 'TowardZero Double
  #-}

instance RoundedVectorOperation Float where
  roundedSum_StorableVector mode vec = coerce (roundedSum_StorableVector mode (unsafeCoerce vec :: VS.Vector FloatImpl))
  roundedSum_UnboxedVector mode vec = coerce (roundedSum_UnboxedVector mode (coerce vec) :: FloatImpl)

instance RoundedVectorOperation Double where
  roundedSum_StorableVector mode vec = coerce (roundedSum_StorableVector mode (unsafeCoerce vec :: VS.Vector DoubleImpl))
  roundedSum_UnboxedVector mode vec = coerce (roundedSum_UnboxedVector mode (coerce vec) :: DoubleImpl)
