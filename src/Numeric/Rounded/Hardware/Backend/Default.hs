{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import qualified Data.Vector.Unboxed as VU
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
deriving via FloatImpl instance RoundedRing_Vector VU.Vector Float
deriving via FloatImpl instance RoundedFractional_Vector VU.Vector Float
deriving via FloatImpl instance RoundedSqrt_Vector VU.Vector Float

instance RoundedRing_Vector VS.Vector Float where
  roundedSum mode vec = coerce (roundedSum mode (unsafeCoerce vec :: VS.Vector FloatImpl))
  zipWith_roundedAdd mode vec vec' = unsafeCoerce (zipWith_roundedAdd mode (unsafeCoerce vec) (unsafeCoerce vec') :: VS.Vector FloatImpl)
  zipWith_roundedSub mode vec vec' = unsafeCoerce (zipWith_roundedSub mode (unsafeCoerce vec) (unsafeCoerce vec') :: VS.Vector FloatImpl)
  zipWith_roundedMul mode vec vec' = unsafeCoerce (zipWith_roundedMul mode (unsafeCoerce vec) (unsafeCoerce vec') :: VS.Vector FloatImpl)
  {-# INLINE roundedSum #-}
  {-# INLINE zipWith_roundedAdd #-}
  {-# INLINE zipWith_roundedSub #-}
  {-# INLINE zipWith_roundedMul #-}

instance RoundedFractional_Vector VS.Vector Float where
  zipWith_roundedDiv mode vec vec' = unsafeCoerce (zipWith_roundedDiv mode (unsafeCoerce vec) (unsafeCoerce vec') :: VS.Vector FloatImpl)
  {-# INLINE zipWith_roundedDiv #-}

instance RoundedSqrt_Vector VS.Vector Float where
  map_roundedSqrt mode vec = unsafeCoerce (map_roundedSqrt mode (unsafeCoerce vec) :: VS.Vector FloatImpl)
  {-# INLINE map_roundedSqrt #-}

deriving via DoubleImpl instance RoundedRing Double
deriving via DoubleImpl instance RoundedFractional Double
deriving via DoubleImpl instance RoundedSqrt Double
deriving via DoubleImpl instance RoundedRing_Vector VU.Vector Double
deriving via DoubleImpl instance RoundedFractional_Vector VU.Vector Double
deriving via DoubleImpl instance RoundedSqrt_Vector VU.Vector Double

instance RoundedRing_Vector VS.Vector Double where
  roundedSum mode vec = coerce (roundedSum mode (unsafeCoerce vec :: VS.Vector DoubleImpl))
  zipWith_roundedAdd mode vec vec' = unsafeCoerce (zipWith_roundedAdd mode (unsafeCoerce vec) (unsafeCoerce vec') :: VS.Vector DoubleImpl)
  zipWith_roundedSub mode vec vec' = unsafeCoerce (zipWith_roundedSub mode (unsafeCoerce vec) (unsafeCoerce vec') :: VS.Vector DoubleImpl)
  zipWith_roundedMul mode vec vec' = unsafeCoerce (zipWith_roundedMul mode (unsafeCoerce vec) (unsafeCoerce vec') :: VS.Vector DoubleImpl)
  {-# INLINE roundedSum #-}
  {-# INLINE zipWith_roundedAdd #-}
  {-# INLINE zipWith_roundedSub #-}
  {-# INLINE zipWith_roundedMul #-}

instance RoundedFractional_Vector VS.Vector Double where
  zipWith_roundedDiv mode vec vec' = unsafeCoerce (zipWith_roundedDiv mode (unsafeCoerce vec) (unsafeCoerce vec') :: VS.Vector DoubleImpl)
  {-# INLINE zipWith_roundedDiv #-}

instance RoundedSqrt_Vector VS.Vector Double where
  map_roundedSqrt mode vec = unsafeCoerce (map_roundedSqrt mode (unsafeCoerce vec) :: VS.Vector DoubleImpl)
  {-# INLINE map_roundedSqrt #-}

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
