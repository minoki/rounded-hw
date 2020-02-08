{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}
module Numeric.Rounded.Hardware.Backend.FastFFI
  ( CDouble(..)
  , fastIntervalAdd
  , fastIntervalSub
  , fastIntervalRecip
  , backendName
  , VUM.MVector(MV_CFloat, MV_CDouble)
  , VU.Vector(V_CFloat, V_CDouble)
  ) where
import           Control.DeepSeq (NFData (..))
import           Data.Coerce
import           Data.Proxy
import           Data.Tagged
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified FFIWrapper.Double as D
import           Foreign.Storable (Storable)
import           GHC.Exts (Double (D#), Double#)
import           GHC.Generics (Generic)
import qualified Numeric.Rounded.Hardware.Backend.C as C
import           Numeric.Rounded.Hardware.Internal.Class
import           Unsafe.Coerce

--
-- Double
--

newtype CDouble = CDouble Double
  deriving (Eq,Ord,Show,Generic,Num,Storable)

instance NFData CDouble

instance RoundedRing CDouble where
  roundedAdd = coerce D.roundedAdd
  roundedSub = coerce D.roundedSub
  roundedMul = coerce D.roundedMul
  intervalAdd x x' y y' = coerce fastIntervalAdd x x' y y'
  intervalSub x x' y y' = coerce fastIntervalSub x x' y y'
  intervalMul x x' y y' = (coerce D.intervalMul_down x x' y y', coerce D.intervalMul_up x x' y y')
  roundedFromInteger = coerce (roundedFromInteger :: RoundingMode -> Integer -> C.CDouble)
  intervalFromInteger = coerce (intervalFromInteger :: Integer -> (Rounded 'TowardNegInf C.CDouble, Rounded 'TowardInf C.CDouble))
  backendNameT = Tagged $ "FastFFI+" ++ backendName (Proxy :: Proxy C.CDouble)
  {-# INLINE roundedAdd #-}
  {-# INLINE roundedSub #-}
  {-# INLINE roundedMul #-}
  {-# INLINE intervalAdd #-}
  {-# INLINE intervalSub #-}
  {-# INLINE intervalMul #-}
  {-# INLINE roundedFromInteger #-}
  {-# INLINE intervalFromInteger #-}

instance RoundedFractional CDouble where
  roundedDiv = coerce D.roundedDiv
  intervalDiv x x' y y' = (coerce D.intervalDiv_down x x' y y', coerce D.intervalDiv_up x x' y y')
  intervalRecip x x' = coerce fastIntervalRecip x x'
  roundedFromRational = coerce (roundedFromRational :: RoundingMode -> Rational -> C.CDouble)
  intervalFromRational = coerce (intervalFromRational :: Rational -> (Rounded 'TowardNegInf C.CDouble, Rounded 'TowardInf C.CDouble))
  {-# INLINE roundedDiv #-}
  {-# INLINE intervalDiv #-}
  {-# INLINE intervalRecip #-}
  {-# INLINE roundedFromRational #-}
  {-# INLINE intervalFromRational #-}

instance RoundedSqrt CDouble where
  roundedSqrt = coerce D.roundedSqrt
  {-# INLINE roundedSqrt #-}

instance RoundedVectorOperation CDouble where
  roundedSum_StorableVector mode vec = coerce (roundedSum_StorableVector mode (unsafeCoerce vec) :: C.CDouble)
  roundedSum_UnboxedVector mode vec = coerce (roundedSum_UnboxedVector mode (coerce vec) :: C.CDouble)
  {-# INLINE roundedSum_StorableVector #-}
  {-# INLINE roundedSum_UnboxedVector #-}

--
-- FFI
--

foreign import prim "rounded_hw_interval_add"
  c_rounded_interval_add :: Double# -- lower 1 %xmm1
                         -> Double# -- upper 1 %xmm2
                         -> Double# -- lower 2 %xmm3
                         -> Double# -- upper 2 %xmm4
                         -> (# Double#  -- lower %xmm1
                             , Double#  -- upper %xmm2
                            #)

foreign import prim "rounded_hw_interval_sub"
  c_rounded_interval_sub :: Double# -- lower 1 %xmm1
                         -> Double# -- upper 1 %xmm2
                         -> Double# -- lower 2 %xmm3
                         -> Double# -- upper 2 %xmm4
                         -> (# Double#  -- lower %xmm1
                             , Double#  -- upper %xmm2
                            #)

foreign import prim "rounded_hw_interval_recip"
  c_rounded_interval_recip :: Double# -- lower 1 %xmm1
                           -> Double# -- upper 1 %xmm2
                           -> (# Double#  -- lower %xmm1
                               , Double#  -- upper %xmm2
                              #)

-- TODO: sqrt?

fastIntervalAdd :: Double -> Double -> Double -> Double -> (Double, Double)
fastIntervalAdd (D# l1) (D# h1) (D# l2) (D# h2) = case c_rounded_interval_add l1 h1 l2 h2 of
  (# l3, h3 #) -> (D# l3, D# h3)
{-# INLINE fastIntervalAdd #-}

fastIntervalSub :: Double -> Double -> Double -> Double -> (Double, Double)
fastIntervalSub (D# l1) (D# h1) (D# l2) (D# h2) = case c_rounded_interval_sub l1 h1 l2 h2 of
  (# l3, h3 #) -> (D# l3, D# h3)
{-# INLINE fastIntervalSub #-}

fastIntervalRecip :: Double -> Double -> (Double, Double)
fastIntervalRecip (D# l1) (D# h1) = case c_rounded_interval_recip l1 h1 of
  (# l2, h2 #) -> (D# l2, D# h2)
{-# INLINE fastIntervalRecip #-}

--
-- instance for Data.Vector.Unboxed.Unbox
--

newtype instance VUM.MVector s CDouble = MV_CDouble (VUM.MVector s Double)
newtype instance VU.Vector CDouble = V_CDouble (VU.Vector Double)

instance VGM.MVector VUM.MVector CDouble where
  basicLength (MV_CDouble mv) = VGM.basicLength mv
  basicUnsafeSlice i l (MV_CDouble mv) = MV_CDouble (VGM.basicUnsafeSlice i l mv)
  basicOverlaps (MV_CDouble mv) (MV_CDouble mv') = VGM.basicOverlaps mv mv'
  basicUnsafeNew l = MV_CDouble <$> VGM.basicUnsafeNew l
  basicInitialize (MV_CDouble mv) = VGM.basicInitialize mv
  basicUnsafeReplicate i x = MV_CDouble <$> VGM.basicUnsafeReplicate i (coerce x)
  basicUnsafeRead (MV_CDouble mv) i = coerce <$> VGM.basicUnsafeRead mv i
  basicUnsafeWrite (MV_CDouble mv) i x = VGM.basicUnsafeWrite mv i (coerce x)
  basicClear (MV_CDouble mv) = VGM.basicClear mv
  basicSet (MV_CDouble mv) x = VGM.basicSet mv (coerce x)
  basicUnsafeCopy (MV_CDouble mv) (MV_CDouble mv') = VGM.basicUnsafeCopy mv mv'
  basicUnsafeMove (MV_CDouble mv) (MV_CDouble mv') = VGM.basicUnsafeMove mv mv'
  basicUnsafeGrow (MV_CDouble mv) n = MV_CDouble <$> VGM.basicUnsafeGrow mv n

instance VG.Vector VU.Vector CDouble where
  basicUnsafeFreeze (MV_CDouble mv) = V_CDouble <$> VG.basicUnsafeFreeze mv
  basicUnsafeThaw (V_CDouble v) = MV_CDouble <$> VG.basicUnsafeThaw v
  basicLength (V_CDouble v) = VG.basicLength v
  basicUnsafeSlice i l (V_CDouble v) = V_CDouble (VG.basicUnsafeSlice i l v)
  basicUnsafeIndexM (V_CDouble v) i = coerce <$> VG.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_CDouble mv) (V_CDouble v) = VG.basicUnsafeCopy mv v
  elemseq (V_CDouble v) x y = VG.elemseq v (coerce x) y

instance VU.Unbox CDouble
