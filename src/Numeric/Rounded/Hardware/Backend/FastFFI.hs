{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}
module Numeric.Rounded.Hardware.Backend.FastFFI
  ( CDouble(..)
  , fastIntervalAdd
  , fastIntervalSub
  , fastIntervalRecip
  , VUM.MVector(MV_CFloat, MV_CDouble)
  , VU.Vector(V_CFloat, V_CDouble)
  ) where
import           Control.DeepSeq (NFData (..))
import           Data.Coerce
import           Data.Proxy
import           Data.Tagged
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified FFIWrapper.Double as D
import           Foreign.C.String (CString, peekCString)
import           Foreign.Storable (Storable)
import           GHC.Exts
import           GHC.Generics (Generic)
import           GHC.Int (Int64 (I64#))
import           GHC.Word (Word64 (W64#))
import qualified Numeric.Rounded.Hardware.Backend.C as C
import           Numeric.Rounded.Hardware.Internal.Class
import           System.IO.Unsafe (unsafePerformIO)
import           Unsafe.Coerce

#include "MachDeps.h"

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
  roundedFusedMultiplyAdd = coerce D.roundedFMA
  intervalAdd x x' y y' = coerce fastIntervalAdd x x' y y'
  intervalSub x x' y y' = coerce fastIntervalSub x x' y y'
  intervalMul x x' y y' = (coerce D.intervalMul_down x x' y y', coerce D.intervalMul_up x x' y y')
  intervalMulAdd x x' y y' z z' = (coerce D.intervalMulAdd_down x x' y y' z, coerce D.intervalMulAdd_up x x' y y' z')
  roundedFromInteger = coerce (roundedFromInteger :: RoundingMode -> Integer -> C.CDouble)
  intervalFromInteger = coerce (intervalFromInteger :: Integer -> (Rounded 'TowardNegInf C.CDouble, Rounded 'TowardInf C.CDouble))
  backendNameT = Tagged $ let base = backendName (Proxy :: Proxy C.CDouble)
                              intervals = intervalBackendName
                          in if base == intervals
                             then base ++ "+FastFFI"
                             else base ++ "+FastFFI(" ++ intervals ++ ")"
  {-# INLINE roundedAdd #-}
  {-# INLINE roundedSub #-}
  {-# INLINE roundedMul #-}
  {-# INLINE roundedFusedMultiplyAdd #-}
  {-# INLINE intervalAdd #-}
  {-# INLINE intervalSub #-}
  {-# INLINE intervalMul #-}
  {-# INLINE roundedFromInteger #-}
  {-# INLINE intervalFromInteger #-}

instance RoundedFractional CDouble where
  roundedDiv = coerce D.roundedDiv
  intervalDiv x x' y y' = (coerce D.intervalDiv_down x x' y y', coerce D.intervalDiv_up x x' y y')
  intervalDivAdd x x' y y' z z' = (coerce D.intervalDivAdd_down x x' y y' z, coerce D.intervalDivAdd_up x x' y y' z')
  intervalRecip x x' = coerce fastIntervalRecip x x'
  roundedFromRational = coerce (roundedFromRational :: RoundingMode -> Rational -> C.CDouble)
  roundedFromRealFloat r x = coerce (roundedFromRealFloat r x :: C.CDouble)
  intervalFromRational = coerce (intervalFromRational :: Rational -> (Rounded 'TowardNegInf C.CDouble, Rounded 'TowardInf C.CDouble))
  {-# INLINE roundedDiv #-}
  {-# INLINE intervalDiv #-}
  {-# INLINE intervalRecip #-}
  {-# INLINE roundedFromRational #-}
  {-# INLINE roundedFromRealFloat #-}
  {-# INLINE intervalFromRational #-}

instance RoundedSqrt CDouble where
  roundedSqrt = coerce D.roundedSqrt
  {-# INLINE roundedSqrt #-}

instance RoundedRing_Vector VS.Vector CDouble where
  roundedSum mode vec = coerce (roundedSum mode (unsafeCoerce vec :: VS.Vector C.CDouble))
  zipWith_roundedAdd mode vec vec' = unsafeCoerce (zipWith_roundedAdd mode (unsafeCoerce vec) (unsafeCoerce vec') :: VS.Vector C.CDouble)
  zipWith_roundedSub mode vec vec' = unsafeCoerce (zipWith_roundedSub mode (unsafeCoerce vec) (unsafeCoerce vec') :: VS.Vector C.CDouble)
  zipWith_roundedMul mode vec vec' = unsafeCoerce (zipWith_roundedMul mode (unsafeCoerce vec) (unsafeCoerce vec') :: VS.Vector C.CDouble)
  {-# INLINE roundedSum #-}
  {-# INLINE zipWith_roundedAdd #-}
  {-# INLINE zipWith_roundedSub #-}
  {-# INLINE zipWith_roundedMul #-}

instance RoundedFractional_Vector VS.Vector CDouble where
  zipWith_roundedDiv mode vec vec' = unsafeCoerce (zipWith_roundedDiv mode (unsafeCoerce vec) (unsafeCoerce vec') :: VS.Vector C.CDouble)
  {-# INLINE zipWith_roundedDiv #-}

instance RoundedSqrt_Vector VS.Vector CDouble where
  map_roundedSqrt mode vec = unsafeCoerce (map_roundedSqrt mode (unsafeCoerce vec) :: VS.Vector C.CDouble)
  {-# INLINE map_roundedSqrt #-}

deriving via C.CDouble instance RoundedRing_Vector VU.Vector CDouble
deriving via C.CDouble instance RoundedFractional_Vector VU.Vector CDouble
deriving via C.CDouble instance RoundedSqrt_Vector VU.Vector CDouble

--
-- FFI
--

foreign import prim "rounded_hw_interval_add"
  fastIntervalAdd# :: Double# -- lower 1, %xmm1
                   -> Double# -- upper 1, %xmm2
                   -> Double# -- lower 2, %xmm3
                   -> Double# -- upper 2, %xmm4
                   -> (# Double#  -- lower, %xmm1
                       , Double#  -- upper, %xmm2
                       #)

foreign import prim "rounded_hw_interval_sub"
  fastIntervalSub# :: Double# -- lower 1, %xmm1
                   -> Double# -- upper 1, %xmm2
                   -> Double# -- lower 2, %xmm3
                   -> Double# -- upper 2, %xmm4
                   -> (# Double#  -- lower, %xmm1
                       , Double#  -- upper, %xmm2
                       #)

foreign import prim "rounded_hw_interval_recip"
  fastIntervalRecip# :: Double# -- lower 1, %xmm1
                     -> Double# -- upper 1, %xmm2
                     -> (# Double#  -- lower, %xmm1
                         , Double#  -- upper, %xmm2
                         #)

foreign import prim "rounded_hw_interval_sqrt"
  fastIntervalSqrt# :: Double# -- lower 1, %xmm1
                    -> Double# -- upper 1, %xmm2
                    -> (# Double#  -- lower, %xmm1
                        , Double#  -- upper, %xmm2
                        #)

#if WORD_SIZE_IN_BITS >= 64
type INT64# = Int#
type WORD64# = Word#
#else
type INT64# = Int64#
type WORD64# = Word64#
#endif

foreign import prim "rounded_hw_interval_from_int64"
  fastIntervalFromInt64# :: INT64# -- value
                         -> (# Double# -- lower, %xmm1
                             , Double# -- upper, %xmm2
                             #)

{-
foreign import prim "rounded_hw_interval_from_word64"
  fastIntervalFromWord64# :: WORD64# -- value
                          -> (# Double# -- lower, %xmm1
                              , Double# -- upper, %xmm2
                              #)
-}

fastIntervalAdd :: Double -> Double -> Double -> Double -> (Double, Double)
fastIntervalAdd (D# l1) (D# h1) (D# l2) (D# h2) = case fastIntervalAdd# l1 h1 l2 h2 of
  (# l3, h3 #) -> (D# l3, D# h3)
{-# INLINE fastIntervalAdd #-}

fastIntervalSub :: Double -> Double -> Double -> Double -> (Double, Double)
fastIntervalSub (D# l1) (D# h1) (D# l2) (D# h2) = case fastIntervalSub# l1 h1 l2 h2 of
  (# l3, h3 #) -> (D# l3, D# h3)
{-# INLINE fastIntervalSub #-}

fastIntervalRecip :: Double -> Double -> (Double, Double)
fastIntervalRecip (D# l1) (D# h1) = case fastIntervalRecip# l1 h1 of
  (# l2, h2 #) -> (D# l2, D# h2)
{-# INLINE fastIntervalRecip #-}

fastIntervalSqrt :: Double -> Double -> (Double, Double)
fastIntervalSqrt (D# l1) (D# h1) = case fastIntervalSqrt# l1 h1 of
  (# l2, h2 #) -> (D# l2, D# h2)
{-# INLINE fastIntervalSqrt #-}

fastIntervalFromInt64 :: Int64 -> (Double, Double)
fastIntervalFromInt64 (I64# x) = case fastIntervalFromInt64# x of
  (# l, h #) -> (D# l, D# h)
{-# INLINE fastIntervalFromInt64 #-}

{-
fastIntervalFromWord64 :: Word64 -> (Double, Double)
fastIntervalFromWord64 (W64# x) = case fastIntervalFromWord64# x of
  (# l, h #) -> (D# l, D# h)
{-# INLINE fastIntervalFromWord64 #-}
-}

--
-- Backend name
--

foreign import ccall "&rounded_hw_interval_backend_name"
  c_interval_backend_name :: CString

intervalBackendName :: String
intervalBackendName = unsafePerformIO (peekCString c_interval_backend_name)

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
