{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GHCForeignImportPrim       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UnboxedTuples              #-}
{-# LANGUAGE UnliftedFFITypes           #-}
module Numeric.Rounded.Hardware.Backend.FastFFI
  (CDouble(..)
  ,fastIntervalAdd
  ,fastIntervalSub
  ,fastIntervalRecip
  ) where
import           Control.DeepSeq                             (NFData (..))
import           Data.Coerce
import           Data.Functor.Product
import           Data.Ratio
import           FFIImports
import qualified FFIWrapper.Double                           as D
import qualified FFIWrapper.Float                            as F
import           GHC.Exts
import           GHC.Generics                                (Generic)
import           Numeric.Rounded.Hardware.Class
import           Numeric.Rounded.Hardware.Rounding
import           Numeric.Rounded.Hardware.Util.Constants
import           Numeric.Rounded.Hardware.Util.Conversion

--
-- Double
--

newtype CDouble = CDouble Double
  deriving (Eq,Ord,Show,Generic,Num)

instance NFData CDouble

instance RoundedRing CDouble where
  roundedAdd = coerce D.roundedAdd
  roundedSub = coerce D.roundedSub
  roundedMul = coerce D.roundedMul
  intervalAdd x x' y y' = coerce fastIntervalAdd x x' y y'
  intervalSub x x' y y' = coerce fastIntervalSub x x' y y'
  intervalMul x x' y y' = (coerce c_interval_mul_double_down x x' y y', coerce c_interval_mul_double_up x x' y y')
  roundedFromInteger rn x = CDouble (fromInt rn x)
  intervalFromInteger x = case fromIntF x :: Product (Rounded 'TowardNegInf) (Rounded 'TowardInf) Double of
    Pair a b -> (CDouble <$> a, CDouble <$> b)
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
  intervalDiv x x' y y' = (coerce c_interval_div_double_down x x' y y', coerce c_interval_div_double_up x x' y y')
  intervalRecip x x' = coerce fastIntervalRecip x x'
  roundedFromRational rn x = CDouble $ fromRatio rn (numerator x) (denominator x)
  intervalFromRational x = case fromRatioF (numerator x) (denominator x) :: Product (Rounded 'TowardNegInf) (Rounded 'TowardInf) Double of
    Pair a b -> (CDouble <$> a, CDouble <$> b)
  {-# INLINE roundedDiv #-}
  {-# INLINE intervalDiv #-}
  {-# INLINE intervalRecip #-}
  {-# INLINE roundedFromRational #-}
  {-# INLINE intervalFromRational #-}

instance RoundedSqrt CDouble where
  roundedSqrt = coerce D.roundedSqrt
  {-# INLINE roundedSqrt #-}

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
