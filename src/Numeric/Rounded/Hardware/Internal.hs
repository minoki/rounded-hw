{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
module Numeric.Rounded.Hardware.Internal
  ( module Numeric.Rounded.Hardware.Internal
  , module Numeric.Rounded.Hardware.Rounding
  , module Numeric.Rounded.Hardware.Util.Conversion
  , module Numeric.Rounded.Hardware.Util.Show
  ) where
import Numeric.Rounded.Hardware.Rounding
import Numeric.Rounded.Hardware.Util.Conversion
import Numeric.Rounded.Hardware.Util.Show
import Data.Coerce
import Data.Proxy
import Data.Ratio
import GHC.Generics (Generic)
import Control.DeepSeq (NFData(..))

foreign import ccall unsafe "hs_rounded_c99_add_up"
  c_rounded_add_up :: Double -> Double -> Double

foreign import ccall unsafe "hs_rounded_c99_add_down"
  c_rounded_add_down :: Double -> Double -> Double

foreign import ccall unsafe "hs_rounded_c99_add_zero"
  c_rounded_add_zero :: Double -> Double -> Double

foreign import ccall unsafe "hs_rounded_c99_sub_up"
  c_rounded_sub_up :: Double -> Double -> Double

foreign import ccall unsafe "hs_rounded_c99_sub_down"
  c_rounded_sub_down :: Double -> Double -> Double

foreign import ccall unsafe "hs_rounded_c99_sub_zero"
  c_rounded_sub_zero :: Double -> Double -> Double

foreign import ccall unsafe "hs_rounded_c99_mul_up"
  c_rounded_mul_up :: Double -> Double -> Double

foreign import ccall unsafe "hs_rounded_c99_mul_down"
  c_rounded_mul_down :: Double -> Double -> Double

foreign import ccall unsafe "hs_rounded_c99_mul_zero"
  c_rounded_mul_zero :: Double -> Double -> Double

foreign import ccall unsafe "hs_rounded_c99_div_up"
  c_rounded_div_up :: Double -> Double -> Double

foreign import ccall unsafe "hs_rounded_c99_div_down"
  c_rounded_div_down :: Double -> Double -> Double

foreign import ccall unsafe "hs_rounded_c99_div_zero"
  c_rounded_div_zero :: Double -> Double -> Double

foreign import ccall unsafe "hs_rounded_c99_sqrt_up"
  c_rounded_sqrt_up :: Double -> Double

foreign import ccall unsafe "hs_rounded_c99_sqrt_down"
  c_rounded_sqrt_down :: Double -> Double

foreign import ccall unsafe "hs_rounded_c99_sqrt_zero"
  c_rounded_sqrt_zero :: Double -> Double

newtype RoundedDouble (rn :: RoundingMode) = RoundedDouble Double
  deriving (Eq, Ord, Show, Generic)

instance NFData (RoundedDouble rn)

getRoundedDouble :: RoundedDouble rn -> Double
getRoundedDouble (RoundedDouble x) = x

class RoundedPrim (rn :: RoundingMode) where
  rounding :: proxy rn -> RoundingMode
  addDouble :: proxy rn -> Double -> Double -> Double
  subDouble :: proxy rn -> Double -> Double -> Double
  mulDouble :: proxy rn -> Double -> Double -> Double
  divDouble :: proxy rn -> Double -> Double -> Double
  sqrtDouble :: proxy rn -> Double -> Double

instance RoundedPrim TowardNearest where
  rounding _ = TowardNearest
  addDouble _ = (+)
  subDouble _ = (-)
  mulDouble _ = (*)
  divDouble _ = (/)
  sqrtDouble _ = sqrt

instance RoundedPrim TowardInf where
  rounding _ = TowardInf
  addDouble _ = c_rounded_add_up
  subDouble _ = c_rounded_sub_up
  mulDouble _ = c_rounded_mul_up
  divDouble _ = c_rounded_div_up
  sqrtDouble _ = c_rounded_sqrt_up

instance RoundedPrim TowardNegInf where
  rounding _ = TowardNegInf
  addDouble _ = c_rounded_add_down
  subDouble _ = c_rounded_sub_down
  mulDouble _ = c_rounded_mul_down
  divDouble _ = c_rounded_div_down
  sqrtDouble _ = c_rounded_sqrt_down

instance RoundedPrim TowardZero where
  rounding _ = TowardZero
  addDouble _ = c_rounded_add_zero
  subDouble _ = c_rounded_sub_zero
  mulDouble _ = c_rounded_mul_zero
  divDouble _ = c_rounded_div_zero
  sqrtDouble _ = c_rounded_sqrt_zero

instance (RoundedPrim rn) => Num (RoundedDouble rn) where
  lhs@(RoundedDouble x) + RoundedDouble y = RoundedDouble (addDouble lhs x y)
  lhs@(RoundedDouble x) - RoundedDouble y = RoundedDouble (subDouble lhs x y)
  lhs@(RoundedDouble x) * RoundedDouble y = RoundedDouble (mulDouble lhs x y)
  negate = coerce (negate :: Double -> Double)
  abs = coerce (abs :: Double -> Double)
  signum = coerce (signum :: Double -> Double)
  fromInteger n = RoundedDouble (fromInt (rounding (Proxy :: Proxy rn)) n)

instance (RoundedPrim rn) => Fractional (RoundedDouble rn) where
  fromRational x
    | abs (numerator x) <= 2^53 && abs (denominator x) <= 2^53
    = let n' = fromInteger (numerator x)
          d' = fromInteger (denominator x)
      in RoundedDouble (divDouble (Proxy :: Proxy rn) n' d')
    | otherwise = RoundedDouble $ fromRatio (rounding (Proxy :: Proxy rn)) (numerator x) (denominator x)
  recip a@(RoundedDouble x) = RoundedDouble (divDouble a 1 x)
  lhs@(RoundedDouble x) / RoundedDouble y = RoundedDouble (divDouble lhs x y)

foreign import ccall unsafe "nextafter" c_nextafter :: Double -> Double -> Double
