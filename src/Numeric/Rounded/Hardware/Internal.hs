{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
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
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM

foreign import ccall unsafe "rounded_hw_add"
  c_rounded_add :: Int -> Double -> Double -> Double

foreign import ccall unsafe "rounded_hw_add_up"
  c_rounded_add_up :: Double -> Double -> Double

foreign import ccall unsafe "rounded_hw_add_down"
  c_rounded_add_down :: Double -> Double -> Double

foreign import ccall unsafe "rounded_hw_add_zero"
  c_rounded_add_zero :: Double -> Double -> Double

foreign import ccall unsafe "rounded_hw_sub"
  c_rounded_sub :: Int -> Double -> Double -> Double

foreign import ccall unsafe "rounded_hw_sub_up"
  c_rounded_sub_up :: Double -> Double -> Double

foreign import ccall unsafe "rounded_hw_sub_down"
  c_rounded_sub_down :: Double -> Double -> Double

foreign import ccall unsafe "rounded_hw_sub_zero"
  c_rounded_sub_zero :: Double -> Double -> Double

foreign import ccall unsafe "rounded_hw_mul"
  c_rounded_mul :: Int -> Double -> Double -> Double

foreign import ccall unsafe "rounded_hw_mul_up"
  c_rounded_mul_up :: Double -> Double -> Double

foreign import ccall unsafe "rounded_hw_mul_down"
  c_rounded_mul_down :: Double -> Double -> Double

foreign import ccall unsafe "rounded_hw_mul_zero"
  c_rounded_mul_zero :: Double -> Double -> Double

foreign import ccall unsafe "rounded_hw_div"
  c_rounded_div :: Int -> Double -> Double -> Double

foreign import ccall unsafe "rounded_hw_div_up"
  c_rounded_div_up :: Double -> Double -> Double

foreign import ccall unsafe "rounded_hw_div_down"
  c_rounded_div_down :: Double -> Double -> Double

foreign import ccall unsafe "rounded_hw_div_zero"
  c_rounded_div_zero :: Double -> Double -> Double

foreign import ccall unsafe "rounded_hw_sqrt"
  c_rounded_sqrt :: Int -> Double -> Double

foreign import ccall unsafe "rounded_hw_sqrt_up"
  c_rounded_sqrt_up :: Double -> Double

foreign import ccall unsafe "rounded_hw_sqrt_down"
  c_rounded_sqrt_down :: Double -> Double

foreign import ccall unsafe "rounded_hw_sqrt_zero"
  c_rounded_sqrt_zero :: Double -> Double

newtype RoundedDouble (rn :: RoundingMode) = RoundedDouble Double
  deriving (Eq, Ord, Show, Generic)

instance NFData (RoundedDouble rn)

getRoundedDouble :: RoundedDouble rn -> Double
getRoundedDouble (RoundedDouble x) = x

class RoundedPrim (rn :: RoundingMode) where
  rounding :: proxy rn -> RoundingMode

addDouble :: (RoundedPrim rn) => proxy rn -> Double -> Double -> Double
addDouble proxy x y = c_rounded_add (fromEnum (rounding proxy)) x y
{-# INLINE [1] addDouble #-}
subDouble :: (RoundedPrim rn) => proxy rn -> Double -> Double -> Double
subDouble proxy x y = c_rounded_sub (fromEnum (rounding proxy)) x y
{-# INLINE [1] subDouble #-}
mulDouble :: (RoundedPrim rn) => proxy rn -> Double -> Double -> Double
mulDouble proxy x y = c_rounded_mul (fromEnum (rounding proxy)) x y
{-# INLINE [1] mulDouble #-}
divDouble :: (RoundedPrim rn) => proxy rn -> Double -> Double -> Double
divDouble proxy x y = c_rounded_div (fromEnum (rounding proxy)) x y
{-# INLINE [1] divDouble #-}
{-# RULES
"addDouble/TowardNegInf" [~1] forall (proxy :: Proxy TowardNegInf). addDouble proxy = c_rounded_add_down
"addDouble/TowardInf"    [~1] forall (proxy :: Proxy TowardInf).    addDouble proxy = c_rounded_add_up
"subDouble/TowardNegInf" [~1] forall (proxy :: Proxy TowardNegInf). subDouble proxy = c_rounded_sub_down
"subDouble/TowardInf"    [~1] forall (proxy :: Proxy TowardInf).    subDouble proxy = c_rounded_sub_up
"mulDouble/TowardNegInf" [~1] forall (proxy :: Proxy TowardNegInf). mulDouble proxy = c_rounded_mul_down
"mulDouble/TowardInf"    [~1] forall (proxy :: Proxy TowardInf).    mulDouble proxy = c_rounded_mul_up
"divDouble/TowardNegInf" [~1] forall (proxy :: Proxy TowardNegInf). divDouble proxy = c_rounded_div_down
"divDouble/TowardInf"    [~1] forall (proxy :: Proxy TowardInf).    divDouble proxy = c_rounded_div_up
#-}

instance RoundedPrim TowardNearest where
  rounding _ = TowardNearest

instance RoundedPrim TowardInf where
  rounding _ = TowardInf

instance RoundedPrim TowardNegInf where
  rounding _ = TowardNegInf

instance RoundedPrim TowardZero where
  rounding _ = TowardZero

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

newtype instance UM.MVector s (RoundedDouble rn) = MV_RoundedDouble (UM.MVector s Double)
newtype instance U.Vector (RoundedDouble rn) = V_RoundedDouble (U.Vector Double)

instance GM.MVector UM.MVector (RoundedDouble rn) where
  basicLength (MV_RoundedDouble mv) = GM.basicLength mv
  basicUnsafeSlice i l (MV_RoundedDouble mv) = MV_RoundedDouble (GM.basicUnsafeSlice i l mv)
  basicOverlaps (MV_RoundedDouble mv) (MV_RoundedDouble mv') = GM.basicOverlaps mv mv'
  basicUnsafeNew l = MV_RoundedDouble <$> GM.basicUnsafeNew l
  basicInitialize (MV_RoundedDouble mv) = GM.basicInitialize mv
  basicUnsafeReplicate i x = MV_RoundedDouble <$> GM.basicUnsafeReplicate i (coerce x)
  basicUnsafeRead (MV_RoundedDouble mv) i = coerce <$> GM.basicUnsafeRead mv i
  basicUnsafeWrite (MV_RoundedDouble mv) i x = GM.basicUnsafeWrite mv i (coerce x)
  basicClear (MV_RoundedDouble mv) = GM.basicClear mv
  basicSet (MV_RoundedDouble mv) x = GM.basicSet mv (coerce x)
  basicUnsafeCopy (MV_RoundedDouble mv) (MV_RoundedDouble mv') = GM.basicUnsafeCopy mv mv'
  basicUnsafeMove (MV_RoundedDouble mv) (MV_RoundedDouble mv') = GM.basicUnsafeMove mv mv'
  basicUnsafeGrow (MV_RoundedDouble mv) n = MV_RoundedDouble <$> GM.basicUnsafeGrow mv n

instance G.Vector U.Vector (RoundedDouble rn) where
  basicUnsafeFreeze (MV_RoundedDouble mv) = V_RoundedDouble <$> G.basicUnsafeFreeze mv
  basicUnsafeThaw (V_RoundedDouble v) = MV_RoundedDouble <$> G.basicUnsafeThaw v
  basicLength (V_RoundedDouble v) = G.basicLength v
  basicUnsafeSlice i l (V_RoundedDouble v) = V_RoundedDouble (G.basicUnsafeSlice i l v)
  basicUnsafeIndexM (V_RoundedDouble v) i = coerce <$> G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_RoundedDouble mv) (V_RoundedDouble v) = G.basicUnsafeCopy mv v
  elemseq (V_RoundedDouble v) x y = G.elemseq v (coerce x) y

instance U.Unbox (RoundedDouble rn)
