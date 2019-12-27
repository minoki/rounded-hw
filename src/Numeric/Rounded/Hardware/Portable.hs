{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Numeric.Rounded.Hardware.Portable where
import Numeric.Rounded.Hardware.Rounding
import Numeric.Rounded.Hardware.Util.Conversion
import Data.Coerce
import Data.Proxy
import Data.Ratio
import GHC.Generics (Generic)
import Control.DeepSeq (NFData(..))
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM

newtype RoundedDouble (rn :: RoundingMode) = RoundedDouble Double
  deriving (Eq, Ord, Show, Generic)

instance NFData (RoundedDouble rn)

getRoundedDouble :: RoundedDouble rn -> Double
getRoundedDouble (RoundedDouble x) = x

instance (Rounding rn) => Num (RoundedDouble rn) where
  -- TODO: Handle negative zero
  RoundedDouble x + RoundedDouble y
    | isNaN x || isNaN y || isInfinite x || isInfinite y = RoundedDouble (x + y)
    | x == 0 && y == 0 = if isNegativeZero x == isNegativeZero y
                         then RoundedDouble x
                         else case rounding (Proxy :: Proxy rn) of
                                TowardNearest -> RoundedDouble 0
                                TowardNegInf -> RoundedDouble (-0)
                                TowardInf -> RoundedDouble 0
                                TowardZero -> RoundedDouble 0
    | otherwise = fromRational (toRational x + toRational y)
  RoundedDouble x - RoundedDouble y
    | isNaN x || isNaN y || isInfinite x || isInfinite y = RoundedDouble (x - y)
    | x == 0 && y == 0 = if isNegativeZero x /= isNegativeZero y
                         then RoundedDouble x
                         else case rounding (Proxy :: Proxy rn) of
                                TowardNearest -> RoundedDouble 0
                                TowardNegInf -> RoundedDouble (-0)
                                TowardInf -> RoundedDouble 0
                                TowardZero -> RoundedDouble 0
    | otherwise = fromRational (toRational x - toRational y)
  RoundedDouble x * RoundedDouble y
    | isNaN x || isNaN y || isInfinite x || isInfinite y || isNegativeZero x || isNegativeZero y = RoundedDouble (x * y)
    | otherwise = fromRational (toRational x * toRational y)
  negate = coerce (negate :: Double -> Double)
  abs = coerce (abs :: Double -> Double)
  signum = coerce (signum :: Double -> Double)
  fromInteger n = RoundedDouble (fromInt (rounding (Proxy :: Proxy rn)) n)

instance (Rounding rn) => Fractional (RoundedDouble rn) where
  fromRational x = RoundedDouble $ fromRatio (rounding (Proxy :: Proxy rn)) (numerator x) (denominator x)
  RoundedDouble x / RoundedDouble y | isNaN x || isNaN y || isInfinite x || isInfinite y || x == 0 || y == 0 = RoundedDouble (x / y)
                                    | otherwise = fromRational (toRational x / toRational y)

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
