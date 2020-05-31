{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Numeric.Rounded.Hardware.Internal.Class
  ( module Numeric.Rounded.Hardware.Internal.Class
  , module Numeric.Rounded.Hardware.Internal.Rounding
  ) where
import           Data.Coerce
import           Data.Proxy
import           Data.Ratio
import           Data.Tagged
import qualified Data.Vector.Generic as VG
import           Foreign.Storable (Storable)
import           Numeric.Rounded.Hardware.Internal.Rounding
import           Prelude hiding (fromInteger, fromRational, recip, sqrt, (*),
                          (+), (-), (/))
import qualified Prelude

class Ord a => RoundedRing a where
  roundedAdd :: RoundingMode -> a -> a -> a
  roundedSub :: RoundingMode -> a -> a -> a
  roundedMul :: RoundingMode -> a -> a -> a
  roundedFromInteger :: RoundingMode -> Integer -> a
  -- roundedToFloat :: RoundingMode -> a -> Float
  -- roundedToDouble :: RoundingMode -> a -> Double

  intervalAdd :: Rounded 'TowardNegInf a -> Rounded 'TowardInf a -> Rounded 'TowardNegInf a -> Rounded 'TowardInf a -> (Rounded 'TowardNegInf a, Rounded 'TowardInf a)
  intervalAdd x_lo x_hi y_lo y_hi = (x_lo + y_lo, x_hi + y_hi)
    where (+) :: forall r. Rounding r => Rounded r a -> Rounded r a -> Rounded r a
          Rounded x + Rounded y = Rounded (roundedAdd (rounding (Proxy :: Proxy r)) x y)
  intervalSub :: Rounded 'TowardNegInf a -> Rounded 'TowardInf a -> Rounded 'TowardNegInf a -> Rounded 'TowardInf a -> (Rounded 'TowardNegInf a, Rounded 'TowardInf a)
  intervalSub x_lo x_hi y_lo y_hi = (x_lo - coerce y_hi, x_hi - coerce y_lo)
    where (-) :: forall r. Rounding r => Rounded r a -> Rounded r a -> Rounded r a
          Rounded x - Rounded y = Rounded (roundedSub (rounding (Proxy :: Proxy r)) x y)
  intervalMul :: Rounded 'TowardNegInf a -> Rounded 'TowardInf a -> Rounded 'TowardNegInf a -> Rounded 'TowardInf a -> (Rounded 'TowardNegInf a, Rounded 'TowardInf a)
  intervalMul x_lo x_hi y_lo y_hi
    = ( minimum [        x_lo *        y_lo
                ,        x_lo * coerce y_hi
                , coerce x_hi *        y_lo
                , coerce x_hi * coerce y_hi
                ]
      , maximum [ coerce x_lo * coerce y_lo
                , coerce x_lo *        y_hi
                ,        x_hi * coerce y_lo
                ,        x_hi *        y_hi
                ]
      )
    where (*) :: forall r. Rounding r => Rounded r a -> Rounded r a -> Rounded r a
          Rounded x * Rounded y = Rounded (roundedMul (rounding (Proxy :: Proxy r)) x y)
  intervalMulAdd :: Rounded 'TowardNegInf a -> Rounded 'TowardInf a -> Rounded 'TowardNegInf a -> Rounded 'TowardInf a -> Rounded 'TowardNegInf a -> Rounded 'TowardInf a -> (Rounded 'TowardNegInf a, Rounded 'TowardInf a)
  intervalMulAdd x_lo x_hi y_lo y_hi z_lo z_hi = case intervalMul x_lo x_hi y_lo y_hi of
                                                   (xy_lo, xy_hi) -> intervalAdd xy_lo xy_hi z_lo z_hi
  intervalFromInteger :: Integer -> (Rounded 'TowardNegInf a, Rounded 'TowardInf a)
  intervalFromInteger x = (fromInteger x, fromInteger x)
    where fromInteger :: forall r. Rounding r => Integer -> Rounded r a
          fromInteger y = Rounded (roundedFromInteger (rounding (Proxy :: Proxy r)) y)
  {-# INLINE intervalAdd #-}
  {-# INLINE intervalSub #-}
  {-# INLINE intervalMul #-}
  {-# INLINE intervalFromInteger #-}

  backendNameT :: Tagged a String

backendName :: RoundedRing a => proxy a -> String
backendName = Data.Tagged.proxy backendNameT
{-# INLINE backendName #-}

class RoundedRing a => RoundedFractional a where
  roundedDiv :: RoundingMode -> a -> a -> a
  roundedRecip :: RoundingMode -> a -> a
  default roundedRecip :: Num a => RoundingMode -> a -> a
  roundedRecip r = roundedDiv r 1
  roundedFromRational :: RoundingMode -> Rational -> a
  roundedFromRealFloat :: RealFloat b => RoundingMode -> b -> a
  default roundedFromRealFloat :: (Fractional a, RealFloat b) => RoundingMode -> b -> a
  roundedFromRealFloat r x | isNaN x = 0 Prelude./ 0
                           | isInfinite x = if x > 0 then 1 Prelude./ 0 else -1 Prelude./ 0
                           | isNegativeZero x = -0
                           | otherwise = roundedFromRational r (toRational x)
  intervalDiv :: Rounded 'TowardNegInf a -> Rounded 'TowardInf a -> Rounded 'TowardNegInf a -> Rounded 'TowardInf a -> (Rounded 'TowardNegInf a, Rounded 'TowardInf a)
  intervalDiv x_lo x_hi y_lo y_hi
    = ( minimum [        x_lo /        y_lo
                ,        x_lo / coerce y_hi
                , coerce x_hi /        y_lo
                , coerce x_hi / coerce y_hi
                ]
      , maximum [ coerce x_lo / coerce y_lo
                , coerce x_lo /        y_hi
                ,        x_hi / coerce y_lo
                ,        x_hi /        y_hi
                ]
      )
    where (/) :: forall r. Rounding r => Rounded r a -> Rounded r a -> Rounded r a
          Rounded x / Rounded y = Rounded (roundedDiv (rounding (Proxy :: Proxy r)) x y)
  intervalDivAdd :: Rounded 'TowardNegInf a -> Rounded 'TowardInf a -> Rounded 'TowardNegInf a -> Rounded 'TowardInf a -> Rounded 'TowardNegInf a -> Rounded 'TowardInf a -> (Rounded 'TowardNegInf a, Rounded 'TowardInf a)
  intervalDivAdd x_lo x_hi y_lo y_hi z_lo z_hi = case intervalDiv x_lo x_hi y_lo y_hi of
                                                   (xy_lo, xy_hi) -> intervalAdd xy_lo xy_hi z_lo z_hi
  intervalRecip :: Rounded 'TowardNegInf a -> Rounded 'TowardInf a -> (Rounded 'TowardNegInf a, Rounded 'TowardInf a)
  intervalRecip x_lo x_hi = (recip (coerce x_hi), recip (coerce x_lo))
    where recip :: forall r. Rounding r => Rounded r a -> Rounded r a
          recip (Rounded x) = Rounded (roundedRecip (rounding (Proxy :: Proxy r)) x)
  intervalFromRational :: Rational -> (Rounded 'TowardNegInf a, Rounded 'TowardInf a)
  intervalFromRational x = (fromRational x, fromRational x)
    where fromRational :: forall r. Rounding r => Rational -> Rounded r a
          fromRational y = Rounded (roundedFromRational (rounding (Proxy :: Proxy r)) y)
  {-# INLINE intervalDiv #-}
  {-# INLINE intervalRecip #-}
  {-# INLINE intervalFromRational #-}

class RoundedRing a => RoundedSqrt a where
  roundedSqrt :: RoundingMode -> a -> a
  intervalSqrt :: Rounded 'TowardNegInf a -> Rounded 'TowardInf a -> (Rounded 'TowardNegInf a, Rounded 'TowardInf a)
  intervalSqrt x y = (sqrt x, sqrt y)
    where sqrt :: forall r. Rounding r => Rounded r a -> Rounded r a
          sqrt (Rounded z) = Rounded (roundedSqrt (rounding (Proxy :: Proxy r)) z)
  {-# INLINE intervalSqrt #-}

class RoundedRing a => RoundedRing_Vector vector a where
  roundedSum :: RoundingMode -> vector a -> a
  zipWith_roundedAdd :: RoundingMode -> vector a -> vector a -> vector a
  zipWith_roundedSub :: RoundingMode -> vector a -> vector a -> vector a
  zipWith_roundedMul :: RoundingMode -> vector a -> vector a -> vector a

  default roundedSum :: (VG.Vector vector a, Num a) => RoundingMode -> vector a -> a
  roundedSum mode = VG.foldl' (roundedAdd mode) 0

  default zipWith_roundedAdd :: (VG.Vector vector a) => RoundingMode -> vector a -> vector a -> vector a
  zipWith_roundedAdd mode = VG.zipWith (roundedAdd mode)

  default zipWith_roundedSub :: (VG.Vector vector a) => RoundingMode -> vector a -> vector a -> vector a
  zipWith_roundedSub mode = VG.zipWith (roundedSub mode)

  default zipWith_roundedMul :: (VG.Vector vector a) => RoundingMode -> vector a -> vector a -> vector a
  zipWith_roundedMul mode = VG.zipWith (roundedMul mode)

class (RoundedFractional a, RoundedRing_Vector vector a) => RoundedFractional_Vector vector a where
  zipWith_roundedDiv :: RoundingMode -> vector a -> vector a -> vector a
  map_roundedRecip :: RoundingMode -> vector a -> vector a

class (RoundedRing a, RoundedRing_Vector vector a) => RoundedSqrt_Vector vector a where
  map_roundedSqrt :: RoundingMode -> vector a -> vector a

instance (Rounding r, Num a, RoundedRing a) => Num (Rounded r a) where
  Rounded x + Rounded y = Rounded (roundedAdd (rounding (Proxy :: Proxy r)) x y)
  Rounded x - Rounded y = Rounded (roundedSub (rounding (Proxy :: Proxy r)) x y)
  Rounded x * Rounded y = Rounded (roundedMul (rounding (Proxy :: Proxy r)) x y)
  negate = coerce (negate :: a -> a)
  abs = coerce (abs :: a -> a)
  signum = coerce (signum :: a -> a)
  fromInteger x = Rounded (roundedFromInteger (rounding (Proxy :: Proxy r)) x)
  {-# INLINE (+) #-}
  {-# INLINE (-) #-}
  {-# INLINE (*) #-}
  {-# INLINE negate #-}
  {-# INLINE abs #-}
  {-# INLINE signum #-}
  {-# INLINE fromInteger #-}

instance (Rounding r, Num a, RoundedFractional a) => Fractional (Rounded r a) where
  Rounded x / Rounded y = Rounded (roundedDiv (rounding (Proxy :: Proxy r)) x y)
  recip (Rounded x) = Rounded (roundedRecip (rounding (Proxy :: Proxy r)) x)
  fromRational x = Rounded (roundedFromRational (rounding (Proxy :: Proxy r)) x)
  {-# INLINE (/) #-}
  {-# INLINE recip #-}
  {-# INLINE fromRational #-}

deriving newtype instance (Rounding r, Real a, RoundedFractional a) => Real (Rounded r a)
deriving newtype instance (Rounding r, RealFrac a, RoundedFractional a) => RealFrac (Rounded r a)

-- These instances are provided in Numeric.Rounded.Hardware.Backend.Default:
--   instance RoundedRing Float
--   instance RoundedFractional Float
--   instance RoundedSqrt Float
--   instance RoundedRing Double
--   instance RoundedFractional Double
--   instance RoundedSqrt Double

instance RoundedRing Integer where
  roundedAdd _ = (Prelude.+)
  roundedSub _ = (Prelude.-)
  roundedMul _ = (Prelude.*)
  roundedFromInteger _ = id
  backendNameT = Tagged "Integer"

instance RoundedFractional Integer where
  roundedDiv r x y = roundedFromRational r (x % y)
  roundedFromRational ToNearest    = round
  roundedFromRational TowardNegInf = floor
  roundedFromRational TowardInf    = ceiling
  roundedFromRational TowardZero   = truncate
  roundedFromRealFloat r x | isNaN x = error "NaN"
                           | isInfinite x = error "Infinity"
                           | otherwise = roundedFromRational r (toRational x)

-- TODO: instance RoundedSqrt Integer

instance Integral a => RoundedRing (Ratio a) where
  roundedAdd _ = (Prelude.+)
  roundedSub _ = (Prelude.-)
  roundedMul _ = (Prelude.*)
  roundedFromInteger _ = Prelude.fromInteger
  backendNameT = Tagged "Rational"

instance Integral a => RoundedFractional (Ratio a) where
  roundedDiv _ = (Prelude./)
  roundedRecip _ = Prelude.recip
  roundedFromRational _ = Prelude.fromRational
  roundedFromRealFloat _ x | isNaN x = error "NaN"
                           | isInfinite x = error "Infinity"
                           | otherwise = Prelude.fromRational (toRational x)

-- There is no RoundedSqrt (Ratio a)
