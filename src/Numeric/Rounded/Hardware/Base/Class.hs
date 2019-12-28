{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Numeric.Rounded.Hardware.Base.Class
  ( module Numeric.Rounded.Hardware.Base.Class
  , module Numeric.Rounded.Hardware.Base.Rounding
  ) where
import           Data.Coerce
import           Data.Proxy
import           Data.Ratio
import           Numeric.Rounded.Hardware.Base.Rounding

class Ord a => RoundedRing a where
  roundedAdd :: RoundingMode -> a -> a -> a
  roundedSub :: RoundingMode -> a -> a -> a
  roundedMul :: RoundingMode -> a -> a -> a
  roundedFromInteger :: RoundingMode -> Integer -> a
  -- roundedToFloat :: RoundingMode -> a -> Float
  -- roundedToDouble :: RoundingMode -> a -> Double

  intervalAdd :: Rounded 'TowardNegInf a -> Rounded 'TowardInf a -> Rounded 'TowardNegInf a -> Rounded 'TowardInf a -> (Rounded 'TowardNegInf a, Rounded 'TowardInf a)
  default intervalAdd :: Num a => Rounded 'TowardNegInf a -> Rounded 'TowardInf a -> Rounded 'TowardNegInf a -> Rounded 'TowardInf a -> (Rounded 'TowardNegInf a, Rounded 'TowardInf a)
  intervalAdd x_lo x_hi y_lo y_hi = (x_lo + y_lo, x_hi + y_hi)
  intervalSub :: Rounded 'TowardNegInf a -> Rounded 'TowardInf a -> Rounded 'TowardNegInf a -> Rounded 'TowardInf a -> (Rounded 'TowardNegInf a, Rounded 'TowardInf a)
  default intervalSub :: Num a => Rounded 'TowardNegInf a -> Rounded 'TowardInf a -> Rounded 'TowardNegInf a -> Rounded 'TowardInf a -> (Rounded 'TowardNegInf a, Rounded 'TowardInf a)
  intervalSub x_lo x_hi y_lo y_hi = (x_lo - coerce y_hi, x_hi - coerce y_lo)
  intervalMul :: Rounded 'TowardNegInf a -> Rounded 'TowardInf a -> Rounded 'TowardNegInf a -> Rounded 'TowardInf a -> (Rounded 'TowardNegInf a, Rounded 'TowardInf a)
  default intervalMul :: Num a => Rounded 'TowardNegInf a -> Rounded 'TowardInf a -> Rounded 'TowardNegInf a -> Rounded 'TowardInf a -> (Rounded 'TowardNegInf a, Rounded 'TowardInf a)
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
  intervalFromInteger :: Integer -> (Rounded 'TowardNegInf a, Rounded 'TowardInf a)
  default intervalFromInteger :: Num a => Integer -> (Rounded 'TowardNegInf a, Rounded 'TowardInf a)
  intervalFromInteger x = (fromInteger x, fromInteger x)
  {-# INLINE intervalAdd #-}
  {-# INLINE intervalSub #-}
  {-# INLINE intervalMul #-}
  {-# INLINE intervalFromInteger #-}

class RoundedRing a => RoundedFractional a where
  roundedDiv :: RoundingMode -> a -> a -> a
  roundedRecip :: RoundingMode -> a -> a
  default roundedRecip :: Num a => RoundingMode -> a -> a
  roundedRecip r = roundedDiv r 1
  roundedFromRational :: RoundingMode -> Rational -> a
  intervalDiv :: Rounded 'TowardNegInf a -> Rounded 'TowardInf a -> Rounded 'TowardNegInf a -> Rounded 'TowardInf a -> (Rounded 'TowardNegInf a, Rounded 'TowardInf a)
  default intervalDiv :: Num a => Rounded 'TowardNegInf a -> Rounded 'TowardInf a -> Rounded 'TowardNegInf a -> Rounded 'TowardInf a -> (Rounded 'TowardNegInf a, Rounded 'TowardInf a)
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
  intervalRecip :: Rounded 'TowardNegInf a -> Rounded 'TowardInf a -> (Rounded 'TowardNegInf a, Rounded 'TowardInf a)
  default intervalRecip :: Num a => Rounded 'TowardNegInf a -> Rounded 'TowardInf a -> (Rounded 'TowardNegInf a, Rounded 'TowardInf a)
  intervalRecip x_lo x_hi
    | 0 < x_lo || x_hi < 0 = (recip (coerce x_hi), recip (coerce x_lo))
    | otherwise = error "divide by zero"
  intervalFromRational :: Rational -> (Rounded 'TowardNegInf a, Rounded 'TowardInf a)
  default intervalFromRational :: Num a => Rational -> (Rounded 'TowardNegInf a, Rounded 'TowardInf a)
  intervalFromRational x = (fromRational x, fromRational x)
  {-# INLINE intervalDiv #-}
  {-# INLINE intervalRecip #-}
  {-# INLINE intervalFromRational #-}

class RoundedRing a => RoundedSqrt a where
  roundedSqrt :: RoundingMode -> a -> a

instance (Rounding rn, Num a, RoundedRing a) => Num (Rounded rn a) where
  Rounded x + Rounded y = Rounded (roundedAdd (rounding (Proxy :: Proxy rn)) x y)
  Rounded x - Rounded y = Rounded (roundedSub (rounding (Proxy :: Proxy rn)) x y)
  Rounded x * Rounded y = Rounded (roundedMul (rounding (Proxy :: Proxy rn)) x y)
  negate = coerce (negate :: a -> a)
  abs = coerce (abs :: a -> a)
  signum = coerce (signum :: a -> a)
  fromInteger x = Rounded (roundedFromInteger (rounding (Proxy :: Proxy rn)) x)
  {-# INLINE (+) #-}
  {-# INLINE (-) #-}
  {-# INLINE (*) #-}
  {-# INLINE negate #-}
  {-# INLINE abs #-}
  {-# INLINE signum #-}
  {-# INLINE fromInteger #-}

instance (Rounding rn, Num a, RoundedFractional a) => Fractional (Rounded rn a) where
  Rounded x / Rounded y = Rounded (roundedDiv (rounding (Proxy :: Proxy rn)) x y)
  recip (Rounded x) = Rounded (roundedRecip (rounding (Proxy :: Proxy rn)) x)
  fromRational x = Rounded (roundedFromRational (rounding (Proxy :: Proxy rn)) x)
  {-# INLINE (/) #-}
  {-# INLINE recip #-}
  {-# INLINE fromRational #-}

deriving newtype instance (Rounding rn, Real a, RoundedFractional a) => Real (Rounded rn a)
deriving newtype instance (Rounding rn, RealFrac a, RoundedFractional a) => RealFrac (Rounded rn a)

-- These instances are provided in Numeric.Rounded.Hardware.Backend.Default:
--   instance RoundedRing Float
--   instance RoundedFractional Float
--   instance RoundedSqrt Float
--   instance RoundedRing Double
--   instance RoundedFractional Double
--   instance RoundedSqrt Double

instance RoundedRing Integer where
  roundedAdd _ = (+)
  roundedSub _ = (-)
  roundedMul _ = (*)
  roundedFromInteger _ = id

instance RoundedFractional Integer where
  roundedDiv r x y = roundedFromRational r (x % y)
  roundedFromRational TowardNearest = round
  roundedFromRational TowardNegInf  = floor
  roundedFromRational TowardInf     = ceiling
  roundedFromRational TowardZero    = truncate

-- TODO: instance RoundedSqrt Integer

instance Integral a => RoundedRing (Ratio a) where
  roundedAdd _ = (+)
  roundedSub _ = (-)
  roundedMul _ = (*)
  roundedFromInteger _ = fromInteger

instance Integral a => RoundedFractional (Ratio a) where
  roundedDiv _ = (/)
  roundedRecip _ = recip
  roundedFromRational _ = fromRational

-- There is no RoundedSqrt (Ratio a)
