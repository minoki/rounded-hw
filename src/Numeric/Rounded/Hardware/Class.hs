{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Numeric.Rounded.Hardware.Class where
import           Control.DeepSeq                   (NFData (..))
import           Data.Coerce
import           Data.Proxy
import           Data.Ratio
import           GHC.Generics                      (Generic)
import           Numeric.Rounded.Hardware.Rounding

class Ord a => RoundedRing a where
  roundedAdd :: RoundingMode -> a -> a -> a
  roundedSub :: RoundingMode -> a -> a -> a
  roundedMul :: RoundingMode -> a -> a -> a
  roundedFromInteger :: RoundingMode -> Integer -> a
  -- roundedToFloat :: RoundingMode -> a -> Float
  -- roundedToDouble :: RoundingMode -> a -> Double

  intervalAdd :: a -> a -> a -> a -> (a, a)
  intervalAdd x_lo x_hi y_lo y_hi = (roundedAdd TowardNegInf x_lo y_lo, roundedAdd TowardInf x_hi y_hi)
  intervalSub :: a -> a -> a -> a -> (a, a)
  intervalSub x_lo x_hi y_lo y_hi = (roundedSub TowardNegInf x_lo y_hi, roundedSub TowardInf x_hi y_lo)
  intervalMul :: a -> a -> a -> a -> (a, a)
  intervalMul x_lo x_hi y_lo y_hi
    = ( minimum [ roundedMul TowardNegInf x_lo y_lo
                , roundedMul TowardNegInf x_lo y_hi
                , roundedMul TowardNegInf x_hi y_lo
                , roundedMul TowardNegInf x_hi y_hi
                ]
      , maximum [ roundedMul TowardInf x_lo y_lo
                , roundedMul TowardInf x_lo y_hi
                , roundedMul TowardInf x_hi y_lo
                , roundedMul TowardInf x_hi y_hi
                ]
      )
  intervalFromInteger :: Integer -> (a, a)
  intervalFromInteger x = (roundedFromInteger TowardNegInf x, roundedFromInteger TowardInf x)

class RoundedRing a => RoundedField a where
  roundedDiv :: RoundingMode -> a -> a -> a
  roundedRecip :: RoundingMode -> a -> a
  default roundedRecip :: Num a => RoundingMode -> a -> a
  roundedRecip r = roundedDiv r 1
  roundedFromRational :: RoundingMode -> Rational -> a
  intervalDiv :: a -> a -> a -> a -> (a, a)
  intervalDiv x_lo x_hi y_lo y_hi
    = ( minimum [ roundedDiv TowardNegInf x_lo y_lo
                , roundedDiv TowardNegInf x_lo y_hi
                , roundedDiv TowardNegInf x_hi y_lo
                , roundedDiv TowardNegInf x_hi y_hi
                ]
      , maximum [ roundedDiv TowardInf x_lo y_lo
                , roundedDiv TowardInf x_lo y_hi
                , roundedDiv TowardInf x_hi y_lo
                , roundedDiv TowardInf x_hi y_hi
                ]
      )
  intervalRecip :: a -> a -> (a, a)
  default intervalRecip :: (Num a) => a -> a -> (a, a)
  intervalRecip x_lo x_hi
    | 0 < x_lo || x_hi < 0 = (roundedRecip TowardNegInf x_hi, roundedRecip TowardNegInf x_lo)
    | otherwise = error "divide by zero"
  intervalFromRational :: Rational -> (a, a)
  intervalFromRational x = (roundedFromRational TowardNegInf x, roundedFromRational TowardInf x)

class RoundedRing a => RoundedSqrt a where
  roundedSqrt :: RoundingMode -> a -> a

newtype Rounded (rn :: RoundingMode) a = Rounded a
  deriving (Eq,Ord,Show,Generic)

instance NFData a => NFData (Rounded rn a)

instance (Rounding rn, Num a, RoundedRing a) => Num (Rounded rn a) where
  Rounded x + Rounded y = Rounded (roundedAdd (rounding (Proxy :: Proxy rn)) x y)
  Rounded x - Rounded y = Rounded (roundedSub (rounding (Proxy :: Proxy rn)) x y)
  Rounded x * Rounded y = Rounded (roundedMul (rounding (Proxy :: Proxy rn)) x y)
  negate = coerce (negate :: a -> a)
  abs = coerce (abs :: a -> a)
  signum = coerce (signum :: a -> a)
  fromInteger x = Rounded (roundedFromInteger (rounding (Proxy :: Proxy rn)) x)

instance (Rounding rn, Num a, RoundedField a) => Fractional (Rounded rn a) where
  Rounded x / Rounded y = Rounded (roundedDiv (rounding (Proxy :: Proxy rn)) x y)
  recip (Rounded x) = Rounded (roundedRecip (rounding (Proxy :: Proxy rn)) x)
  fromRational x = Rounded (roundedFromRational (rounding (Proxy :: Proxy rn)) x)

-- These instances are provided in Numeric.Rounded.Hardware.Backend.Default:
--   instance RoundedRing Float
--   instance RoundedField Float
--   instance RoundedSqrt Float
--   instance RoundedRing Double
--   instance RoundedField Double
--   instance RoundedSqrt Double

instance RoundedRing Integer where
  roundedAdd _ = (+)
  roundedSub _ = (-)
  roundedMul _ = (*)
  roundedFromInteger _ = id

instance RoundedField Integer where
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

instance Integral a => RoundedField (Ratio a) where
  roundedDiv _ = (/)
  roundedRecip _ = recip
  roundedFromRational _ = fromRational

-- There is no RoundedSqrt (Ratio a)
