{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Numeric.Rounded.Hardware.Internal.Class
  ( module Numeric.Rounded.Hardware.Internal.Class
  , module Numeric.Rounded.Hardware.Internal.Rounding
  ) where
import           Data.Coerce
import           Data.Proxy
import           Data.Ratio
import           Data.Tagged
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
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

class RoundedRing a => RoundedVectorOperation a where
  roundedSum_StorableVector :: Storable a => RoundingMode -> VS.Vector a -> a
  default roundedSum_StorableVector :: (Num a, Storable a) => RoundingMode -> VS.Vector a -> a
  roundedSum_StorableVector mode = VS.foldl' (roundedAdd mode) 0

  roundedSum_UnboxedVector :: VU.Unbox a => RoundingMode -> VU.Vector a -> a
  default roundedSum_UnboxedVector :: (Num a, VU.Unbox a) => RoundingMode -> VU.Vector a -> a
  roundedSum_UnboxedVector mode = VU.foldl' (roundedAdd mode) 0

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
  roundedAdd _ = (Prelude.+)
  roundedSub _ = (Prelude.-)
  roundedMul _ = (Prelude.*)
  roundedFromInteger _ = id
  backendNameT = Tagged "Integer"

instance RoundedFractional Integer where
  roundedDiv r x y = roundedFromRational r (x % y)
  roundedFromRational TowardNearest = round
  roundedFromRational TowardNegInf  = floor
  roundedFromRational TowardInf     = ceiling
  roundedFromRational TowardZero    = truncate

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

-- There is no RoundedSqrt (Ratio a)
