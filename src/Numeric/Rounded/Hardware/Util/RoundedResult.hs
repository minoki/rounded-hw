{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Numeric.Rounded.Hardware.Util.RoundedResult where
import Data.Proxy
import Data.Functor.Product
import Numeric.Rounded.Hardware.Rounding

class Functor f => Result f where
  exact :: a -> f a
  inexact :: a -- toward nearest
          -> a -- toward inf
          -> a -- toward neg inf
          -> a -- toward zero
          -> f a

newtype Exactness a = Exactness { getExactness :: Bool }
  deriving (Eq, Ord, Show, Functor)

instance Rounding rn => Result (Rounded rn) where
  exact x = Rounded x
  inexact n inf ninf z = case rounding (Proxy :: Proxy rn) of
                           TowardNearest -> Rounded n
                           TowardInf -> Rounded inf
                           TowardNegInf -> Rounded ninf
                           TowardZero -> Rounded z

newtype DynamicRoundingMode a = DynamicRoundingMode { withRoundingMode :: RoundingMode -> a }
  deriving (Functor)
instance Result DynamicRoundingMode where
  exact x = DynamicRoundingMode (\_ -> x)
  inexact n inf ninf z = DynamicRoundingMode $ \rn ->
    case rn of
      TowardNearest -> n
      TowardInf -> inf
      TowardNegInf -> ninf
      TowardZero -> z

instance Result Exactness where
  exact _ = Exactness True
  inexact _ _ _ _ = Exactness False

-- Usage: Product (Rounded TowardNegInf) (Rounded TowardInf)
instance (Result f, Result g) => Result (Product f g) where
  exact x = Pair (exact x) (exact x)
  inexact n inf ninf z = Pair (inexact n inf ninf z) (inexact n inf ninf z)

newtype OppositeRoundingMode f a = OppositeRoundingMode { withOppositeRoundingMode :: f a }
  deriving (Eq, Ord, Show, Functor)

instance Result f => Result (OppositeRoundingMode f) where
  exact x = OppositeRoundingMode (exact x)
  inexact n inf ninf z = OppositeRoundingMode (inexact n ninf inf z)
