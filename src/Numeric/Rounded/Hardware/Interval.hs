{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
module Numeric.Rounded.Hardware.Interval where
import Numeric.Rounded.Hardware.Rounding
import Numeric.Rounded.Hardware.Internal
import Data.Coerce
import GHC.Generics (Generic)
import Control.DeepSeq (NFData(..))

data IntervalDouble
  = I !(RoundedDouble TowardNegInf) !(RoundedDouble TowardInf)
  | Empty
  deriving (Show,Generic)

instance NFData IntervalDouble

instance Num IntervalDouble where
  I a b + I a' b' = I (a + a') (b + b')
  _ + _ = Empty
  I a b - I a' b' = I (a - coerce b') (b - coerce a')
  _ - _ = Empty
  negate (I a b) = I (coerce (negate b)) (coerce (negate a))
  negate Empty = Empty
  I a b * I a' b' = I (minimum [a * a', a * coerce b', coerce b * a', coerce b * coerce b'])
                      (maximum [coerce a * coerce a', coerce a * b', b * coerce a', b * b'])
  _ * _ = Empty
  abs x@(I a b)
    | a >= 0 = x
    | b <= 0 = negate x
    | otherwise = I 0 (max (negate (coerce a)) b)
  abs Empty = Empty
  signum = increasing signum
  fromInteger = I <$> fromInteger <*> fromInteger

increasing :: (forall r. RoundedPrim r => RoundedDouble r -> RoundedDouble r) -> IntervalDouble -> IntervalDouble
increasing f (I a b) = I (f a) (f b)
increasing _ Empty = Empty

whole :: IntervalDouble
whole = I ((-1)/0) (1/0)

instance Fractional IntervalDouble where
  recip Empty = Empty
  recip (I a b) | 0 < a || b < 0 = I (recip (coerce b)) (recip (coerce a))
                | otherwise = error "divide by zero"
  x@(I a b) / y@(I a' b') | 0 < a' || b' < 0 = I (minimum [a / a', a / coerce b', coerce b / a', coerce b / coerce b'])
                                                 (maximum [coerce a / coerce a', coerce a / b', b / coerce a', b / b'])
                            -- TODO: Allow a' == 0 || b' == 0?
                          | otherwise = error "divide by zero"
  _ / Empty = Empty
  Empty / _ = Empty
  fromRational = I <$> fromRational <*> fromRational
