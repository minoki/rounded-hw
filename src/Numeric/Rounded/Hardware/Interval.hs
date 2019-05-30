{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
module Numeric.Rounded.Hardware.Interval where
import Data.Coerce
import Numeric.Rounded.Hardware.Internal

data IntervalDouble
  = I !(RoundedDouble TowardNegInf) !(RoundedDouble TowardInf)
  | Empty

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

{-
instance Fractional IntervalDouble where
  _ / Empty = Empty
  x / y@(I a b)
-}
