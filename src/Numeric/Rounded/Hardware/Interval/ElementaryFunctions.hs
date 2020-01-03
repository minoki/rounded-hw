{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Numeric.Rounded.Hardware.Interval.ElementaryFunctions where
import Numeric.Rounded.Hardware.Internal
import Numeric.Rounded.Hardware.Interval.Class

expP :: forall i. (IsInterval i, Fractional i, Eq (EndPoint i), RealFloat (EndPoint i), RealFloatConstants (EndPoint i)) => EndPoint i -> i
expP x | isInfinite x = if x > 0
                        then makeInterval (Rounded maxFinite) (Rounded positiveInfinity)
                        else makeInterval (Rounded 0) (Rounded minPositive)
expP x = let a = round x
             b = x - fromInteger a -- -1/2 <= b && b <= 1/2
             b' = singleton b
             series :: Int -> i -> i
             series n acc | n == 0 = acc
                          | otherwise = series (n-1) $ 1 + acc * b' / fromIntegral n
         in if fromInteger a + b == x && abs b <= 0.5
            then (makeInterval exp1_down exp1_up)^^a * series 15 (makeInterval expm1_2_down exp1_2_up)
            else error "rounding error"

expI :: (IsInterval i, Fractional i, Eq (EndPoint i), RealFloat (EndPoint i), RealFloatConstants (EndPoint i)) => i -> i
expI = withEndPoints (\(Rounded x) (Rounded y) -> hull (expP x) (expP y))

expm1P :: forall i. (IsInterval i, Fractional i, Eq (EndPoint i), RealFloat (EndPoint i), RealFloatConstants (EndPoint i)) => EndPoint i -> i
expm1P x | -0.5 <= x && x <= 0.5 = let b' = singleton x
                                       series :: Int -> i -> i
                                       series n acc | n == 1 = acc * b'
                                                    | otherwise = series (n-1) $ 1 + acc * b' / fromIntegral n
                                   in series 15 (makeInterval expm1_2_down exp1_2_up)
         | otherwise = expP x - 1

expm1I :: (IsInterval i, Fractional i, Eq (EndPoint i), RealFloat (EndPoint i), RealFloatConstants (EndPoint i)) => i -> i
expm1I = withEndPoints (\(Rounded x) (Rounded y) -> hull (expm1P x) (expm1P y))
