{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE DeriveGeneric #-}
module Numeric.Rounded.Hardware.Prim where
import Numeric.Rounded.Hardware.Internal
import Numeric.Rounded.Hardware.Interval
import GHC.Exts
import Data.Coerce
import GHC.Generics (Generic)
import Control.DeepSeq (NFData(..))

foreign import prim "rounded_hw_interval_add"
  c_rounded_interval_add :: Double# -- lower 1 %xmm1
                         -> Double# -- upper 1 %xmm2
                         -> Double# -- lower 2 %xmm3
                         -> Double# -- upper 2 %xmm4
                         -> (# Double#  -- lower %xmm1
                             , Double#  -- upper %xmm2
                            #)

foreign import prim "rounded_hw_interval_sub"
  c_rounded_interval_sub :: Double# -- lower 1 %xmm1
                         -> Double# -- upper 1 %xmm2
                         -> Double# -- lower 2 %xmm3
                         -> Double# -- upper 2 %xmm4
                         -> (# Double#  -- lower %xmm1
                             , Double#  -- upper %xmm2
                            #)

foreign import prim "rounded_hw_interval_recip"
  c_rounded_interval_recip :: Double# -- lower 1 %xmm1
                           -> Double# -- upper 1 %xmm2
                           -> (# Double#  -- lower %xmm1
                               , Double#  -- upper %xmm2
                              #)

-- TODO: sqrt?

rounded_interval_add :: Double -> Double -> Double -> Double -> (Double, Double)
rounded_interval_add (D# l1) (D# h1) (D# l2) (D# h2) = case c_rounded_interval_add l1 h1 l2 h2 of
  (# l3, h3 #) -> (D# l3, D# h3)

rounded_interval_sub :: Double -> Double -> Double -> Double -> (Double, Double)
rounded_interval_sub (D# l1) (D# h1) (D# l2) (D# h2) = case c_rounded_interval_sub l1 h1 l2 h2 of
  (# l3, h3 #) -> (D# l3, D# h3)

rounded_interval_recip :: Double -> Double -> (Double, Double)
rounded_interval_recip (D# l1) (D# h1) = case c_rounded_interval_recip l1 h1 of
  (# l2, h2 #) -> (D# l2, D# h2)

fastAdd :: IntervalDouble -> IntervalDouble -> IntervalDouble
fastAdd (I a b) (I a' b') = case coerce rounded_interval_add a b a' b' of
                              (c, d) -> I c d
fastAdd _ _ = Empty

fastSub :: IntervalDouble -> IntervalDouble -> IntervalDouble
fastSub (I a b) (I a' b') = case coerce rounded_interval_sub a b a' b' of
                              (c, d) -> I c d
fastSub _ _ = Empty

fastRecip :: IntervalDouble -> IntervalDouble
fastRecip (I a b) | 0 < a || b < 0 = case coerce rounded_interval_recip a b of
                                       (c, d) -> I c d
                  | otherwise = error "divide by zero"
fastRecip Empty = Empty

newtype FastIntervalDouble = FastIntervalDouble IntervalDouble deriving (Show,Generic)

instance NFData FastIntervalDouble

instance Num FastIntervalDouble where
  (+) = coerce fastAdd
  (-) = coerce fastSub
  negate = coerce (negate :: IntervalDouble -> IntervalDouble)
  (*) = coerce ((*) :: IntervalDouble -> IntervalDouble -> IntervalDouble)
  abs = coerce (abs :: IntervalDouble -> IntervalDouble)
  signum = coerce (signum :: IntervalDouble -> IntervalDouble)
  fromInteger = coerce (fromInteger :: Integer -> IntervalDouble)

instance Fractional FastIntervalDouble where
  recip = coerce fastRecip
  (/) = coerce ((/) :: IntervalDouble -> IntervalDouble -> IntervalDouble)
  fromRational = coerce (fromRational :: Rational -> IntervalDouble)
