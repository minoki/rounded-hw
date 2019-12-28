{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes    #-}
module Numeric.Rounded.Hardware.Interval where
import           Control.DeepSeq                          (NFData (..))
import           Data.Coerce
import           GHC.Generics                             (Generic)
import           Numeric.Rounded.Hardware.Backend.Default ()
import           Numeric.Rounded.Hardware.Class

data Interval a
  = I !(Rounded 'TowardNegInf a) !(Rounded 'TowardInf a)
  | Empty
  deriving (Show,Generic)

instance NFData a => NFData (Interval a)

increasing :: (forall r. Rounding r => Rounded r a -> Rounded r a) -> Interval a -> Interval a
increasing f (I a b) = I (f a) (f b)
increasing _ Empty   = Empty

instance (Num a, RoundedRing a) => Num (Interval a) where
  I a b + I a' b' = case intervalAdd a b a' b' of
                      (a'', b'') -> I a'' b''
  _ + _ = Empty
  I a b - I a' b' = case intervalSub a b a' b' of
                      (a'', b'') -> I a'' b''
  _ - _ = Empty
  negate (I a b) = I (negate (coerce b)) (negate (coerce a))
  negate Empty   = Empty
  I a b * I a' b' = case intervalMul a b a' b' of
                      (a'', b'') -> I a'' b''
  _ * _ = Empty
  abs x@(I a b)
    | a >= 0 = x
    | b <= 0 = negate x
    | otherwise = I 0 (max (negate (coerce a)) b)
  abs Empty = Empty
  signum = increasing signum
  fromInteger x = case intervalFromInteger x of
                    (y, y') -> I y y'
  {-# SPECIALIZE instance Num (Interval Float) #-}
  {-# SPECIALIZE instance Num (Interval Double) #-}

instance (Num a, RoundedFractional a) => Fractional (Interval a) where
  recip Empty = Empty
  recip (I a b)
    -- TODO: Allow a' == 0 || b' == 0?
    | 0 < a || b < 0 = case intervalRecip a b of
                         (x, x') -> I x x'
    | otherwise = error "divide by zero"
  I a b / I a' b'
    -- TODO: Allow a' == 0 || b' == 0?
    | 0 < a' || b' < 0 = case intervalDiv a b a' b' of
                           (a'', b'') -> I a'' b''
                           -- TODO: Allow a' == 0 || b' == 0?
    | otherwise = error "divide by zero"
  _ / Empty = Empty
  Empty / _ = Empty
  fromRational x = case intervalFromRational x of
                     (y, y') -> I y y'
  {-# SPECIALIZE instance Fractional (Interval Float) #-}
  {-# SPECIALIZE instance Fractional (Interval Double) #-}

maxI :: Ord a => Interval a -> Interval a -> Interval a
maxI (I a a') (I b b') = I (max a b) (max a' b')
maxI _ _               = Empty
{-# SPECIALIZE maxI :: Interval Float -> Interval Float -> Interval Float #-}
{-# SPECIALIZE maxI :: Interval Double -> Interval Double -> Interval Double #-}

minI :: Ord a => Interval a -> Interval a -> Interval a
minI (I a a') (I b b') = I (min a b) (min a' b')
minI _ _               = Empty
{-# SPECIALIZE minI :: Interval Float -> Interval Float -> Interval Float #-}
{-# SPECIALIZE minI :: Interval Double -> Interval Double -> Interval Double #-}

powInt :: (Ord a, Num a, RoundedRing a) => Interval a -> Int -> Interval a
powInt (I a a') n | odd n || 0 <= a = I (a^n) (a'^n)
                  | a' <= 0 = I ((coerce (abs a'))^n) ((coerce (abs a))^n)
                  | otherwise = I 0 (max ((coerce (abs a))^n) (a'^n))
powInt Empty _ = Empty
{-# SPECIALIZE powInt :: Interval Float -> Int -> Interval Float #-}
{-# SPECIALIZE powInt :: Interval Double -> Int -> Interval Double #-}
