{-# LANGUAGE HexFloatLiterals #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Numeric.Rounded.Hardware.Internal.Conversion
  ( fromInt
  , fromIntF
  , intervalFromInteger_default
  , fromRatio
  , fromRatioF
  , intervalFromRational_default
  ) where
import Numeric.Rounded.Hardware.Internal.Rounding
import Numeric.Rounded.Hardware.Internal.RoundedResult
import Numeric.Rounded.Hardware.Internal.FloatUtil
import Data.Bits
import Data.Functor.Product
import Math.NumberTheory.Logarithms (integerLog2')
import Data.Ratio
import Control.Exception (assert)
-- import GHC.Integer.Logarithms.Internals (integerLog2IsPowerOf2#)
-- integerLog2IsPowerOf2# :: Integer -> (# Int#, Int# #)

intervalFromInteger_default :: RealFloat a => Integer -> (Rounded 'TowardNegInf a, Rounded 'TowardInf a)
intervalFromInteger_default x = case fromIntF x of Pair a b -> (a, b)
{-# SPECIALIZE intervalFromInteger_default :: Integer -> (Rounded 'TowardNegInf Float, Rounded 'TowardInf Float) #-}
{-# SPECIALIZE intervalFromInteger_default :: Integer -> (Rounded 'TowardNegInf Double, Rounded 'TowardInf Double) #-}

intervalFromRational_default :: RealFloat a => Rational -> (Rounded 'TowardNegInf a, Rounded 'TowardInf a)
intervalFromRational_default x = case fromRatioF (numerator x) (denominator x) of Pair a b -> (a, b)
{-# SPECIALIZE intervalFromRational_default :: Rational -> (Rounded 'TowardNegInf Float, Rounded 'TowardInf Float) #-}
{-# SPECIALIZE intervalFromRational_default :: Rational -> (Rounded 'TowardNegInf Double, Rounded 'TowardInf Double) #-}

fromInt :: RealFloat a => RoundingMode -> Integer -> a
fromInt r n = withRoundingMode (fromIntF n) r
{-# SPECIALIZE fromInt :: RoundingMode -> Integer -> Float #-}
{-# SPECIALIZE fromInt :: RoundingMode -> Integer -> Double #-}

fromIntF :: forall a f. (RealFloat a, Result f) => Integer -> f a
fromIntF !_ | floatRadix (undefined :: a) /= 2 = error "radix other than 2 is not supported"
fromIntF 0 = exact 0
fromIntF n | n < 0 = negate <$> withOppositeRoundingMode (fromPositiveIntF (- n))
           | otherwise = fromPositiveIntF n
{-# INLINE fromIntF #-}

-- n > 0
fromPositiveIntF :: forall a f. (RealFloat a, Result f) => Integer -> f a
fromPositiveIntF !n
  = let !k = integerLog2' n -- floor (log2 n)
        -- 2^k <= n < 2^(k+1)
        !fDigits = floatDigits (undefined :: a) -- 53 for Double
    in if k < fDigits
       then exact (fromInteger n)
       else let e = k - (fDigits - 1)
                  -- (!q, !r) = n `quotRem` (1 `unsafeShiftL` e)
                q = n `unsafeShiftR` e
                r = n .&. ((1 `unsafeShiftL` e) - 1)
                    -- 2^52 <= q < 2^53, 0 <= r < 2^(k-52)
                (_expMin, !expMax) = floatRange (undefined :: a) -- (-1021, 1024) for Double
            in if k >= expMax
               then
                 -- infinity
                 inexact (1 / 0) -- ToNearest
                         (1 / 0) -- TowardInf
                         maxFinite_ieee -- TowardNegInf
                         maxFinite_ieee -- TowardZero
               else
                 if r == 0
                 then exact $ encodeFloat q e -- exact
                 else
                   -- inexact
                   let down = encodeFloat q e
                       up = encodeFloat (q + 1) e
                       toNearest = case compare r (1 `unsafeShiftL` (e-1)) of
                         LT -> down
                         EQ | even q -> down
                            | otherwise -> up
                         GT -> up
                   in inexact toNearest up down down
{-# SPECIALIZE fromPositiveIntF :: Integer -> DynamicRoundingMode Float #-}
{-# SPECIALIZE fromPositiveIntF :: Integer -> OppositeRoundingMode DynamicRoundingMode Float #-}
{-# SPECIALIZE fromPositiveIntF :: Rounding r => Integer -> Rounded r Float #-}
{-# SPECIALIZE fromPositiveIntF :: Rounding r => Integer -> OppositeRoundingMode (Rounded r) Float #-}
{-# SPECIALIZE fromPositiveIntF :: Integer -> Product (Rounded 'TowardNegInf) (Rounded 'TowardInf) Float #-}
{-# SPECIALIZE fromPositiveIntF :: Integer -> OppositeRoundingMode (Product (Rounded 'TowardNegInf) (Rounded 'TowardInf)) Float #-}
{-# SPECIALIZE fromPositiveIntF :: Integer -> DynamicRoundingMode Double #-}
{-# SPECIALIZE fromPositiveIntF :: Integer -> OppositeRoundingMode DynamicRoundingMode Double #-}
{-# SPECIALIZE fromPositiveIntF :: Rounding r => Integer -> Rounded r Double #-}
{-# SPECIALIZE fromPositiveIntF :: Rounding r => Integer -> OppositeRoundingMode (Rounded r) Double #-}
{-# SPECIALIZE fromPositiveIntF :: Integer -> Product (Rounded 'TowardNegInf) (Rounded 'TowardInf) Double #-}
{-# SPECIALIZE fromPositiveIntF :: Integer -> OppositeRoundingMode (Product (Rounded 'TowardNegInf) (Rounded 'TowardInf)) Double #-}

fromRatio :: (RealFloat a)
          => RoundingMode
          -> Integer -- ^ numerator
          -> Integer -- ^ denominator
          -> a
fromRatio r n d = withRoundingMode (fromRatioF n d) r
{-# SPECIALIZE fromRatio :: RoundingMode -> Integer -> Integer -> Float #-}
{-# SPECIALIZE fromRatio :: RoundingMode -> Integer -> Integer -> Double #-}

fromRatioF :: forall a f. (RealFloat a, Result f)
           => Integer -- ^ numerator
           -> Integer -- ^ denominator
           -> f a
fromRatioF !_ !_ | floatRadix (undefined :: a) /= 2 = error "radix other than 2 is not supported"
fromRatioF 0 _ = exact 0
fromRatioF n 0 | n > 0 = exact (1 / 0) -- positive infinity
               | otherwise = exact (- 1 / 0) -- negative infinity
fromRatioF n d | d < 0 = error "fromRatio: negative denominator"
               | n < 0 = negate <$> withOppositeRoundingMode (fromPositiveRatioF (- n) d)
               | otherwise = fromPositiveRatioF n d
{-# INLINE fromRatioF #-}

-- n > 0, d > 0
fromPositiveRatioF :: forall a f. (RealFloat a, Result f)
                   => Integer -> Integer -> f a
fromPositiveRatioF !n !d
  = let ln, ld, e :: Int
        ln = integerLog2' n
        ld = integerLog2' d
        e = ln - ld - fDigits
        q, r, d_ :: Integer
        d_ | e >= 0 = d `unsafeShiftL` e
           | otherwise = d
        (!q, !r) | e >= 0 = n `quotRem` d_
                 | otherwise = (n `unsafeShiftL` (-e)) `quotRem` d
        -- e >= 0: n = q * (d * 2^e) + r, 0 <= r < d * 2^e
        -- e <= 0: n * 2^(-e) = q * d + r, 0 <= r < d
        -- n / d * 2^^(-e) = q + r / d_
        -- 52 <= log2 q < 54
        q', r', d' :: Integer
        e' :: Int
        (!q', !r', !d', !e') | q < (1 `unsafeShiftL` fDigits) = (q, r, d_, e)
                             | otherwise = let (q'', r'') = q `quotRem` 2
                                           in (q'', r'' * d_ + r, 2 * d_, e + 1)
        -- n / d * 2^^(-e') = q' + r' / d', 2^52 <= q' < 2^53, 0 <= r' < d'
        -- q' * 2^^e' <= n/d < (q'+1) * 2^^e', 2^52 <= q' < 2^53
        -- (q'/2^53) * 2^^(e'+53) <= n/d < (q'+1)/2^53 * 2^^(e'+53), 1/2 <= q'/2^53 < 1
        -- normal: 0x1p-1022 <= x <= 0x1.fffffffffffffp+1023
    in assert (n % d * 2^^(-e) == fromInteger q + r % d_) $
       assert (n % d * 2^^(-e') == fromInteger q' + r' % d') $
       if expMin <= e' + fDigits && e' + fDigits <= expMax
       then
         -- normal
         if r' == 0
         then
           exact $ encodeFloat q' e' -- exact
         else
           -- inexact
           let down = encodeFloat q' e'
               up = encodeFloat (q' + 1) e' -- may be infinity
               toNearest = case compare (2 * r') d' of
                 LT -> down
                 EQ | even q' -> down
                    | otherwise -> up -- q' + 1 is even
                 GT -> up
           in inexact toNearest up down down
       else
         -- infinity or subnormal
         if expMax <= e' + fDigits
         then
           -- infinity
           inexact (1 / 0) -- ToNearest
                   (1 / 0) -- TowardInf
                   maxFinite_ieee -- TowardNegInf
                   maxFinite_ieee -- TowardZero
         else
           -- subnormal
           -- e' + fDigits < expMin (or, e' < expMin - fDigits = -1074)
           -- 0 <= rounded(n/d) <= 2^(expMin - 1) = 0x1p-1022, minimum (positive) subnormal: 0x1p-1074
           let (!q'', !r'') = q' `quotRem` (1 `unsafeShiftL` (expMin - fDigits - e'))
               -- q' = q'' * 2^(expMin - fDigits - e') + r'', 0 <= r'' < 2^(expMin - fDigits - e')
               -- 2^(fDigits-1) <= q' = q'' * 2^(expMin - fDigits - e') + r'' < 2^fDigits
               -- n / d * 2^^(-e') = q' + r' / d' = q'' * 2^(expMin - fDigits - e') + r'' + r' / d'
               -- n / d = q'' * 2^^(expMin - fDigits) + (r'' + r' / d') * 2^^e'
               -- 0 <= r'' < 2^(expMin - fDigits - e')
           in if r' == 0 && r'' == 0
              then exact $ encodeFloat q'' (expMin - fDigits) -- exact
              else let down = encodeFloat q'' (expMin - fDigits)
                       up = encodeFloat (q'' + 1) (expMin - fDigits)
                       toNearest = case compare r'' (1 `unsafeShiftL` (expMin - fDigits - e' - 1)) of
                         LT -> down
                         GT -> up
                         EQ | r' /= 0   -> up
                            | even q'   -> down
                            | otherwise -> up
                   in inexact toNearest up down down
  where
    !fDigits = floatDigits (undefined :: a) -- 53 for Double
    (!expMin, !expMax) = floatRange (undefined :: a) -- (-1021, 1024) for Double
{-# SPECIALIZE fromPositiveRatioF :: Integer -> Integer -> DynamicRoundingMode Float #-}
{-# SPECIALIZE fromPositiveRatioF :: Integer -> Integer -> OppositeRoundingMode DynamicRoundingMode Float #-}
{-# SPECIALIZE fromPositiveRatioF :: Rounding r => Integer -> Integer -> Rounded r Float #-}
{-# SPECIALIZE fromPositiveRatioF :: Rounding r => Integer -> Integer -> OppositeRoundingMode (Rounded r) Float #-}
{-# SPECIALIZE fromPositiveRatioF :: Integer -> Integer -> Product (Rounded 'TowardNegInf) (Rounded 'TowardInf) Float #-}
{-# SPECIALIZE fromPositiveRatioF :: Integer -> Integer -> OppositeRoundingMode (Product (Rounded 'TowardNegInf) (Rounded 'TowardInf)) Float #-}
{-# SPECIALIZE fromPositiveRatioF :: Integer -> Integer -> DynamicRoundingMode Double #-}
{-# SPECIALIZE fromPositiveRatioF :: Integer -> Integer -> OppositeRoundingMode DynamicRoundingMode Double #-}
{-# SPECIALIZE fromPositiveRatioF :: Rounding r => Integer -> Integer -> Rounded r Double #-}
{-# SPECIALIZE fromPositiveRatioF :: Rounding r => Integer -> Integer -> OppositeRoundingMode (Rounded r) Double #-}
{-# SPECIALIZE fromPositiveRatioF :: Integer -> Integer -> Product (Rounded 'TowardNegInf) (Rounded 'TowardInf) Double #-}
{-# SPECIALIZE fromPositiveRatioF :: Integer -> Integer -> OppositeRoundingMode (Product (Rounded 'TowardNegInf) (Rounded 'TowardInf)) Double #-}
