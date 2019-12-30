{-# LANGUAGE HexFloatLiterals #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Numeric.Rounded.Hardware.Internal.Conversion
  ( fromInt
  , fromIntF
  , fromRatio
  , fromRatioF
  ) where
import Numeric.Rounded.Hardware.Internal.Rounding
import Numeric.Rounded.Hardware.Internal.RoundedResult
import Numeric.Rounded.Hardware.Internal.Constants
import Data.Bits
import Data.Functor.Product
import Math.NumberTheory.Logarithms (integerLog2')
-- import GHC.Integer.Logarithms.Internals (integerLog2IsPowerOf2#)
-- integerLog2IsPowerOf2# :: Integer -> (# Int#, Int# #)

fromInt :: (RealFloat a, RealFloatConstants a)
        => RoundingMode -> Integer -> a
fromInt rn n = withRoundingMode (fromIntF n) rn
{-# SPECIALIZE fromInt :: RoundingMode -> Integer -> Float #-}
{-# SPECIALIZE fromInt :: RoundingMode -> Integer -> Double #-}

fromIntF :: forall a f. (RealFloat a, RealFloatConstants a, Result f)
         => Integer -> f a
fromIntF !_ | floatRadix (undefined :: a) /= 2 = error "radix other than 2 is not supported"
fromIntF 0 = exact 0
fromIntF n | n < 0 = negate <$> withOppositeRoundingMode (fromPositiveIntF (- n))
           | otherwise = fromPositiveIntF n
{-# INLINE fromIntF #-}

-- n > 0
fromPositiveIntF :: forall a f. (RealFloat a, RealFloatConstants a, Result f)
                 => Integer -> f a
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
                (_expMin, !expMax) = floatRange (undefined :: Double) -- (-1021, 1024) for Double
            in if k >= expMax
               then
                 -- infinity
                 inexact positiveInfinity -- TowardNearest
                         positiveInfinity -- TowardInf
                         maxFinite -- TowardNegInf
                         maxFinite -- TowardZero
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
{-# SPECIALIZE fromPositiveIntF :: Rounding rn => Integer -> Rounded rn Float #-}
{-# SPECIALIZE fromPositiveIntF :: Rounding rn => Integer -> OppositeRoundingMode (Rounded rn) Float #-}
{-# SPECIALIZE fromPositiveIntF :: Integer -> Product (Rounded 'TowardNegInf) (Rounded 'TowardInf) Float #-}
{-# SPECIALIZE fromPositiveIntF :: Integer -> OppositeRoundingMode (Product (Rounded 'TowardNegInf) (Rounded 'TowardInf)) Float #-}
{-# SPECIALIZE fromPositiveIntF :: Integer -> DynamicRoundingMode Double #-}
{-# SPECIALIZE fromPositiveIntF :: Integer -> OppositeRoundingMode DynamicRoundingMode Double #-}
{-# SPECIALIZE fromPositiveIntF :: Rounding rn => Integer -> Rounded rn Double #-}
{-# SPECIALIZE fromPositiveIntF :: Rounding rn => Integer -> OppositeRoundingMode (Rounded rn) Double #-}
{-# SPECIALIZE fromPositiveIntF :: Integer -> Product (Rounded 'TowardNegInf) (Rounded 'TowardInf) Double #-}
{-# SPECIALIZE fromPositiveIntF :: Integer -> OppositeRoundingMode (Product (Rounded 'TowardNegInf) (Rounded 'TowardInf)) Double #-}

fromRatio :: (RealFloat a, RealFloatConstants a)
          => RoundingMode
          -> Integer -- ^ numerator
          -> Integer -- ^ denominator
          -> a
fromRatio rn n d = withRoundingMode (fromRatioF n d) rn
{-# SPECIALIZE fromRatio :: RoundingMode -> Integer -> Integer -> Float #-}
{-# SPECIALIZE fromRatio :: RoundingMode -> Integer -> Integer -> Double #-}

fromRatioF :: forall a f. (RealFloat a, RealFloatConstants a, Result f)
           => Integer -- ^ numerator
           -> Integer -- ^ denominator
           -> f a
fromRatioF !_ !_ | floatRadix (undefined :: a) /= 2 = error "radix other than 2 is not supported"
fromRatioF 0 _ = exact 0
fromRatioF n 0 | n > 0 = exact positiveInfinity -- positive infinity
               | otherwise = exact negativeInfinity -- negative infinity
fromRatioF n d | d < 0 = error "fromRatio: negative denominator"
               | n < 0 = negate <$> withOppositeRoundingMode (fromPositiveRatioF (- n) d)
               | otherwise = fromPositiveRatioF n d
{-# INLINE fromRatioF #-}

-- n > 0, d > 0
fromPositiveRatioF :: forall a f. (RealFloat a, RealFloatConstants a, Result f)
                   => Integer -> Integer -> f a
fromPositiveRatioF !n !d
  = let ln = integerLog2' n
        ld = integerLog2' d
        !fDigits = floatDigits (undefined :: a) -- 53 for Double
        e = ln - ld - fDigits
        (!q, !r) | e >= 0 = n `quotRem` (d `unsafeShiftL` e)
                 | otherwise = (n `unsafeShiftL` (-e)) `quotRem` d
        -- e >= 0: n = q * (d * 2^e) + r, 0 <= r < d * 2^e
        -- e <= 0: n * 2^(-e) = q * d + r, 0 <= r < d
        -- n / d * 2^^(-e) = q + r / d
        -- 52 <= log2 q < 54
        (!q', !r', !d', !e') | q < (1 `unsafeShiftL` fDigits) = (q, r, d, e)
                             | e >= 0 = let (q'', r'') = q `quotRem` 2
                                        in (q'', r'' * (d `shiftL` e) + r, d, e + 1)
                             | otherwise = let (q'', r'') = q `quotRem` 2
                                           in (q'', r'' * d + r, 2 * d, e + 1)
        -- n' / d' * 2^^(-e') = q' + r' / d', 2^52 <= q' < 2^53
        -- e >= 0: n = (2 * (q `quot` 2) + (q `rem` 2)) * (d * 2^e) + r
        --           = (q `quot` 2) * (d * 2^(e+1)) + (q `rem` 2) * (d * 2^e) + r
        -- e < 0: n * 2^(-e) = (2 * (q `quot` 2) + (q `rem` 2)) * d + r
        --                   = (q `quot` 2) * (2 * d) + (q `rem` 2) * d + r
        -- q' * 2^^e' <= n/d < (q'+1) * 2^^e', 2^52 <= q' < 2^53
        -- (q'/2^53) * 2^^(e'+53) <= n/d < (q'+1)/2^53 * 2^^(e'+53), 1/2 <= q'/2^53 < 1
        (!expMin, !expMax) = floatRange (undefined :: Double) -- (-1021, 1024) for Double
        -- normal: 0x1p-1022 <= x <= 0x1.fffffffffffffp+1023
    in if expMin <= e'+fDigits && e'+fDigits < expMax
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
         if expMax <= e'+fDigits
         then
           -- infinity
           inexact positiveInfinity -- TowardNearest
                   positiveInfinity -- TowardInf
                   maxFinite -- TowardNegInf
                   maxFinite -- TowardZero
         else
           -- e'+53 < expMin (e' < expMin - 53 = -1074)
           -- subnormal: 0 <= rounded(n/d) <= 0x1p-1022, minimum (positive) subnormal: 0x1p-1074
           -- e'+53 < expMin = -1021,  i.e. e < expMin - 53 = -1074
           -- q' * 2^^e' = q' * 2^^(e'+1074) * 2^^(-1074)
           --            = ((q' `quot` (2^(-1074-e'))) * (2^(-1074-e')) + (q' `rem` (2^(-1074-e')))) * 2^^(e'+1074) * 2^^(-1074)
           --            = (q' `quot` (2^(-1074-e'))) * 2^^(-1074) + (q' `rem` (2^(-1074-e'))) * 2^^(e'+1074) * 2^^(-1074)
           --            = q'' * 2^^(-1074) + r'' * 2^^e'
           let (!q'', !r'') = q' `quotRem` (1 `unsafeShiftL` (expMin - fDigits - e'))
           in if r == 0 && r'' == 0
              then exact $ encodeFloat q'' (expMin - fDigits) -- exact
              else let down = encodeFloat q'' (expMin - fDigits)
                       up = encodeFloat (q'' + 1) (expMin - fDigits)
                       toNearest = case compare r' (2^(expMin - fDigits - e' - 1)) of
                         LT -> down
                         GT -> up
                         EQ | r /= 0    -> up
                            | even q'   -> down
                            | otherwise -> up
                   in inexact toNearest up down down
{-# SPECIALIZE fromPositiveRatioF :: Integer -> Integer -> DynamicRoundingMode Float #-}
{-# SPECIALIZE fromPositiveRatioF :: Integer -> Integer -> OppositeRoundingMode DynamicRoundingMode Float #-}
{-# SPECIALIZE fromPositiveRatioF :: Rounding rn => Integer -> Integer -> Rounded rn Float #-}
{-# SPECIALIZE fromPositiveRatioF :: Rounding rn => Integer -> Integer -> OppositeRoundingMode (Rounded rn) Float #-}
{-# SPECIALIZE fromPositiveRatioF :: Integer -> Integer -> Product (Rounded 'TowardNegInf) (Rounded 'TowardInf) Float #-}
{-# SPECIALIZE fromPositiveRatioF :: Integer -> Integer -> OppositeRoundingMode (Product (Rounded 'TowardNegInf) (Rounded 'TowardInf)) Float #-}
{-# SPECIALIZE fromPositiveRatioF :: Integer -> Integer -> DynamicRoundingMode Double #-}
{-# SPECIALIZE fromPositiveRatioF :: Integer -> Integer -> OppositeRoundingMode DynamicRoundingMode Double #-}
{-# SPECIALIZE fromPositiveRatioF :: Rounding rn => Integer -> Integer -> Rounded rn Double #-}
{-# SPECIALIZE fromPositiveRatioF :: Rounding rn => Integer -> Integer -> OppositeRoundingMode (Rounded rn) Double #-}
{-# SPECIALIZE fromPositiveRatioF :: Integer -> Integer -> Product (Rounded 'TowardNegInf) (Rounded 'TowardInf) Double #-}
{-# SPECIALIZE fromPositiveRatioF :: Integer -> Integer -> OppositeRoundingMode (Product (Rounded 'TowardNegInf) (Rounded 'TowardInf)) Double #-}
