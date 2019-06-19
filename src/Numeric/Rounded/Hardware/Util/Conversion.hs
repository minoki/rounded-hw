{-# LANGUAGE HexFloatLiterals #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
module Numeric.Rounded.Hardware.Util.Conversion where
import Numeric.Rounded.Hardware.Rounding
import Numeric.Rounded.Hardware.Util.RoundedResult
import Data.Bits
import Data.Functor.Product
import Math.NumberTheory.Logarithms (integerLog2')
-- import GHC.Integer.Logarithms.Internals (integerLog2IsPowerOf2#)
-- integerLog2IsPowerOf2# :: Integer -> (# Int#, Int# #)

fromInt :: RoundingMode -> Integer -> Double
fromInt rn n = withRoundingMode (fromIntF n) rn

fromIntF :: Result f => Integer -> f Double
fromIntF 0 = exact 0
fromIntF n | n < 0 = negate <$> withOppositeRoundingMode (fromPositiveInt (- n))
           | otherwise = fromPositiveInt n
  where
    -- n > 0
    fromPositiveInt :: Result f => Integer -> f Double
    fromPositiveInt !n
      = let !k = integerLog2' n -- floor (log2 n)
            -- 2^k <= n < 2^(k+1)
        in if k < 53
           then exact (fromInteger n)
           else let e = k - 52
                    -- (!q, !r) = n `quotRem` (1 `unsafeShiftL` e)
                    q = n `unsafeShiftR` e
                    r = n .&. ((1 `unsafeShiftL` e) - 1)
                    -- 2^52 <= q < 2^53, 0 <= r < 2^(k-52)
                    (expMin, expMax) = floatRange (undefined :: Double) -- (-1021, 1024) for Double
                in if k >= expMax
                   then
                     -- infinity
                     let maxFinite = 0x1.fffffffffffffp+1023
                         inf = 1/0
                     in inexact inf -- TowardNearest
                                inf -- TowardInf
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
    {-# SPECIALIZE fromPositiveInt :: Integer -> DynamicRoundingMode Double #-}
    {-# SPECIALIZE fromPositiveInt :: Integer -> OppositeRoundingMode DynamicRoundingMode Double #-}
    {-# SPECIALIZE fromPositiveInt :: Rounding rn => Integer -> Rounded rn Double #-}
    {-# SPECIALIZE fromPositiveInt :: Rounding rn => Integer -> OppositeRoundingMode (Rounded rn) Double #-}
    {-# SPECIALIZE fromPositiveInt :: Integer -> Product (Rounded TowardNegInf) (Rounded TowardInf) Double #-}
    {-# SPECIALIZE fromPositiveInt :: Integer -> OppositeRoundingMode (Product (Rounded TowardNegInf) (Rounded TowardInf)) Double #-}
{-# INLINE fromIntF #-}

fromRatio :: RoundingMode
          -> Integer -- ^ numerator
          -> Integer -- ^ denominator
          -> Double
fromRatio rn n d = withRoundingMode (fromRatioF n d) rn

fromRatioF :: Result f
           => Integer -- ^ numerator
           -> Integer -- ^ denominator
           -> f Double
fromRatioF 0 _ = exact 0
fromRatioF n 0 | n > 0 = exact (1/0) -- positive infinity
               | otherwise = exact (-1/0) -- negative infinity
fromRatioF n d | d < 0 = error "fromRatio: negative denominator"
               | n < 0 = negate <$> withOppositeRoundingMode (fromPositiveRatio (- n) d)
               | otherwise = fromPositiveRatio n d
  where
    -- n > 0, d > 0
    fromPositiveRatio :: Result f => Integer -> Integer -> f Double
    fromPositiveRatio !n !d
      = let ln = integerLog2' n
            ld = integerLog2' d
            e = ln - ld - 53
            (!q, !r) | e >= 0 = n `quotRem` (d `unsafeShiftL` e)
                     | e <  0 = (n `unsafeShiftL` (-e)) `quotRem` d
            -- e >= 0: n = q * (d * 2^e) + r, 0 <= r < d * 2^e
            -- e <= 0: n * 2^(-e) = q * d + r, 0 <= r < d
            -- n / d * 2^^(-e) = q + r / d
            -- 52 <= log2 q < 54
            (!q', !r', !d', !e') | q < (1 `unsafeShiftL` 53) = (q, r, d, e)
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
            (expMin, expMax) = floatRange (undefined :: Double) -- (-1021, 1024) for Double
            -- normal: 0x1p-1022 <= x <= 0x1.fffffffffffffp+1023
      in if expMin <= e'+53 && e'+53 < expMax
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
           if expMax <= e'+53
           then
             -- infinity
             let maxFinite = 0x1.fffffffffffffp+1023
                 inf = 1/0
             in inexact inf -- TowardNearest
                        inf -- TowardInf
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
             let (!q'', !r'') = q' `quotRem` (1 `unsafeShiftL` (expMin-53-e'))
             in if r == 0 && r'' == 0
                then exact $ encodeFloat q'' (expMin-53) -- exact
                else let down = encodeFloat q'' (expMin-53)
                         up = encodeFloat (q'' + 1) (expMin-53)
                         toNearest = case compare r' (2^(expMin - 53 - e' - 1)) of
                           LT -> down
                           GT -> up
                           EQ | r /= 0    -> up
                              | even q'   -> down
                              | otherwise -> up
                     in inexact toNearest up down down
    {-# SPECIALIZE fromPositiveRatio :: Integer -> Integer -> DynamicRoundingMode Double #-}
    {-# SPECIALIZE fromPositiveRatio :: Integer -> Integer -> OppositeRoundingMode DynamicRoundingMode Double #-}
    {-# SPECIALIZE fromPositiveRatio :: Rounding rn => Integer -> Integer -> Rounded rn Double #-}
    {-# SPECIALIZE fromPositiveRatio :: Rounding rn => Integer -> Integer -> OppositeRoundingMode (Rounded rn) Double #-}
    {-# SPECIALIZE fromPositiveRatio :: Integer -> Integer -> Product (Rounded TowardNegInf) (Rounded TowardInf) Double #-}
    {-# SPECIALIZE fromPositiveRatio :: Integer -> Integer -> OppositeRoundingMode (Product (Rounded TowardNegInf) (Rounded TowardInf)) Double #-}
{-# INLINE fromRatioF #-}
