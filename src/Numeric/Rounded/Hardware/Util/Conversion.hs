{-# LANGUAGE HexFloatLiterals #-}
module Numeric.Rounded.Hardware.Util.Conversion where
import Numeric.Rounded.Hardware.Rounding
import Data.Bits
import Math.NumberTheory.Logarithms -- (integerLog2')
-- import GHC.Integer.Logarithms.Internals (integerLog2IsPowerOf2#)
-- integerLog2IsPowerOf2# :: Integer -> (# Int#, Int# #)

fromInt :: RoundingMode -> Integer -> Double
fromInt rn 0 = 0
fromInt rn n | n < 0 = - fromInt (oppositeRoundingMode rn) (- n)
-- Now n > 0
fromInt rn n
  = let k = integerLog2' n -- floor (log2 n)
        -- 2^k <= n < 2^(k+1)
    in if k < 53
       then fromInteger n
       else let e = k - 52
                (q, r) = n `quotRem` (2^e)
                -- 2^52 <= q < 2^53, 0 <= r < 2^(k-52)
                (expMin, expMax) = floatRange (undefined :: Double) -- (-1021, 1024) for Double
            in if k >= expMax
               then
                 -- infinity
                 case rn of
                   TowardNegInf  -> 0x1.fffffffffffffp+1023 -- max finite
                   TowardZero    -> 0x1.fffffffffffffp+1023
                   TowardInf     -> 1/0 -- infinity
                   TowardNearest -> 1/0 -- infinity
               else
                 if r == 0
                 then encodeFloat q e -- exact
                 else
                   -- inexact
                   case rn of
                     TowardNegInf -> encodeFloat q e
                     TowardZero   -> encodeFloat q e
                     TowardInf    -> encodeFloat (q + 1) e
                     TowardNearest -> case compare r (2^(e-1)) of
                       LT -> encodeFloat q e
                       EQ | even q -> encodeFloat q e
                          | otherwise -> encodeFloat (q + 1) e
                       GT -> encodeFloat (q + 1) e

fromRatio :: RoundingMode
          -> Integer -- ^ numerator
          -> Integer -- ^ denominator
          -> Double
fromRatio _rn 0 _ = 0
fromRatio rn n 0 | n > 0 = 1/0 -- positive infinity
                 | otherwise = -1/0 -- negative infinity
fromRatio rn n d | n < 0 = - fromRatio (oppositeRoundingMode rn) (- n) d
fromRatio rn n d | d < 0 = error "fromRatio: negative denominator"
-- Now n > 0, d > 0
fromRatio rn n d
  = let ln = integerLog2 n
        ld = integerLog2 d
        e = ln - ld - 53
        (q, r) | e >= 0 = n `quotRem` (d `shiftL` e)
               | e <  0 = (n `shiftL` (-e)) `quotRem` d
        -- e >= 0: n = q * (d * 2^e) + r, 0 <= r < d * 2^e
        -- e <= 0: n * 2^(-e) = q * d + r, 0 <= r < d
        -- n / d * 2^^(-e) = q + r / d
        -- 52 <= log2 q < 54
        (q', r', d', e') | q < 2^53 = (q, r, d, e)
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
           encodeFloat q' e' -- exact
         else
           -- inexact
           -- encodeFloat (q' + 1) e' may be infinity
           case rn of
             TowardNegInf  -> encodeFloat q' e'
             TowardZero    -> encodeFloat q' e'
             TowardInf     -> encodeFloat (q' + 1) e'
             TowardNearest -> case compare (2 * r') d' of
               LT -> encodeFloat q' e'
               EQ | even q' -> encodeFloat q' e'
                  | otherwise -> encodeFloat (q' + 1) e' -- q' + 1 is even
               GT -> encodeFloat (q' + 1) e'
       else
         -- infinity or subnormal
         if expMax <= e'+53
         then
           -- infinity
           case rn of
             TowardNegInf  -> 0x1.fffffffffffffp+1023 -- max finite
             TowardZero    -> 0x1.fffffffffffffp+1023
             TowardInf     -> 1/0 -- infinity
             TowardNearest -> 1/0 -- infinity
         else
           -- e'+53 < expMin (e' < expMin - 53 = -1074)
           -- subnormal: 0 <= rounded(n/d) <= 0x1p-1022, minimum (positive) subnormal: 0x1p-1074
           -- e'+53 < expMin = -1021,  i.e. e < expMin - 53 = -1074
           -- q' * 2^^e' = q' * 2^^(e'+1074) * 2^^(-1074)
           --            = ((q' `quot` (2^(-1074-e'))) * (2^(-1074-e')) + (q' `rem` (2^(-1074-e')))) * 2^^(e'+1074) * 2^^(-1074)
           --            = (q' `quot` (2^(-1074-e'))) * 2^^(-1074) + (q' `rem` (2^(-1074-e'))) * 2^^(e'+1074) * 2^^(-1074)
           --            = q'' * 2^^(-1074) + r'' * 2^^e'
           let (q'', r'') = q' `quotRem` (2^(expMin-53-e'))
           in if r == 0 && r'' == 0
              then encodeFloat q'' (expMin-53) -- exact
              else case rn of
                     TowardNegInf -> encodeFloat q'' (expMin-53)
                     TowardZero   -> encodeFloat q'' (expMin-53)
                     TowardInf    -> encodeFloat (q'' + 1) (expMin-53)
                     TowardNearest -> case compare r' (2^(expMin - 53 - e' - 1)) of
                       LT -> encodeFloat q'' (expMin-53)
                       GT -> encodeFloat (q'' + 1) (expMin-53)
                       EQ | r /= 0    -> encodeFloat (q'' + 1) (expMin-53)
                          | even q'   -> encodeFloat q'' (expMin-53)
                          | otherwise -> encodeFloat (q'' + 1) (expMin-53)
