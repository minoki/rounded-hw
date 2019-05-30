{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE HexFloatLiterals #-}
module Numeric.Rounded.Hardware.Internal where
import Data.Coerce
import Data.Proxy
import Data.Ratio
import Data.Bits
import Math.NumberTheory.Logarithms -- (integerLog2')
-- import GHC.Integer.Logarithms.Internals (integerLog2IsPowerOf2#)
-- integerLog2IsPowerOf2# :: Integer -> (# Int#, Int# #)

foreign import ccall unsafe "hs_rounded_c99_add_up"
  c_rounded_add_up :: Double -> Double -> Double

foreign import ccall unsafe "hs_rounded_c99_add_down"
  c_rounded_add_down :: Double -> Double -> Double

foreign import ccall unsafe "hs_rounded_c99_add_zero"
  c_rounded_add_zero :: Double -> Double -> Double

foreign import ccall unsafe "hs_rounded_c99_sub_up"
  c_rounded_sub_up :: Double -> Double -> Double

foreign import ccall unsafe "hs_rounded_c99_sub_down"
  c_rounded_sub_down :: Double -> Double -> Double

foreign import ccall unsafe "hs_rounded_c99_sub_zero"
  c_rounded_sub_zero :: Double -> Double -> Double

foreign import ccall unsafe "hs_rounded_c99_mul_up"
  c_rounded_mul_up :: Double -> Double -> Double

foreign import ccall unsafe "hs_rounded_c99_mul_down"
  c_rounded_mul_down :: Double -> Double -> Double

foreign import ccall unsafe "hs_rounded_c99_mul_zero"
  c_rounded_mul_zero :: Double -> Double -> Double

foreign import ccall unsafe "hs_rounded_c99_div_up"
  c_rounded_div_up :: Double -> Double -> Double

foreign import ccall unsafe "hs_rounded_c99_div_down"
  c_rounded_div_down :: Double -> Double -> Double

foreign import ccall unsafe "hs_rounded_c99_div_zero"
  c_rounded_div_zero :: Double -> Double -> Double

foreign import ccall unsafe "hs_rounded_c99_sqrt_up"
  c_rounded_sqrt_up :: Double -> Double

foreign import ccall unsafe "hs_rounded_c99_sqrt_down"
  c_rounded_sqrt_down :: Double -> Double

foreign import ccall unsafe "hs_rounded_c99_sqrt_zero"
  c_rounded_sqrt_zero :: Double -> Double

-- TODO: Use rounded package (Numeric.Rounded)
data RoundingMode
  = TowardNearest
  | TowardZero
  | TowardInf
  | TowardNegInf
  deriving (Eq, Ord, Read, Show, Bounded)

oppositeRoundingMode :: RoundingMode -> RoundingMode
oppositeRoundingMode TowardNearest = TowardNearest
oppositeRoundingMode TowardZero = TowardZero
oppositeRoundingMode TowardInf = TowardNegInf
oppositeRoundingMode TowardNegInf = TowardInf

newtype RoundedDouble (rn :: RoundingMode) = RoundedDouble Double
  deriving (Eq, Ord, Show)

getRoundedDouble :: RoundedDouble rn -> Double
getRoundedDouble (RoundedDouble x) = x

class RoundedPrim (rn :: RoundingMode) where
  rounding :: proxy rn -> RoundingMode
  addDouble :: proxy rn -> Double -> Double -> Double
  subDouble :: proxy rn -> Double -> Double -> Double
  mulDouble :: proxy rn -> Double -> Double -> Double
  divDouble :: proxy rn -> Double -> Double -> Double
  sqrtDouble :: proxy rn -> Double -> Double

instance RoundedPrim TowardNearest where
  rounding _ = TowardNearest
  addDouble _ = (+)
  subDouble _ = (-)
  mulDouble _ = (*)
  divDouble _ = (/)
  sqrtDouble _ = sqrt

instance RoundedPrim TowardInf where
  rounding _ = TowardInf
  addDouble _ = c_rounded_add_up
  subDouble _ = c_rounded_sub_up
  mulDouble _ = c_rounded_mul_up
  divDouble _ = c_rounded_div_up
  sqrtDouble _ = c_rounded_sqrt_up

instance RoundedPrim TowardNegInf where
  rounding _ = TowardNegInf
  addDouble _ = c_rounded_add_down
  subDouble _ = c_rounded_sub_down
  mulDouble _ = c_rounded_mul_down
  divDouble _ = c_rounded_div_down
  sqrtDouble _ = c_rounded_sqrt_down

instance RoundedPrim TowardZero where
  rounding _ = TowardZero
  addDouble _ = c_rounded_add_zero
  subDouble _ = c_rounded_sub_zero
  mulDouble _ = c_rounded_mul_zero
  divDouble _ = c_rounded_div_zero
  sqrtDouble _ = c_rounded_sqrt_zero

instance (RoundedPrim rn) => Num (RoundedDouble rn) where
  lhs@(RoundedDouble x) + RoundedDouble y = RoundedDouble (addDouble lhs x y)
  lhs@(RoundedDouble x) - RoundedDouble y = RoundedDouble (subDouble lhs x y)
  lhs@(RoundedDouble x) * RoundedDouble y = RoundedDouble (mulDouble lhs x y)
  negate = coerce (negate :: Double -> Double)
  abs = coerce (abs :: Double -> Double)
  signum = coerce (signum :: Double -> Double)
  fromInteger !n | abs n <= 2^53 = RoundedDouble (fromInteger n) -- exact
                 | n > 0 = let k = integerLog2' n -- floor (log2 n)
                               l = k - 52
                               -- Since abs n > 2^53, k >= 53 and l >= 1
                               -- (q, r) = n `quotRem` (2^l)
                               q = n `shiftR` l
                               r = n .&. (1 `shiftL` l)
                           in if r == 0
                              then let v = encodeFloat q l
                                   in RoundedDouble v -- exact
                                      -- TODO: overflow
                              else RoundedDouble (case rounding (Proxy :: Proxy rn) of
                                                    TowardInf -> encodeFloat (q + 1) l
                                                    TowardNegInf -> encodeFloat q l
                                                    TowardZero -> encodeFloat q l
                                                    TowardNearest | r `shiftR` (l - 1) == 0 -> encodeFloat q l
                                                                  | r == 2^(l - 1) -> if even q then encodeFloat q l else encodeFloat (q - 1) l
                                                                  | otherwise -> encodeFloat (q + 1) l
                                                 )
                 | otherwise = let n' = - n
                                   k = integerLog2' n' -- floor (log2 n)
                                   l = k - 52
                                   -- Since abs n > 2^53, k >= 53 and l >= 1
                                   -- (q, r) = n `quotRem` (2^l)
                                   q = n' `shiftR` l
                                   r = n' .&. (1 `shiftL` l)
                               in if r == 0
                                  then let v = - encodeFloat q l
                                       in RoundedDouble v -- exact
                                          -- TODO: overflow
                                  else RoundedDouble (- (case rounding (Proxy :: Proxy rn) of
                                                           TowardInf -> encodeFloat q l
                                                           TowardNegInf -> encodeFloat (q + 1) l
                                                           TowardZero -> encodeFloat q l
                                                           TowardNearest | r `shiftR` (l - 1) == 0 -> encodeFloat q l
                                                                         | r == 2^(l - 1) -> if even q then encodeFloat q l else encodeFloat (q - 1) l
                                                                         | otherwise -> encodeFloat (q + 1) l
                                                        )
                                                     )

countTrailingZerosInteger :: Integer -> Int
countTrailingZerosInteger x
  | x == 0 = error "countTrailingZerosInteger: zero"
  {- | odd x = 0 -}
  | otherwise = integerLog2 (x `xor` (x - 1))

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

instance (RoundedPrim rn) => Fractional (RoundedDouble rn) where
  fromRational x
    | abs (numerator x) <= 2^53 && abs (denominator x) <= 2^53
    = let n' = fromInteger (numerator x)
          d' = fromInteger (denominator x)
      in RoundedDouble (divDouble (Proxy :: Proxy rn) n' d')
    | otherwise = RoundedDouble $ fromRatio (rounding (Proxy :: Proxy rn)) (numerator x) (denominator x)
  recip a@(RoundedDouble x) = RoundedDouble (divDouble a 1 x)
  lhs@(RoundedDouble x) / RoundedDouble y = RoundedDouble (divDouble lhs x y)
