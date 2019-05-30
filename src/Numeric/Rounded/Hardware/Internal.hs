{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
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

fromRatio :: RoundingMode -> Integer -> Integer -> Double
fromRatio _rn 0 _ = 0
fromRatio rn n 0 | n > 0 = 1/0 -- positive infinity
                 | otherwise = -1/0 -- negative infinity
fromRatio rn n d | n < 0 = - fromRatio (oppositeRoundingMode rn) (- n) d
fromRatio rn n d | d < 0 = error "fromRatio: negative denominator"
-- Now n > 0, d > 0
fromRatio rn n d
  = let ln = integerLog2 n
        ld = integerLog2 d
        (q, r) = (n `shiftL` (53 + ld - ln)) `quotRem` d
        -- n * 2^(53+ld-ln) = q * d + r, 0 <= r < d
        -- n/d = q * 2^(ln-ld-53) + r*2^(ln-ld-53)/d
        -- 52 <= log2 q < 54
        q' = q `shiftR` 1
    in if r == 0 && (q < 2^53 || even q)
       then
         -- exact
         encodeFloat q (ln - ld - 53)
       else
         -- inexact
         case rn of
           TowardNegInf | q < 2^53  -> encodeFloat q  (ln - ld - 53)
                        | otherwise -> encodeFloat q' (ln - ld - 52)
           TowardZero   | q < 2^53  -> encodeFloat q  (ln - ld - 53)
                        | otherwise -> encodeFloat q' (ln - ld - 52)
           TowardInf    | q < 2^53  -> encodeFloat (q + 1) (ln - ld - 53)
                        | otherwise -> encodeFloat (q' + 1) (ln - ld - 52)
           TowardNearest | q < 2^53 -> case compare (2 * r) d of
                                         LT -> encodeFloat q (ln - ld - 53)
                                         EQ | even q -> encodeFloat q (ln - ld - 53)
                                            | otherwise -> encodeFloat (q + 1) (ln - ld - 53)
                                         GT -> encodeFloat (q + 1) (ln - ld - 53)
                         | even q -> encodeFloat q (ln - ld - 53)
                         | {- odd q, -} r == 0 ->
                                        if even q'
                                        then encodeFloat q' (ln - ld - 52)
                                        else encodeFloat (q' + 1) (ln - ld - 52)
                         | otherwise {- odd q, r > 0 -} ->
                             encodeFloat (q' + 1) (ln - ld - 52)

instance (RoundedPrim rn) => Fractional (RoundedDouble rn) where
  fromRational x
    | abs (numerator x) <= 2^53 && abs (denominator x) <= 2^53
    = let n' = fromInteger (numerator x)
          d' = fromInteger (denominator x)
      in RoundedDouble (divDouble (Proxy :: Proxy rn) n' d')
    | otherwise = RoundedDouble $ fromRatio (rounding (Proxy :: Proxy rn)) (numerator x) (denominator x)
  recip a@(RoundedDouble x) = RoundedDouble (divDouble a 1 x)
  lhs@(RoundedDouble x) / RoundedDouble y = RoundedDouble (divDouble lhs x y)
