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
import Data.Char (intToDigit)
import Data.Bifunctor (first)
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

instance (RoundedPrim rn) => Num (RoundedDouble rn) where
  lhs@(RoundedDouble x) + RoundedDouble y = RoundedDouble (addDouble lhs x y)
  lhs@(RoundedDouble x) - RoundedDouble y = RoundedDouble (subDouble lhs x y)
  lhs@(RoundedDouble x) * RoundedDouble y = RoundedDouble (mulDouble lhs x y)
  negate = coerce (negate :: Double -> Double)
  abs = coerce (abs :: Double -> Double)
  signum = coerce (signum :: Double -> Double)
  fromInteger n = RoundedDouble (fromInt (rounding (Proxy :: Proxy rn)) n)

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

-- ratToDigitsRn :: RoundingMode -> Int -> Int -> Rational -> ([Int], Int)

-- doubleToDecimalDigitsRn _ prec x = ([d1,d2,...,dn], e)
-- 0 <= n <= prec + 1, x = 0.d1d2...dn * (10^^e) up to rounding
-- 0 <= di < 10
doubleToDecimalDigitsRn :: RoundingMode -- ^ rounding mode
                        -> Int -- ^ prec
                        -> Double -- ^ a non-negative number (zero, normal or subnormal)
                        -> ([Int], Int)
doubleToDecimalDigitsRn _rn _prec 0 = ([], 0)
-- doubleToDecimalDigitsRn _rn _prec x | floatRadix x /= 2 = error "floatRadix x must be 2"
doubleToDecimalDigitsRn rn prec x =
  -- x > 0
  let (m,n) = decodeFloat x
      -- x = m*2^n, 2^52 <= m < 2^53
      -- 2^(-1074) <= x < 2^1024
      -- => -1074-52=-1126 <= n < 1024-52=972
      -- d = floatDigits x -- d=53 for Double
      e0 = floor (fromIntegral (52 + n) * logBase 10 2 :: Double) - prec
      -- TODO: precision?
      -- TODO: Use rational approximation for logBase 10 2?
      (s,t) | n < 0,       0 <= e0 = (m,     2^(-n) * 10^e0)
            | {- n >= 0 -} 0 <= e0 = (m * 2^n,        10^e0)
            | n < 0   {- e0 < 0 -} = (m * 10^(-e0),  2^(-n))
            | otherwise            = (m * 2^n * 10^(-e0), 1)
      -- s/t = m * 2^n * 10^(-e0)
      (q,r) = s `quotRem` t
      -- s = q * t + r
      -- 10^prec <= q + r/t < 2 * 10^(prec+1)
      (q',r',t',e') | 10^(prec+1) <= q = case q `quotRem` 10 of
                                           -- q = q'*10+r'
                                           -- s = (q'*10+r')*t + r = q'*10*t+(r'*t+r)
                                           (q',r') -> (q', r'*t+r, 10*t, e0+1)
                    | otherwise = (q,r,t,e0)
      -- 10^prec <= q' + r'/t' < 10^(prec+1), 0 <= r' < t'

      -- x = m*2^n
      --   = s/t * 10^^(e0)
      --   = (q + r/t) * 10^^(e0)
      --   = (q' + r'/t') * 10^^e'

      -- loop0 e n: x = n * 10^(e-prec-1)
      loop0 !e 0 = ([], 0) -- should not occur
      loop0 !e a = case a `quotRem` 10 of
                     (q,0) -> loop0 (e+1) q
                     (q,r) -> loop (e+1) [fromInteger r] q

      -- loop e acc a: (a + 0.<acc>)*10^(e-prec-1)
      loop !e acc 0 = (acc, e)
      loop !e acc a = case a `quotRem` 10 of
                        (q,r) -> loop (e+1) (fromInteger r : acc) q
  in if r' == 0
     then
       -- exact
       loop0 e' q'
     else
       -- inexact
       case rn of
         TowardNegInf -> loop0 e' q'
         TowardZero   -> loop0 e' q'
         TowardInf    -> loop0 e' (q' + 1)
         TowardNearest -> case compare (2 * r') t' of
           LT -> loop0 e' q'
           EQ | even q' -> loop0 e' q'
              | otherwise -> loop0 e' (q' + 1)
           GT -> loop0 e' (q' + 1)

-- doubleToFixedDecimalDigitsRn _ prec x = [d1,d2,...,dn]
-- x = d1d2...dn * (10^^(-prec)) up to rounding
-- 0 <= di < 10
doubleToFixedDecimalDigitsRn :: RoundingMode -- ^ rounding mode
                             -> Int -- ^ prec
                             -> Double -- ^ a non-negative number (zero, normal or subnormal)
                             -> [Int]
doubleToFixedDecimalDigitsRn rn prec x =
  let (m,e) = decodeFloat x -- x = m*2^e
      (s,t) | prec >= 0, e + prec >= 0     = (m * 2^(e+prec) * 5^prec, 1)
            | prec >= 0 {- e + prec < 0 -} = (m * 5^prec, 2^(-e-prec))
            | {- prec < 0 -} e + prec >= 0 = (m * 2^(e+prec), 5^(-prec))
            | otherwise {- prec < 0, e + prec < 0 -} = (m, 2^(-e-prec) * 5^(-prec))
      -- x*10^^prec = s/t
      (q,r) = s `quotRem` t
      loop acc 0 = acc
      loop acc a = case a `quotRem` 10 of
                     (q,r) -> loop (fromInteger r : acc) q
  in if r == 0
     then
       -- exact
       loop [] q
     else
       -- inexact
       case rn of
         TowardNegInf -> loop [] q
         TowardZero -> loop [] q
         TowardInf -> loop [] (q + 1)
         TowardNearest -> case compare (2 * r) t of
           LT -> loop [] q
           EQ | even q -> loop [] q
              | otherwise -> loop [] (q + 1)
           GT -> loop [] (q + 1)

-- doubleToDecimalDigits x = ([d1,d2,...,dn], e)
-- n >= 0, x = 0.d1d2...dn * (10^^e)
-- 0 <= di < 10
doubleToDecimalDigits :: Double -- ^ a non-negative number (zero, normal or subnormal)
                      -> ([Int], Int)
doubleToDecimalDigits 0 = ([], 0)
doubleToDecimalDigits x =
  let (m,n) = decodeFloat x -- x = m*2^n
      z = countTrailingZerosInteger m
      (m',n') = (m `shiftR` z, n + z)
      -- x = m*2^n = m'*2^n'
      (m'',e) | n' < 0 = (m' * 5^(-n'), n') -- x = m'/2^(-n') = m'*5^(-n') / 10^(-n')
              | otherwise = (m' * 2^n', 0)
      -- x = m''*10^e, m'' is an integer, e <= 0

      -- x = a*10^e, a is an integer
      loop0 !e 0 = ([0], 0) -- should not occur
      loop0 !e a = case a `quotRem` 10 of
                     (q,0) -> loop0 (e+1) q
                     (q,r) -> loop (e+1) [fromInteger r] q

      -- x = (a + 0.<acc>)*10^e, a is an integer
      loop !e acc 0 = (acc, e)
      loop !e acc n = case n `quotRem` 10 of
                        (q,r) -> loop (e+1) (fromInteger r : acc) q
  in loop0 e m''

-- TODO: Maybe implement ByteString or Text versions

-- |
-- >>> showEFloatRn TowardNearest (Just 0) 0 ""
-- "0e0"
-- >>> showEFloatRn TowardNearest Nothing 0 ""
-- "0.0e0"
showEFloatRn :: RoundingMode -> Maybe Int -> Double -> ShowS
showEFloatRn rn mprec x
  | isNaN x = showString "NaN"
  | x < 0 || isNegativeZero x = showChar '-' . showEFloatRn (oppositeRoundingMode rn) mprec (-x)
  | isInfinite x = showString "Infinity"
  | otherwise = let (xs,e) = case mprec of
                      Nothing -> doubleToDecimalDigits x
                      Just prec -> let !prec' = max prec 0
                                   in first (padRight0 (prec' + 1)) $ doubleToDecimalDigitsRn rn prec' x
                    e' | all (== 0) xs = 0
                       | otherwise = e - 1
                in case xs of
                     [] -> showString "0.0e0" -- mprec must be `Nothing`
                     [0] -> showString "0e0" -- mprec must be `Just 0`
                     [d] -> case mprec of
                              Nothing -> showString $ intToDigit d : '.' : '0' : 'e' : show e'
                              _ -> showString $ intToDigit d : 'e' : show e'
                     (d:ds) -> showString $ (intToDigit d : '.' : map intToDigit ds) ++ ('e' : show e')
  where
    padRight0 :: Int -> [Int] -> [Int]
    padRight0 0 xs = xs
    padRight0 !n [] = replicate n 0
    padRight0 !n (x:xs) = x : padRight0 (n - 1) xs

showFFloatRn :: RoundingMode -> Maybe Int -> Double -> ShowS
showFFloatRn rn mprec x
  | isNaN x = showString "NaN"
  | x < 0 || isNegativeZero x = showChar '-' . showFFloatRn (oppositeRoundingMode rn) mprec (-x)
  | isInfinite x = showString "Infinity"
  | otherwise = case mprec of
                  Nothing -> let (xs,e) = doubleToDecimalDigits x
                                 l = length xs
                             in if e >= l
                                then showString (map intToDigit xs ++ replicate (e - l) '0' ++ ".0")
                                else if e >= 0 -- 0 <= e < l
                                     then if l == e -- null zs
                                          then showString (map intToDigit xs ++ ".0")
                                          else let (ys,zs) = splitAt (l - e) xs
                                               in showString (map intToDigit ys ++ "." ++ map intToDigit zs)
                                     else -- e < 0
                                       showString ("0." ++ replicate (-e) '0' ++ map intToDigit xs)
                  Just prec -> let prec' = max prec 0
                                   xs = doubleToFixedDecimalDigitsRn rn prec' x
                                   l = length xs
                               in if prec' == 0
                                  then if null xs
                                       then showString "0"
                                       else showString $ map intToDigit xs
                                  else if l <= prec'
                                       then showString $ "0." ++ replicate (prec' - l) '0' ++ map intToDigit xs
                                       else let (ys,zs) = splitAt (l - prec') xs
                                                ys' | null ys = [0]
                                                    | otherwise = ys
                                            in showString $ map intToDigit ys' ++ "." ++ map intToDigit zs

showGFloatRn :: RoundingMode -> Maybe Int -> Double -> ShowS
showGFloatRn rn mprec x | x == 0 || (0.1 <= abs x && abs x < 10^7) = showFFloatRn rn mprec x -- Note that 1%10 < toRational (0.1 :: Double)
                        | otherwise = showEFloatRn rn mprec x

{-
showFFloatAltRn :: RoundingMode -> Maybe Int -> Double -> ShowS
showGFloatAltRn :: RoundingMode -> Maybe Int -> Double -> ShowS
-- showFloat :: RoundingMode -> Double -> ShowS
-}

foreign import ccall unsafe "nextafter" c_nextafter :: Double -> Double -> Double
