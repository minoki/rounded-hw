{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Numeric.Rounded.Hardware.Internal.Show where
import Numeric.Rounded.Hardware.Internal.Rounding
import Data.Char (intToDigit)
import Data.Bifunctor (first)
import Data.Bits
import Math.NumberTheory.Logarithms

-- $setup
-- >>> import Data.Int

-- |
-- prop> \x -> x == 0 || countTrailingZerosInteger (fromIntegral x) == countTrailingZeros (x :: Int64)
-- >>> countTrailingZerosInteger 7
-- 0
-- >>> countTrailingZerosInteger 8
-- 3
countTrailingZerosInteger :: Integer -> Int
countTrailingZerosInteger x
  | x == 0 = error "countTrailingZerosInteger: zero"
  | otherwise = integerLog2 (x `xor` (x - 1))

-- ratToDigitsRn :: RoundingMode -> Int -> Int -> Rational -> ([Int], Int)

-- binaryFloatToDecimalDigitsRn _ prec x = ([d1,d2,...,dn], e)
-- 0 <= n <= prec + 1, x = 0.d1d2...dn * (10^^e) up to rounding
-- 0 <= di < 10
binaryFloatToDecimalDigitsRn :: forall a. RealFloat a
                             => RoundingMode -- ^ rounding mode
                             -> Int -- ^ prec
                             -> a -- ^ a non-negative number (zero, normal or subnormal)
                             -> ([Int], Int)
binaryFloatToDecimalDigitsRn _rn _prec 0 = ([], 0)
binaryFloatToDecimalDigitsRn _rn _prec x | floatRadix x /= 2 = error "radix must be 2"
binaryFloatToDecimalDigitsRn rn prec x =
  -- x > 0
  let (m,n) = decodeFloat x
      d = floatDigits x -- d=53 for Double
      -- x = m*2^n, 2^(d-1) <= m < 2^d
      -- 2^(-1074) <= x < 2^1024
      -- => -1074-52=-1126 <= n < 1024-52=972
      e0 = floor (fromIntegral (d - 1 + n) * logBase 10 2 :: a) - prec
      -- TODO: precision of logBase 10 2?
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
{-# SPECIALIZE binaryFloatToDecimalDigitsRn :: RoundingMode -> Int -> Double -> ([Int], Int) #-}

-- binaryFloatToFixedDecimalDigitsRn _ prec x = [d1,d2,...,dn]
-- x = d1d2...dn * (10^^(-prec)) up to rounding
-- 0 <= di < 10
binaryFloatToFixedDecimalDigitsRn :: forall a. RealFloat a
                                  => RoundingMode -- ^ rounding mode
                                  -> Int -- ^ prec
                                  -> a -- ^ a non-negative number (zero, normal or subnormal)
                                  -> [Int]
binaryFloatToFixedDecimalDigitsRn _rn _prec x | floatRadix x /= 2 = error "radix must be 2"
binaryFloatToFixedDecimalDigitsRn rn prec x =
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
{-# SPECIALIZE binaryFloatToFixedDecimalDigitsRn :: RoundingMode -> Int -> Double -> [Int] #-}

-- binaryFloatToDecimalDigits x = ([d1,d2,...,dn], e)
-- n >= 0, x = 0.d1d2...dn * (10^^e)
-- 0 <= di < 10
binaryFloatToDecimalDigits :: RealFloat a
                           => a -- ^ a non-negative number (zero, normal or subnormal)
                           -> ([Int], Int)
binaryFloatToDecimalDigits 0 = ([], 0)
binaryFloatToDecimalDigits x | floatRadix x /= 2 = error "radix must be 2"
binaryFloatToDecimalDigits x =
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
{-# SPECIALIZE binaryFloatToDecimalDigits :: Double -> ([Int], Int) #-}

-- TODO: Maybe implement ByteString or Text versions

-- |
-- >>> showEFloatRn TowardNearest (Just 0) 0 ""
-- "0e0"
-- >>> showEFloatRn TowardNearest Nothing 0 ""
-- "0.0e0"
showEFloatRn :: RealFloat a => RoundingMode -> Maybe Int -> a -> ShowS
showEFloatRn rn mprec x
  | isNaN x = showString "NaN"
  | x < 0 || isNegativeZero x = showChar '-' . showEFloatRn (oppositeRoundingMode rn) mprec (-x)
  | isInfinite x = showString "Infinity"
  | otherwise = let (xs,e) = case mprec of
                      Nothing -> binaryFloatToDecimalDigits x
                      Just prec -> let !prec' = max prec 0
                                   in first (padRight0 (prec' + 1)) $ binaryFloatToDecimalDigitsRn rn prec' x
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
{-# SPECIALIZE showEFloatRn :: RoundingMode -> Maybe Int -> Double -> ShowS #-}

-- |
-- >>> showFFloatRn TowardNearest (Just 0) 0 ""
-- "0"
-- >>> showFFloatRn TowardNearest Nothing 0 ""
-- "0.0"
-- >>> showFFloatRn TowardNearest Nothing (-0) ""
-- "-0.0"
showFFloatRn :: RealFloat a => RoundingMode -> Maybe Int -> a -> ShowS
showFFloatRn rn mprec x
  | isNaN x = showString "NaN"
  | x < 0 || isNegativeZero x = showChar '-' . showFFloatRn (oppositeRoundingMode rn) mprec (-x)
  | isInfinite x = showString "Infinity"
  | otherwise = case mprec of
                  Nothing -> let (xs,e) = binaryFloatToDecimalDigits x
                                 l = length xs
                             in if e >= l
                                then if null xs
                                     then showString "0.0"
                                     else showString (map intToDigit xs ++ replicate (e - l) '0' ++ ".0")
                                else if e >= 0 -- 0 <= e < l
                                     then if l == e -- null zs
                                          then showString (map intToDigit xs ++ ".0")
                                          else let (ys,zs) = splitAt (l - e) xs
                                                   ys' | null ys = [0]
                                                       | otherwise = ys
                                               in showString (map intToDigit ys' ++ "." ++ map intToDigit zs)
                                     else -- e < 0
                                       showString ("0." ++ replicate (-e) '0' ++ map intToDigit xs)
                  Just prec -> let prec' = max prec 0
                                   xs = binaryFloatToFixedDecimalDigitsRn rn prec' x
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
{-# SPECIALIZE showFFloatRn :: RoundingMode -> Maybe Int -> Double -> ShowS #-}

showGFloatRn :: RealFloat a => RoundingMode -> Maybe Int -> a -> ShowS
showGFloatRn rn mprec x | x == 0 || (0.1 <= abs x && abs x < 10^7) = showFFloatRn rn mprec x -- Note that 1%10 < toRational (0.1 :: Double)
                        | otherwise = showEFloatRn rn mprec x
{-# SPECIALIZE showGFloatRn :: RoundingMode -> Maybe Int -> Double -> ShowS #-}

{-
showFFloatAltRn :: RoundingMode -> Maybe Int -> Double -> ShowS
showGFloatAltRn :: RoundingMode -> Maybe Int -> Double -> ShowS
-- showFloat :: RoundingMode -> Double -> ShowS
-}
