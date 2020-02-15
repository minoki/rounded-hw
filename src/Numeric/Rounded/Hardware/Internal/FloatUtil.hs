module Numeric.Rounded.Hardware.Internal.FloatUtil
  ( nextUp
  , nextDown
  , minPositive_ieee
  , maxFinite_ieee
  , distanceUlp
  ) where
import           Data.Ratio
import           Data.Bits

-- $setup
-- >>> :set -XHexFloatLiterals

-- |
-- prop> (minPositive_ieee :: Double) == 0x1p-1074
-- prop> (minPositive_ieee :: Float) == 0x1p-149
minPositive_ieee :: RealFloat a => a
minPositive_ieee = let d = floatDigits x
                       (expMin,_expMax) = floatRange x
                       x = encodeFloat 1 (expMin - d)
                   in x

-- |
-- prop> (maxFinite_ieee :: Double) == 0x1.fffffffffffffp+1023
-- prop> (maxFinite_ieee :: Float) == 0x1.fffffep+127
maxFinite_ieee :: RealFloat a => a
maxFinite_ieee = let d = floatDigits x
                     (_expMin,expMax) = floatRange x
                     r = floatRadix x
                     x = encodeFloat (r^d-1) (expMax - d)
                 in x
{-# SPECIALIZE maxFinite_ieee :: Double #-}
{-# SPECIALIZE maxFinite_ieee :: Float #-}

-- |
-- prop> nextUp 1 == (0x1.0000000000001p0 :: Double)
-- prop> nextUp 1 == (0x1.000002p0 :: Float)
-- prop> nextUp (1/0) == (1/0 :: Double)
-- prop> nextUp (-1/0) == (- maxFinite_ieee :: Double)
-- prop> nextUp 0 == (0x1p-1074 :: Double)
-- prop> isNegativeZero (nextUp (-0x1p-1074) :: Double)
nextUp :: RealFloat a => a -> a
nextUp x | not (isIEEE x) = error "non-IEEE numbers are not supported"
         | floatRadix x /= 2 = error "non-binary types are not supported "
         | isNaN x || (isInfinite x && x > 0) = x -- NaN or positive infinity
         | x >= 0 = nextUp_ieee_positive x
         | otherwise = - nextDown_ieee_positive (- x)
{-# SPECIALIZE nextUp :: Double -> Double, Float -> Float #-}

-- |
-- prop> nextDown 1 == (0x1.fffffffffffffp-1 :: Double)
-- prop> nextDown 1 == (0x1.fffffep-1 :: Float)
-- prop> nextDown (1/0) == (maxFinite_ieee :: Double)
-- prop> nextDown (-1/0) == (-1/0 :: Double)
-- prop> nextDown 0 == (-0x1p-1074 :: Double)
nextDown :: RealFloat a => a -> a
nextDown x | not (isIEEE x) = error "non-IEEE numbers are not supported"
           | floatRadix x /= 2 = error "non-binary types are not supported "
           | isNaN x || (isInfinite x && x < 0) = x -- NaN or negative infinity
           | x >= 0 = nextDown_ieee_positive x
           | otherwise = - nextUp_ieee_positive (- x)
{-# SPECIALIZE nextDown :: Double -> Double, Float -> Float #-}

nextUp_ieee_positive :: RealFloat a => a -> a
nextUp_ieee_positive x
  | isNaN x || x < 0 = error "nextUp_ieee_positive"
  | isInfinite x = x
  | x == 0 = encodeFloat 1 (expMin - d) -- min positive
  | otherwise = let m :: Integer
                    e :: Int
                    (m,e) = decodeFloat x
                    -- x = m * 2^e, 2^(d-1) <= m < 2^d
                    -- 2^expMin < x < 2^expMax
                    -- 2^(expMin-d): min positive
                    -- 2^(expMin - 1): min normal 0x1p-1022
                    -- expMin - d <= e <= expMax - d (-1074 .. 971)
                in if expMin - d <= e
                   then encodeFloat (m + 1) e -- normal
                   else let m' = m `shiftR` (expMin - d - e)
                        in encodeFloat (m' + 1) (expMin - d) -- subnormal
  where
    d, expMin :: Int
    d = floatDigits x -- 53 for Double
    (expMin,_expMax) = floatRange x -- (-1021,1024) for Double
{-# SPECIALIZE nextUp_ieee_positive :: Double -> Double, Float -> Float #-}

nextDown_ieee_positive :: RealFloat a => a -> a
nextDown_ieee_positive x
  | isNaN x || x < 0 = error "nextDown_ieee_positive"
  | isInfinite x = encodeFloat (2^d-1) (expMax - d) -- max finite
  | x == 0 = encodeFloat (-1) (expMin - d) -- max negative
  | otherwise = let m :: Integer
                    e :: Int
                    (m,e) = decodeFloat x
                    -- x = m * 2^e, 2^(d-1) <= m < 2^d
                    -- 2^expMin < x < 2^expMax
                    -- 2^(expMin-d): min positive
                    -- 2^(expMin - 1): min normal 0x1p-1022
                    -- expMin - d <= e <= expMax - d (-1074 .. 971)
                in if expMin - d <= e
                   then -- normal
                     let m1 = m - 1
                     in if m .&. m1 == 0
                        then encodeFloat (2 * m - 1) (e - 1)
                        else encodeFloat (m - 1) e
                   else -- subnormal
                     let m' = m `shiftR` (expMin - d - e)
                     in encodeFloat (m' - 1) (expMin - d)
  where
    d, expMin :: Int
    d = floatDigits x -- 53 for Double
    (expMin,expMax) = floatRange x -- (-1021,1024) for Double
{-# SPECIALIZE nextDown_ieee_positive :: Double -> Double, Float -> Float #-}

distanceUlp :: RealFloat a => a -> a -> Maybe Integer
distanceUlp x y
  | isInfinite x || isInfinite y || isNaN x || isNaN y = Nothing
  | otherwise = let m = min (abs x) (abs y)
                    m' = nextUp m
                    v = (toRational y - toRational x) / toRational (m' - m)
                in if denominator v == 1
                   then Just (abs (numerator v))
                   else error "distanceUlp"
