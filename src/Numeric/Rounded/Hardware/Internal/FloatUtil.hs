{-# LANGUAGE NumericUnderscores #-}
module Numeric.Rounded.Hardware.Internal.FloatUtil
  ( nextUp
  , nextDown
  , nextTowardZero
  , minPositive_ieee
  , maxFinite_ieee
  , distanceUlp
  ) where
import           Data.Bits
import           Data.Ratio
import           GHC.Float (castDoubleToWord64, castFloatToWord32,
                            castWord32ToFloat, castWord64ToDouble)

-- $setup
-- >>> :set -XHexFloatLiterals -XNumericUnderscores

-- |
-- prop> (minPositive_ieee :: Double) == 0x1p-1074
-- prop> (minPositive_ieee :: Float) == 0x1p-149
minPositive_ieee :: RealFloat a => a
minPositive_ieee = let d = floatDigits x
                       (expMin,_expMax) = floatRange x
                       x = encodeFloat 1 (expMin - d)
                   in x

-- |
-- prop> (maxFinite_ieee :: Double) == 0x1.ffff_ffff_ffff_fp+1023
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
-- prop> nextUp 1 == (0x1.0000_0000_0000_1p0 :: Double)
-- prop> nextUp 1 == (0x1.000002p0 :: Float)
-- prop> nextUp (1/0) == (1/0 :: Double)
-- prop> nextUp (-1/0) == (- maxFinite_ieee :: Double)
-- prop> nextUp 0 == (0x1p-1074 :: Double)
-- prop> nextUp (-0) == (0x1p-1074 :: Double)
-- prop> nextUp (-0x1p-1074) == (-0 :: Double)
-- prop> isNegativeZero (nextUp (-0x1p-1074) :: Double)
nextUp :: RealFloat a => a -> a
nextUp x | not (isIEEE x) = error "non-IEEE numbers are not supported"
         | floatRadix x /= 2 = error "non-binary types are not supported "
         | isNaN x || (isInfinite x && x > 0) = x -- NaN or positive infinity
         | x >= 0 = nextUp_ieee_positive x
         | otherwise = - nextDown_ieee_positive (- x)
{-# INLINE [1] nextUp #-}

-- |
-- prop> nextDown 1 == (0x1.ffff_ffff_ffff_fp-1 :: Double)
-- prop> nextDown 1 == (0x1.fffffep-1 :: Float)
-- prop> nextDown (1/0) == (maxFinite_ieee :: Double)
-- prop> nextDown (-1/0) == (-1/0 :: Double)
-- prop> nextDown 0 == (-0x1p-1074 :: Double)
-- prop> nextDown (-0) == (-0x1p-1074 :: Double)
-- prop> nextDown 0x1p-1074 == (0 :: Double)
nextDown :: RealFloat a => a -> a
nextDown x | not (isIEEE x) = error "non-IEEE numbers are not supported"
           | floatRadix x /= 2 = error "non-binary types are not supported "
           | isNaN x || (isInfinite x && x < 0) = x -- NaN or negative infinity
           | x >= 0 = nextDown_ieee_positive x
           | otherwise = - nextUp_ieee_positive (- x)
{-# INLINE [1] nextDown #-}

-- |
-- prop> nextTowardZero 1 == (0x1.ffff_ffff_ffff_fp-1 :: Double)
-- prop> nextTowardZero 1 == (0x1.fffffep-1 :: Float)
-- prop> nextTowardZero (1/0) == (maxFinite_ieee :: Double)
-- prop> nextTowardZero (-1/0) == (-maxFinite_ieee :: Double)
-- prop> nextTowardZero 0 == (0 :: Double)
-- prop> isNegativeZero (nextTowardZero (-0 :: Double))
-- prop> nextTowardZero 0x1p-1074 == (0 :: Double)
nextTowardZero :: RealFloat a => a -> a
nextTowardZero x | not (isIEEE x) = error "non-IEEE numbers are not supported"
                 | floatRadix x /= 2 = error "non-binary types are not supported "
                 | isNaN x || x == 0 = x -- NaN or zero
                 | x >= 0 = nextDown_ieee_positive x
                 | otherwise = - nextDown_ieee_positive (- x)
{-# INLINE [1] nextTowardZero #-}

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
{-# INLINE nextUp_ieee_positive #-}

nextDown_ieee_positive :: RealFloat a => a -> a
nextDown_ieee_positive x
  | isNaN x || x < 0 = error "nextDown_ieee_positive"
  | isInfinite x = encodeFloat ((1 `unsafeShiftL` d) - 1) (expMax - d) -- max finite
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
                        else encodeFloat m1 e
                   else -- subnormal
                     let m' = m `shiftR` (expMin - d - e)
                     in encodeFloat (m' - 1) (expMin - d)
  where
    d, expMin :: Int
    d = floatDigits x -- 53 for Double
    (expMin,expMax) = floatRange x -- (-1021,1024) for Double
{-# INLINE nextDown_ieee_positive #-}

{-# RULES
"nextUp/Float" [~1] nextUp = nextUpFloat
"nextUp/Double" [~1] nextUp = nextUpDouble
"nextDown/Float" [~1] nextDown = nextDownFloat
"nextDown/Double" [~1] nextDown = nextDownDouble
"nextTowardZero/Float" [~1] nextTowardZero = nextTowardZeroFloat
"nextTowardZero/Double" [~1] nextTowardZero = nextTowardZeroDouble
  #-}

-- |
-- prop> nextUpFloat 1 == 0x1.000002p0
-- prop> nextUpFloat (1/0) == 1/0
-- prop> nextUpFloat (-1/0) == - maxFinite_ieee
-- prop> nextUpFloat 0 == 0x1p-149
-- prop> nextUpFloat (-0) == 0x1p-149
-- prop> isNegativeZero (nextUpFloat (-0x1p-149))
nextUpFloat :: Float -> Float
nextUpFloat x
  | not (isIEEE x) || floatRadix x /= 2 || d /= 24 || expMin /= -125 || expMax /= 128 = error "rounded-hw assumes Float is IEEE binary32"
  | isNaN x = x -- NaN -> itself
  | isNegativeZero x = encodeFloat 1 (expMin - d) -- -0 -> min positive
  | x < 0 = castWord32ToFloat (castFloatToWord32 x - 1) -- negative
  | otherwise = case castFloatToWord32 x of
                  0x7f80_0000 -> x -- positive infinity -> itself
                  w           -> castWord32ToFloat (w + 1) -- positive
  where
    d, expMin :: Int
    d = floatDigits x -- 53 for Double
    (expMin,expMax) = floatRange x -- (-1021,1024) for Double
    -- Note: castFloatToWord32 is buggy on GHC <= 8.8 on x86_64, so we can't use it to test for NaN or negative number
    --   https://gitlab.haskell.org/ghc/ghc/issues/16617

-- |
-- prop> nextUpDouble 1 == 0x1.0000_0000_0000_1p0
-- prop> nextUpDouble (1/0) == 1/0
-- prop> nextUpDouble (-1/0) == - maxFinite_ieee
-- prop> nextUpDouble 0 == 0x1p-1074
-- prop> nextUpDouble (-0) == 0x1p-1074
-- prop> isNegativeZero (nextUpDouble (-0x1p-1074))
nextUpDouble :: Double -> Double
nextUpDouble x
  | not (isIEEE x) || floatRadix x /= 2 || d /= 53 || expMin /= -1021 || expMax /= 1024 = error "rounded-hw assumes Double is IEEE binary64"
  | otherwise = case castDoubleToWord64 x of
                  w | w .&. 0x7ff0_0000_0000_0000 == 0x7ff0_0000_0000_0000
                    , w /= 0xfff0_0000_0000_0000 -> x -- NaN or positive infinity -> itself
                  0x8000_0000_0000_0000 -> encodeFloat 1 (expMin - d) -- -0 -> min positive
                  w | testBit w 63 -> castWord64ToDouble (w - 1) -- negative
                    | otherwise -> castWord64ToDouble (w + 1) -- positive
  where
    d, expMin :: Int
    d = floatDigits x -- 53 for Double
    (expMin,expMax) = floatRange x -- (-1021,1024) for Double

-- |
-- prop> nextDownFloat 1 == 0x1.fffffep-1
-- prop> nextDownFloat (1/0) == maxFinite_ieee
-- prop> nextDownFloat (-1/0) == -1/0
-- prop> nextDownFloat 0 == -0x1p-149
-- prop> nextDownFloat (-0) == -0x1p-149
-- prop> nextDownFloat 0x1p-149 == 0
nextDownFloat :: Float -> Float
nextDownFloat x
  | not (isIEEE x) || floatRadix x /= 2 || d /= 24 || expMin /= -125 || expMax /= 128 = error "rounded-hw assumes Float is IEEE binary32"
  | isNaN x || (isInfinite x && x < 0) = x -- NaN or negative infinity -> itself
  | isNegativeZero x || x < 0 = castWord32ToFloat (castFloatToWord32 x + 1) -- negative
  | x == 0 = encodeFloat (-1) (expMin - d) -- +0 -> max negative
  | otherwise = castWord32ToFloat (castFloatToWord32 x - 1) -- positive
  where
    d, expMin :: Int
    d = floatDigits x -- 53 for Double
    (expMin,expMax) = floatRange x -- (-1021,1024) for Double
    -- Note: castFloatToWord32 is buggy on GHC <= 8.8 on x86_64, so we can't use it to test for NaN or negative number
    --   https://gitlab.haskell.org/ghc/ghc/issues/16617

-- |
-- prop> nextDownDouble 1 == 0x1.ffff_ffff_ffff_fp-1
-- prop> nextDownDouble (1/0) == maxFinite_ieee
-- prop> nextDownDouble (-1/0) == -1/0
-- prop> nextDownDouble 0 == -0x1p-1074
-- prop> nextDownDouble (-0) == -0x1p-1074
-- prop> nextDownDouble 0x1p-1074 == 0
nextDownDouble :: Double -> Double
nextDownDouble x
  | not (isIEEE x) || floatRadix x /= 2 || d /= 53 || expMin /= -1021 || expMax /= 1024 = error "rounded-hw assumes Double is IEEE binary64"
  | otherwise = case castDoubleToWord64 x of
                  w | w .&. 0x7ff0_0000_0000_0000 == 0x7ff0_0000_0000_0000
                    , w /= 0x7ff0_0000_0000_0000 -> x -- NaN or negative infinity -> itself
                  0x0000_0000_0000_0000 -> encodeFloat (-1) (expMin - d) -- +0 -> max negative
                  w | testBit w 63 -> castWord64ToDouble (w + 1) -- negative
                    | otherwise -> castWord64ToDouble (w - 1) -- positive
  where
    d, expMin :: Int
    d = floatDigits x -- 53 for Double
    (expMin,expMax) = floatRange x -- (-1021,1024) for Double

-- |
-- prop> nextTowardZeroFloat 1 == 0x1.fffffep-1
-- prop> nextTowardZeroFloat (-1) == -0x1.fffffep-1
-- prop> nextTowardZeroFloat (1/0) == maxFinite_ieee
-- prop> nextTowardZeroFloat (-1/0) == -maxFinite_ieee
-- prop> nextTowardZeroFloat 0 == 0
-- prop> isNegativeZero (nextTowardZeroFloat (-0))
-- prop> nextTowardZeroFloat 0x1p-149 == 0
nextTowardZeroFloat :: Float -> Float
nextTowardZeroFloat x
  | not (isIEEE x) || floatRadix x /= 2 || d /= 24 || expMin /= -125 || expMax /= 128 = error "rounded-hw assumes Float is IEEE binary32"
  | isNaN x || x == 0 = x -- NaN or zero -> itself
  | otherwise = castWord32ToFloat (castFloatToWord32 x - 1) -- positive / negative
  where
    d, expMin :: Int
    d = floatDigits x -- 53 for Double
    (expMin,expMax) = floatRange x -- (-1021,1024) for Double
    -- Note: castFloatToWord32 is buggy on GHC <= 8.8 on x86_64, so we can't use it to test for NaN or negative number
    --   https://gitlab.haskell.org/ghc/ghc/issues/16617

-- |
-- prop> nextTowardZeroDouble 1 == 0x1.ffff_ffff_ffff_fp-1
-- prop> nextTowardZeroDouble (-1) == -0x1.ffff_ffff_ffff_fp-1
-- prop> nextTowardZeroDouble (1/0) == maxFinite_ieee
-- prop> nextTowardZeroDouble (-1/0) == -maxFinite_ieee
-- prop> nextTowardZeroDouble 0 == 0
-- prop> isNegativeZero (nextTowardZeroDouble (-0))
-- prop> nextTowardZeroDouble 0x1p-1074 == 0
nextTowardZeroDouble :: Double -> Double
nextTowardZeroDouble x
  | not (isIEEE x) || floatRadix x /= 2 || d /= 53 || expMin /= -1021 || expMax /= 1024 = error "rounded-hw assumes Double is IEEE binary64"
  | otherwise = case castDoubleToWord64 x of
                  w | w .&. 0x7ff0_0000_0000_0000 == 0x7ff0_0000_0000_0000
                    , w .&. 0x000f_ffff_ffff_ffff /= 0 -> x -- NaN -> itself
                  0x8000_0000_0000_0000 -> x -- -0 -> itself
                  0x0000_0000_0000_0000 -> x -- +0 -> itself
                  w -> castWord64ToDouble (w - 1) -- positive / negative
  where
    d, expMin :: Int
    d = floatDigits x -- 53 for Double
    (expMin,expMax) = floatRange x -- (-1021,1024) for Double

distanceUlp :: RealFloat a => a -> a -> Maybe Integer
distanceUlp x y
  | isInfinite x || isInfinite y || isNaN x || isNaN y = Nothing
  | otherwise = let m = min (abs x) (abs y)
                    m' = nextUp m
                    v = (toRational y - toRational x) / toRational (m' - m)
                in if denominator v == 1
                   then Just (abs (numerator v))
                   else error "distanceUlp"
