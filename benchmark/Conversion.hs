{-# LANGUAGE DataKinds #-}
{-# LANGUAGE HexFloatLiterals #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Conversion (benchmark) where
import           Data.Bits
import           Data.Int
import           Data.Ratio
import           Data.Word
import           Gauge.Benchmark
import           Numeric.Rounded.Hardware
import           Numeric.Rounded.Hardware.Class
import           Numeric.Rounded.Hardware.Interval

word64ToDouble :: RoundingMode -> Word64 -> Double
word64ToDouble ToNearest x
  | x >= 0xFFFF_FFFF_FFFF_FC00 = 0x1p64
  | otherwise = let z = countLeadingZeros x
                    y = if x .&. (0x0000_0000_0000_0800 `unsafeShiftR` z) == 0
                        then x + (0x0000_0000_0000_03FF `unsafeShiftR` z)
                        else x + (0x0000_0000_0000_0400 `unsafeShiftR` z)
                in fromIntegral (y .&. (0xFFFF_FFFF_FFFF_F800 `unsafeShiftR` z))
word64ToDouble TowardInf x
  | x >= 0xFFFF_FFFF_FFFF_F800 = 0x1p64
  | otherwise = let z = countLeadingZeros x
                    y = x + (0x0000_0000_0000_07FF `unsafeShiftR` z)
                in fromIntegral (y .&. (0xFFFF_FFFF_FFFF_F800 `unsafeShiftR` z))
word64ToDouble TowardNegInf x = let z = countLeadingZeros x
                                in fromIntegral (x .&. (0xFFFF_FFFF_FFFF_F800 `unsafeShiftR` z))
word64ToDouble TowardZero x = let z = countLeadingZeros x
                              in fromIntegral (x .&. (0xFFFF_FFFF_FFFF_F800 `unsafeShiftR` z))

int64ToDouble :: RoundingMode -> Int64 -> Double
int64ToDouble r x | x >= 0 = word64ToDouble r (fromIntegral x)
                  | r == TowardInf = - word64ToDouble TowardNegInf (fromIntegral (-x))
                  | r == TowardNegInf = - word64ToDouble TowardInf (fromIntegral (-x))
                  | otherwise = - word64ToDouble r (fromIntegral (-x))

benchmark :: Benchmark
benchmark = bgroup "Conversion"
  [ let smallInteger = -2^50+2^13+127 :: Integer
        mediumInteger = -2^60 + 42 * 2^53 - 137 * 2^24 + 3 :: Integer
        largeInteger = -2^100-37*2^80+2^13+127 :: Integer
    in bgroup "fromInteger"
       [ bench "Double/small" $ nf (fromInteger :: Integer -> Double) smallInteger
       , bench "Double/medium" $ nf (fromInteger :: Integer -> Double) mediumInteger
       , bench "Double/large" $ nf (fromInteger :: Integer -> Double) largeInteger
       , bench "RoundedDouble/ToNearest/small" $ nf (fromInteger :: Integer -> Rounded 'ToNearest Double) smallInteger
       , bench "RoundedDouble/ToNearest/medium" $ nf (fromInteger :: Integer -> Rounded 'ToNearest Double) mediumInteger
       , bench "RoundedDouble/ToNearest/large" $ nf (fromInteger :: Integer -> Rounded 'ToNearest Double) largeInteger
       , bench "RoundedDouble/TowardInf/small" $ nf (fromInteger :: Integer -> Rounded 'TowardInf Double) smallInteger
       , bench "RoundedDouble/TowardInf/medium" $ nf (fromInteger :: Integer -> Rounded 'TowardInf Double) mediumInteger
       , bench "RoundedDouble/TowardInf/large" $ nf (fromInteger :: Integer -> Rounded 'TowardInf Double) largeInteger
       , bench "roundedFromInteger/Double/ToNearest/small" $ nf (roundedFromInteger ToNearest :: Integer -> Double) smallInteger
       , bench "roundedFromInteger/Double/ToNearest/medium" $ nf (roundedFromInteger ToNearest :: Integer -> Double) mediumInteger
       , bench "roundedFromInteger/Double/ToNearest/large" $ nf (roundedFromInteger ToNearest :: Integer -> Double) largeInteger
       , bench "roundedFromInteger/Double/TowardInf/small" $ nf (roundedFromInteger TowardInf :: Integer -> Double) smallInteger
       , bench "roundedFromInteger/Double/TowardInf/medium" $ nf (roundedFromInteger TowardInf :: Integer -> Double) mediumInteger
       , bench "roundedFromInteger/Double/TowardInf/large" $ nf (roundedFromInteger TowardInf :: Integer -> Double) largeInteger
       , bench "IntervalDouble/small" $ nf (fromInteger :: Integer -> Interval Double) smallInteger
       , bench "IntervalDouble/medium" $ nf (fromInteger :: Integer -> Interval Double) mediumInteger
       , bench "IntervalDouble/large" $ nf (fromInteger :: Integer -> Interval Double) largeInteger
       ]
  , let smallInteger = -2^50+2^13+127 :: Int64
        mediumInteger = -2^60 + 42 * 2^53 - 137 * 2^24 + 3 :: Int64
    in bgroup "fromIntegral/Int64"
       [ bench "Double/small" $ nf (fromIntegral :: Int64 -> Double) smallInteger
       , bench "Double/medium" $ nf (fromIntegral :: Int64 -> Double) mediumInteger
       , bench "RoundedDouble/ToNearest/small" $ nf (fromIntegral :: Int64 -> Rounded 'ToNearest Double) smallInteger
       , bench "RoundedDouble/ToNearest/medium" $ nf (fromIntegral :: Int64 -> Rounded 'ToNearest Double) mediumInteger
       , bench "RoundedDouble/TowardInf/small" $ nf (fromIntegral :: Int64 -> Rounded 'TowardInf Double) smallInteger
       , bench "RoundedDouble/TowardInf/medium" $ nf (fromIntegral :: Int64 -> Rounded 'TowardInf Double) mediumInteger
       , bench "roundedFromInteger/Double/ToNearest/small" $ nf (roundedFromInteger ToNearest . fromIntegral :: Int64 -> Double) smallInteger
       , bench "roundedFromInteger/Double/ToNearest/medium" $ nf (roundedFromInteger ToNearest . fromIntegral :: Int64 -> Double) mediumInteger
       , bench "roundedFromInteger/Double/TowardInf/small" $ nf (roundedFromInteger TowardInf . fromIntegral :: Int64 -> Double) smallInteger
       , bench "roundedFromInteger/Double/TowardInf/medium" $ nf (roundedFromInteger TowardInf . fromIntegral :: Int64 -> Double) mediumInteger
       , bench "int64ToDouble/Double/ToNearest/small" $ nf (int64ToDouble ToNearest :: Int64 -> Double) smallInteger
       , bench "int64ToDouble/Double/ToNearest/medium" $ nf (int64ToDouble ToNearest :: Int64 -> Double) mediumInteger
       , bench "int64ToDouble/Double/TowardInf/small" $ nf (int64ToDouble TowardInf :: Int64 -> Double) smallInteger
       , bench "int64ToDouble/Double/TowardInf/medium" $ nf (int64ToDouble TowardInf :: Int64 -> Double) mediumInteger
       ]
  , let pi' = 3.14159265358979323846264338327950 :: Rational
        smallRational = 22 % 7 :: Rational
        largeRational = 78326489123342523452342137498719847192 % 348912374981749170413424213275017 :: Rational
    in bgroup "fromRational"
       [ bench "Double/decimal" $ nf (fromRational :: Rational -> Double) pi'
       , bench "Double/small" $ nf (fromRational :: Rational -> Double) smallRational
       , bench "Double/large" $ nf (fromRational :: Rational -> Double) largeRational
       , bench "RoundedDouble/ToNearest/decimal" $ nf (fromRational :: Rational -> Rounded 'ToNearest Double) pi'
       , bench "RoundedDouble/ToNearest/small" $ nf (fromRational :: Rational -> Rounded 'ToNearest Double) smallRational
       , bench "RoundedDouble/ToNearest/large" $ nf (fromRational :: Rational -> Rounded 'ToNearest Double) largeRational
       , bench "RoundedDouble/TowardInf/decimal" $ nf (fromRational :: Rational -> Rounded 'TowardInf Double) pi'
       , bench "RoundedDouble/TowardInf/small" $ nf (fromRational :: Rational -> Rounded 'TowardInf Double) smallRational
       , bench "RoundedDouble/TowardInf/large" $ nf (fromRational :: Rational -> Rounded 'TowardInf Double) largeRational
       , bench "IntervalDouble/decimal" $ nf (fromRational :: Rational -> Interval Double) pi'
       , bench "IntervalDouble/small" $ nf (fromRational :: Rational -> Interval Double) smallRational
       , bench "IntervalDouble/large" $ nf (fromRational :: Rational -> Interval Double) largeRational
       ]
    ]
