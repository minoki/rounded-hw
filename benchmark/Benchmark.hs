{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE HexFloatLiterals #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
import           Control.Monad
import           Control.Monad.ST
import           Data.Array.IArray
import           Data.Array.MArray
import           Data.Array.ST (STArray, STUArray)
import           Data.Array.Unboxed (UArray)
import           Data.Bits
import           Data.Functor.Identity
import           Data.Int
import           Data.Ratio
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import           Data.Word
import           Gauge.Main
import           Numeric
import           Numeric.Rounded.Hardware.Internal
import           Numeric.Rounded.Hardware.Interval
import qualified Numeric.Rounded.Hardware.Interval.NonEmpty as NE
import qualified Numeric.Rounded.Hardware.Vector.Unboxed as RVU

thawST :: (Ix i, IArray a e) => a i e -> ST s (STArray s i e)
thawST = thaw

thawSTU :: (Ix i, IArray a e {-, MArray (STUArray s) e (ST s) -}) => a i e -> ST s (STArray s i e)
thawSTU = thaw
{-# INLINE thawSTU #-}

intervalGaussianElimination :: (Fractional a) => Array (Int,Int) a -> V.Vector a -> V.Vector a
intervalGaussianElimination a b
  | not (i0 == 0 && j0 == 0 && iN == n - 1 && jN == n - 1) = error "invalid size"
  | otherwise = V.create $ do
      a' <- thawST a
      b' <- V.thaw b

      -- elimination
      forM_ [0..n-2] $ \k -> do
        forM_ [k+1..n-1] $ \i -> do
          !t <- liftM2 (/) (readArray a' (i,k)) (readArray a' (k,k))
          forM_ [k+1..n-1] $ \j -> do
            a_ij <- readArray a' (i,j)
            a_kj <- readArray a' (k,j)
            writeArray a' (i,j) $! a_ij - t * a_kj
          b_k <- VM.read b' k
          modify' b' (subtract (t * b_k)) i

      -- backward substitution
      a_nn <- readArray a' (n-1,n-1)
      modify' b' (/ a_nn) (n-1)
      forM_ [n-2,n-3..0] $ \i -> do
        s <- sum <$> mapM (\j -> liftM2 (*) (readArray a' (i,j)) (VM.read b' j)) [i+1..n-1]
        a_ii <- readArray a' (i,i)
        modify' b' (\b_i -> (b_i - s) / a_ii) i
      return b'
        where
          ((i0,j0),(iN,jN)) = bounds a
          n = V.length b
          modify' vec f i = do
            x <- VM.read vec i
            VM.write vec i $! f x

{-# SPECIALIZE
  intervalGaussianEliminationU :: UArray (Int,Int) Double -> VU.Vector Double -> VU.Vector Double, UArray (Int,Int) (Interval Double) -> VU.Vector (Interval Double) -> VU.Vector (Interval Double)
                                , UArray (Int,Int) (NE.Interval Double) -> VU.Vector (NE.Interval Double) -> VU.Vector (NE.Interval Double)
  #-}
intervalGaussianEliminationU :: (Fractional a, IArray UArray a, forall s. MArray (STUArray s) a (ST s), VU.Unbox a) => UArray (Int,Int) a -> VU.Vector a -> VU.Vector a
intervalGaussianEliminationU a b
  | not (i0 == 0 && j0 == 0 && iN == n - 1 && jN == n - 1) = error "invalid size"
  | otherwise = VU.create $ do
      a' <- thawSTU a
      b' <- VU.thaw b

      -- elimination
      forM_ [0..n-2] $ \k -> do
        forM_ [k+1..n-1] $ \i -> do
          !t <- liftM2 (/) (readArray a' (i,k)) (readArray a' (k,k))
          forM_ [k+1..n-1] $ \j -> do
            a_ij <- readArray a' (i,j)
            a_kj <- readArray a' (k,j)
            writeArray a' (i,j) $! a_ij - t * a_kj
          b_k <- VUM.read b' k
          modify' b' (subtract (t * b_k)) i

      -- backward substitution
      a_nn <- readArray a' (n-1,n-1)
      modify' b' (/ a_nn) (n-1)
      forM_ [n-2,n-3..0] $ \i -> do
        s <- sum <$> mapM (\j -> liftM2 (*) (readArray a' (i,j)) (VUM.read b' j)) [i+1..n-1]
        a_ii <- readArray a' (i,i)
        modify' b' (\b_i -> (b_i - s) / a_ii) i
      return b'
        where
          ((i0,j0),(iN,jN)) = bounds a
          n = VU.length b
          modify' vec f i = do
            x <- VUM.read vec i
            VUM.write vec i $! f x

foreign import ccall unsafe "nextafter"
  c_nextafter_double :: Double -> Double -> Double
foreign import ccall unsafe "nextafterf"
  c_nextafter_float :: Float -> Float -> Float
foreign import ccall unsafe "fma"
  c_fma_double :: Double -> Double -> Double -> Double
foreign import ccall unsafe "fmaf"
  c_fma_float :: Float -> Float -> Float -> Float

class Fractional a => CFloat a where
  c_nextafter :: a -> a -> a
  c_fma :: a -> a -> a -> a

instance CFloat Double where
  c_nextafter = c_nextafter_double
  c_fma = c_fma_double

instance CFloat Float where
  c_nextafter = c_nextafter_float
  c_fma = c_fma_float

c_nextUp, c_nextDown :: (RealFloat a, CFloat a) => a -> a
c_nextUp x = c_nextafter x (1/0)
c_nextDown x = c_nextafter x (-1/0)

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

main :: IO ()
main =
  defaultMain
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
    , let arr :: Fractional a => Array (Int,Int) a
          arr = listArray ((0,0),(4,4))
                [2,4,1,3,8
                ,-4,7,3.1,0,7
                ,9,7,54,1,0,1
                ,0,5,8,1e-10,7
                ,8,6,4,8,0
                ]
          vec :: Fractional a => V.Vector a
          vec = V.fromList [1,0,0,0,0]
      in bgroup "(Interval) Gaussian Elimination"
         [ bench "non-interval" $ nf (uncurry intervalGaussianElimination) (arr, vec :: V.Vector Double)
         , bench "naive" $ nf (uncurry intervalGaussianElimination) (arr, vec :: V.Vector (Interval Double))
         , bench "non-empty" $ nf (uncurry intervalGaussianElimination) (arr, vec :: V.Vector (NE.Interval Double))
         ]
    , let arr :: (IArray UArray a, Fractional a) => UArray (Int,Int) a
          arr = listArray ((0,0),(4,4))
                [2,4,1,3,8
                ,-4,7,3.1,0,7
                ,9,7,54,1,0,1
                ,0,5,8,1e-10,7
                ,8,6,4,8,0
                ]
          vec :: (VU.Unbox a, Fractional a) => VU.Vector a
          vec = VU.fromList [1,0,0,0,0]
      in bgroup "(Interval) Gaussian Elimination, unboxed"
         [ bench "non-interval" $ nf (uncurry intervalGaussianEliminationU) (arr, vec :: VU.Vector Double)
         , bench "naive" $ nf (uncurry intervalGaussianEliminationU) (arr, vec :: VU.Vector (Interval Double))
         , bench "non-empty" $ nf (uncurry intervalGaussianEliminationU) (arr, vec :: VU.Vector (NE.Interval Double))
         ]
    , let vec :: VU.Vector Double
          vec = VU.generate 100000 $ \i -> fromRational (1 % fromIntegral (i+1))
          vec' :: VU.Vector (Rounded 'TowardInf Double)
          vec' = VU.drop 1234 $ VU.take 78245 $ VU.map Rounded vec
          vec'' :: VU.Vector (Rounded 'TowardNegInf Double)
          vec'' = VU.drop 1234 $ VU.take 78245 $ VU.map Rounded vec
      in bgroup "sum"
         [ bench "naive" $ nf VU.sum vec'
         , bench "C impl" $ nf RVU.sum vec'
         ]
    , let vec :: VU.Vector Double
          vec = VU.generate 100000 $ \i -> fromRational (1 % fromIntegral (i+1))
          vec1, vec2 :: VU.Vector (Rounded 'TowardInf Double)
          vec1 = VU.drop 3 $ VU.take 58645 $ VU.map Rounded vec
          vec2 = VU.drop 1234 $ VU.take 78245 $ VU.map Rounded vec
      in bgroup "vector"
         [ bgroup "add"
           [ bench "naive" $ nf (uncurry (VU.zipWith (+))) (vec1, vec2)
           , bench "C impl" $ nf (uncurry RVU.zipWith_add) (vec1, vec2)
           ]
         , bgroup "sub"
           [ bench "naive" $ nf (uncurry (VU.zipWith (-))) (vec1, vec2)
           , bench "C impl" $ nf (uncurry RVU.zipWith_sub) (vec1, vec2)
           ]
         , bgroup "mul"
           [ bench "naive" $ nf (uncurry (VU.zipWith (*))) (vec1, vec2)
           , bench "C impl" $ nf (uncurry RVU.zipWith_mul) (vec1, vec2)
           ]
         , bgroup "div"
           [ bench "naive" $ nf (uncurry (VU.zipWith (/))) (vec1, vec2)
           , bench "C impl" $ nf (uncurry RVU.zipWith_div) (vec1, vec2)
           ]
         ]
    , let vec :: V.Vector (Interval Double)
          vec = V.generate 100000 $ \i -> fromRational (1 % (1 + fromIntegral i))
      in bgroup "interval sum"
         [ bench "naive" $ nf V.sum vec
         , bench "naive 2" $ nf (V.foldl' (+) 0) vec
         ]
    , bgroup "interval elementary functions"
      [ bench "exp" $ nf exp (0.3 :: Interval Double)
      , bench "NE.exp" $ nf exp (0.3 :: NE.Interval Double)
      , bench "sin" $ nf sin (7.3 :: Interval Double)
      , bench "NE.sin" $ nf sin (7.3 :: NE.Interval Double)
      ]
    , bgroup "nextUp"
      [ let cases = [0,1,0x1.ffff_ffff_ffff_fp200] :: [Double]
        in bgroup "Double"
           [ bgroup "C"
             [ bench (showHFloat x "") $ nf c_nextUp x | x <- cases ]
           , bgroup "Haskell"
             [ bench (showHFloat x "") $ nf nextUp x | x <- cases ]
           , bgroup "Haskell (generic)"
             [ bench (showHFloat x "") $ nf nextUp (Identity x) | x <- cases ]
           ]
      , let cases = [0,1,0x1.fffffep100] :: [Float]
        in bgroup "Float"
           [ bgroup "C"
             [ bench (showHFloat x "") $ nf c_nextUp x | x <- cases ]
           , bgroup "Haskell"
             [ bench (showHFloat x "") $ nf nextUp x | x <- cases ]
           , bgroup "Haskell (generic)"
             [ bench (showHFloat x "") $ nf nextUp (Identity x) | x <- cases ]
           ]
      ]
    , bgroup "nextDown"
      [ let cases = [0,1,0x1.ffff_ffff_ffff_fp200] :: [Double]
        in bgroup "Double"
           [ bgroup "C"
             [ bench (showHFloat x "") $ nf c_nextDown x | x <- cases ]
           , bgroup "Haskell"
             [ bench (showHFloat x "") $ nf nextDown x | x <- cases ]
           , bgroup "Haskell (generic)"
             [ bench (showHFloat x "") $ nf nextDown (Identity x) | x <- cases ]
           ]
      , let cases = [0,1,0x1.fffffep100] :: [Float]
        in bgroup "Float"
           [ bgroup "C"
             [ bench (showHFloat x "") $ nf c_nextDown x | x <- cases ]
           , bgroup "Haskell"
             [ bench (showHFloat x "") $ nf nextDown x | x <- cases ]
           , bgroup "Haskell (generic)"
             [ bench (showHFloat x "") $ nf nextDown (Identity x) | x <- cases ]
           ]
      ]
    , bgroup "FMA"
      [ let arg = (1.0, 2.0, 3.0) :: (Double, Double, Double)
        in bgroup "Double"
           [ bench "C" $ nf (\(x,y,z) -> c_fma x y z) arg
           , bench "Haskell" $ nf (\(x,y,z) -> fusedMultiplyAdd x y z) arg
           , bench "Haskell (generic)" $ nf (\(x,y,z) -> fusedMultiplyAdd (Identity x) (Identity y) (Identity z)) arg
           , bench "Haskell (rounded)" $ nf (\(x,y,z) -> roundedFusedMultiplyAdd ToNearest x y z) arg
           ]
      , let arg = (1.0, 2.0, 3.0) :: (Float, Float, Float)
        in bgroup "Float"
           [ bench "C" $ nf (\(x,y,z) -> c_fma x y z) arg
           , bench "Haskell" $ nf (\(x,y,z) -> fusedMultiplyAdd x y z) arg
           , bench "Haskell (generic)" $ nf (\(x,y,z) -> fusedMultiplyAdd (Identity x) (Identity y) (Identity z)) arg
           , bench "Haskell (rounded)" $ nf (\(x,y,z) -> roundedFusedMultiplyAdd ToNearest x y z) arg
           ]
      ]
    ]
