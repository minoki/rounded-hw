{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE HexFloatLiterals #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
import           Control.Monad
import           Control.Monad.ST
import           Data.Array (Array)
import           Data.Array.IArray (IArray)
import           Data.Array.ST
import           Data.Array.Unboxed
import           Data.Ratio
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import           Gauge.Main
import           Numeric
import           Numeric.Rounded.Hardware.Internal
import           Numeric.Rounded.Hardware.Interval
import qualified Numeric.Rounded.Hardware.Vector.Unboxed as RVU

thawST :: (Ix i, IArray a e) => a i e -> ST s (STArray s i e)
thawST = thaw

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

intervalGaussianEliminationU :: (Fractional a, IArray UArray a, VU.Unbox a) => UArray (Int,Int) a -> VU.Vector a -> VU.Vector a
intervalGaussianEliminationU a b
  | not (i0 == 0 && j0 == 0 && iN == n - 1 && jN == n - 1) = error "invalid size"
  | otherwise = VU.create $ do
      a' <- thawST a
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

class Fractional a => CNextAfter a where
  c_nextafter :: a -> a -> a

instance CNextAfter Double where c_nextafter = c_nextafter_double
instance CNextAfter Float where c_nextafter = c_nextafter_float

c_nextUp, c_nextDown :: (RealFloat a, CNextAfter a) => a -> a
c_nextUp x = c_nextafter x (1/0)
c_nextDown x = c_nextafter x (-1/0)

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
         , bench "RoundedDouble/TowardNearest/small" $ nf (fromInteger :: Integer -> Rounded 'TowardNearest Double) smallInteger
         , bench "RoundedDouble/TowardNearest/medium" $ nf (fromInteger :: Integer -> Rounded 'TowardNearest Double) mediumInteger
         , bench "RoundedDouble/TowardNearest/large" $ nf (fromInteger :: Integer -> Rounded 'TowardNearest Double) largeInteger
         , bench "RoundedDouble/TowardInf/small" $ nf (fromInteger :: Integer -> Rounded 'TowardInf Double) smallInteger
         , bench "RoundedDouble/TowardInf/medium" $ nf (fromInteger :: Integer -> Rounded 'TowardInf Double) mediumInteger
         , bench "RoundedDouble/TowardInf/large" $ nf (fromInteger :: Integer -> Rounded 'TowardInf Double) largeInteger
         , bench "IntervalDouble/small" $ nf (fromInteger :: Integer -> Interval Double) smallInteger
         , bench "IntervalDouble/medium" $ nf (fromInteger :: Integer -> Interval Double) mediumInteger
         , bench "IntervalDouble/large" $ nf (fromInteger :: Integer -> Interval Double) largeInteger
         ]
    , let pi' = 3.14159265358979323846264338327950 :: Rational
          smallRational = 22 % 7 :: Rational
          largeRational = 78326489123342523452342137498719847192 % 348912374981749170413424213275017 :: Rational
      in bgroup "fromRational"
         [ bench "Double/decimal" $ nf (fromRational :: Rational -> Double) pi'
         , bench "Double/small" $ nf (fromRational :: Rational -> Double) smallRational
         , bench "Double/large" $ nf (fromRational :: Rational -> Double) largeRational
         , bench "RoundedDouble/TowardNearest/decimal" $ nf (fromRational :: Rational -> Rounded 'TowardNearest Double) pi'
         , bench "RoundedDouble/TowardNearest/small" $ nf (fromRational :: Rational -> Rounded 'TowardNearest Double) smallRational
         , bench "RoundedDouble/TowardNearest/large" $ nf (fromRational :: Rational -> Rounded 'TowardNearest Double) largeRational
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
    , let vec :: V.Vector (Interval Double)
          vec = V.generate 100000 $ \i -> fromRational (1 % (1 + fromIntegral i))
      in bgroup "interval sum"
         [ bench "naive" $ nf V.sum vec
         , bench "naive 2" $ nf (V.foldl' (+) 0) vec
         ]
    , bgroup "nextUp"
      [ let cases = [0,1,0x1.ffff_ffff_ffff_fp200] :: [Double]
        in bgroup "Double"
           [ bgroup "C"
             [ bench (showHFloat x "") $ nf c_nextUp x | x <- cases ]
           , bgroup "Haskell"
             [ bench (showHFloat x "") $ nf nextUp x | x <- cases ]
           ]
      , let cases = [0,1,0x1.fffffep100] :: [Float]
        in bgroup "Float"
           [ bgroup "C"
             [ bench (showHFloat x "") $ nf c_nextUp x | x <- cases ]
           , bgroup "Haskell"
             [ bench (showHFloat x "") $ nf nextUp x | x <- cases ]
           ]
      ]
    , bgroup "nextDown"
      [ let cases = [0,1,0x1.ffff_ffff_ffff_fp200] :: [Double]
        in bgroup "Double"
           [ bgroup "C"
             [ bench (showHFloat x "") $ nf c_nextDown x | x <- cases ]
           , bgroup "Haskell"
             [ bench (showHFloat x "") $ nf nextDown x | x <- cases ]
           ]
      , let cases = [0,1,0x1.fffffep100] :: [Float]
        in bgroup "Float"
           [ bgroup "C"
             [ bench (showHFloat x "") $ nf c_nextDown x | x <- cases ]
           , bgroup "Haskell"
             [ bench (showHFloat x "") $ nf nextDown x | x <- cases ]
           ]
      ]
    ]
