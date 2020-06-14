{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuantifiedConstraints #-}
module IGA (benchmark) where
import           Control.Monad
import           Control.Monad.ST
import           Data.Array.IArray
import           Data.Array.MArray
import           Data.Array.ST (STArray, STUArray)
import           Data.Array.Unboxed (UArray)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import           Gauge.Benchmark
import           Numeric.Rounded.Hardware.Interval
import qualified Numeric.Rounded.Hardware.Interval.NonEmpty as NE

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

benchmark :: Benchmark
benchmark = bgroup "Interval Gaussian Elimination"
  [ let arr :: Fractional a => Array (Int,Int) a
        arr = listArray ((0,0),(4,4))
              [2,4,1,3,8
              ,-4,7,3.1,0,7
              ,9,7,54,1,0,1
              ,0,5,8,1e-10,7
              ,8,6,4,8,0
              ]
        vec :: Fractional a => V.Vector a
        vec = V.fromList [1,0,0,0,0]
    in bgroup "boxed"
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
    in bgroup "unboxed"
       [ bench "non-interval" $ nf (uncurry intervalGaussianEliminationU) (arr, vec :: VU.Vector Double)
       , bench "naive" $ nf (uncurry intervalGaussianEliminationU) (arr, vec :: VU.Vector (Interval Double))
       , bench "non-empty" $ nf (uncurry intervalGaussianEliminationU) (arr, vec :: VU.Vector (NE.Interval Double))
       ]
  ]
