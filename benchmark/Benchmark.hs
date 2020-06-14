{-# LANGUAGE DataKinds #-}
{-# LANGUAGE HexFloatLiterals #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
import           Conversion
import           Data.Coerce
import           Data.Functor.Identity
import           Data.Int
import           Data.Proxy
import           Data.Ratio
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import           Gauge.Main
import           IGA
import           Numeric
import           Numeric.Rounded.Hardware.Internal
import           Numeric.Rounded.Hardware.Interval
import           Numeric.Rounded.Hardware.Interval.Class (makeInterval)
import qualified Numeric.Rounded.Hardware.Interval.NonEmpty as NE
import qualified Numeric.Rounded.Hardware.Vector.Unboxed as RVU

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

main :: IO ()
main =
  defaultMain
    [ Conversion.benchmark
    , IGA.benchmark
    , let vec :: VU.Vector Double
          vec = VU.generate 100000 $ \i -> fromRational (1 % fromIntegral (i+1))
          vec1, vec2 :: VU.Vector (Rounded 'TowardInf Double)
          vec1 = VU.drop 3 $ VU.take 58645 $ VU.map Rounded vec
          vec2 = VU.drop 1234 $ VU.take 78245 $ VU.map Rounded vec
          sqrt' :: forall r a. (Rounding r, RoundedSqrt a) => Rounded r a -> Rounded r a
          sqrt' (Rounded x) = Rounded (roundedSqrt r x)
            where r = rounding (Proxy :: Proxy r)
      in bgroup "Vector"
         [ bgroup "sum"
           [ bench "naive" $ nf VU.sum vec1
           , bench "C impl" $ nf RVU.sum vec1
           , bench "non-rounded" $ nf VU.sum (coerce vec1 :: VU.Vector Double)
           ]
         , bgroup "add"
           [ bench "naive" $ nf (uncurry (VU.zipWith (+))) (vec1, vec2)
           , bench "C impl" $ nf (uncurry RVU.zipWith_add) (vec1, vec2)
           , bench "non-rounded" $ nf (uncurry (VU.zipWith (+))) (coerce vec1 :: VU.Vector Double, coerce vec2)
           ]
         , bgroup "sub"
           [ bench "naive" $ nf (uncurry (VU.zipWith (-))) (vec1, vec2)
           , bench "C impl" $ nf (uncurry RVU.zipWith_sub) (vec1, vec2)
           , bench "non-rounded" $ nf (uncurry (VU.zipWith (-))) (coerce vec1 :: VU.Vector Double, coerce vec2)
           ]
         , bgroup "mul"
           [ bench "naive" $ nf (uncurry (VU.zipWith (*))) (vec1, vec2)
           , bench "C impl" $ nf (uncurry RVU.zipWith_mul) (vec1, vec2)
           , bench "non-rounded" $ nf (uncurry (VU.zipWith (*))) (coerce vec1 :: VU.Vector Double, coerce vec2)
           ]
         , bgroup "div"
           [ bench "naive" $ nf (uncurry (VU.zipWith (/))) (vec1, vec2)
           , bench "C impl" $ nf (uncurry RVU.zipWith_div) (vec1, vec2)
           , bench "non-rounded" $ nf (uncurry (VU.zipWith (/))) (coerce vec1 :: VU.Vector Double, coerce vec2)
           ]
         , bgroup "sqrt"
           [ bench "naive" $ nf (VU.map sqrt') vec1
           , bench "C impl" $ nf RVU.map_sqrt vec1
           , bench "non-rounded" $ nf (VU.map sqrt) (coerce vec1 :: VU.Vector Double)
           ]
         , bgroup "compound"
           [ bench "naive" $ nf (\(v1,v2) -> VU.zipWith (+) (VU.zipWith (*) v1 v2) (VU.map sqrt' v2)) (vec1, vec2)
           , bench "C impl" $ nf (\(v1,v2) -> RVU.zipWith_add (RVU.zipWith_mul v1 v2) (RVU.map_sqrt v2)) (vec1, vec2)
           , bench "non-rounded" $ nf (\(v1,v2) -> VU.zipWith (+) (VU.zipWith (*) v1 v2) (VU.map sqrt v2)) (coerce vec1 :: VU.Vector Double, coerce vec2)
           ]
         ]
    , let iv1, iv2 :: Interval Double
          iv1 = makeInterval (Rounded 1) (Rounded 2)
          iv2 = makeInterval (Rounded 15) (Rounded 18)
      in bgroup "Interval"
         [ bench "add" $ nf (uncurry (+)) (iv1, iv2)
         , bench "sub" $ nf (uncurry (-)) (iv1, iv2)
         , bench "mul" $ nf (uncurry (*)) (iv1, iv2)
         , bench "div" $ nf (uncurry (/)) (iv1, iv2)
         , bench "sqrt" $ nf sqrt iv1
         , bench "fromInteger" $ nf (fromInteger :: Integer -> Interval Double) (2^60 + 1)
         , bench "fromIntegral/Int64" $ nf (fromIntegral :: Int64 -> Interval Double) (2^60 + 1)
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
           , bench "non-fused" $ nf (\(x,y,z) -> x * y + z) arg
           ]
      , let arg = (1.0, 2.0, 3.0) :: (Float, Float, Float)
        in bgroup "Float"
           [ bench "C" $ nf (\(x,y,z) -> c_fma x y z) arg
           , bench "Haskell" $ nf (\(x,y,z) -> fusedMultiplyAdd x y z) arg
           , bench "Haskell (generic)" $ nf (\(x,y,z) -> fusedMultiplyAdd (Identity x) (Identity y) (Identity z)) arg
           , bench "Haskell (rounded)" $ nf (\(x,y,z) -> roundedFusedMultiplyAdd ToNearest x y z) arg
           , bench "non-fused" $ nf (\(x,y,z) -> x * y + z) arg
           ]
      ]
    ]
