module FloatUtilSpec where
import           Numeric.Rounded.Hardware.Internal
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck
import           Util (sameFloatP, variousFloats)

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

c_nextUp, c_nextDown, c_nextTowardZero :: (RealFloat a, CFloat a) => a -> a
c_nextUp x = c_nextafter x (1/0)
c_nextDown x = c_nextafter x (-1/0)
c_nextTowardZero x | isNegativeZero x = x
                   | otherwise = c_nextafter x 0

prop_nextUp_match :: (RealFloat a, CFloat a, Show a) => a -> Property
prop_nextUp_match x = nextUp x `sameFloatP` c_nextUp x

prop_nextDown_match :: (RealFloat a, CFloat a, Show a) => a -> Property
prop_nextDown_match x = nextDown x `sameFloatP` c_nextDown x

prop_nextTowardZero_match :: (RealFloat a, CFloat a, Show a) => a -> Property
prop_nextTowardZero_match x = nextTowardZero x `sameFloatP` c_nextTowardZero x

prop_fma_match :: (RealFloat a, CFloat a, Show a) => a -> a -> a -> Property
prop_fma_match x y z = fusedMultiplyAdd x y z `sameFloatP` c_fma x y z

isPositiveZero :: RealFloat a => a -> Bool
isPositiveZero x = x == 0 && not (isNegativeZero x)

prop_nextUp_nextDown :: (RealFloat a, Show a) => a -> Property
prop_nextUp_nextDown x = x /= (-1/0) ==>
  let x' = nextUp (nextDown x)
  in x' `sameFloatP` x .||. (isPositiveZero x .&&. isNegativeZero x')

prop_nextDown_nextUp :: (RealFloat a, Show a) => a -> Property
prop_nextDown_nextUp x = x /= (1/0) ==>
  let x' = nextDown (nextUp x)
  in x' `sameFloatP` x .||. (isNegativeZero x .&&. isPositiveZero x')

spec :: Spec
spec = do
  describe "Double" $ do
    prop "nextUp vs C nextafter" $ forAll variousFloats (prop_nextUp_match :: Double -> Property)
    prop "nextDown vs C nextafter" $ forAll variousFloats (prop_nextDown_match :: Double -> Property)
    prop "nextTowardZero vs C nextafter" $ forAll variousFloats (prop_nextTowardZero_match :: Double -> Property)
    prop "nextUp . nextDown == id (unless -inf)" $ forAll variousFloats (prop_nextUp_nextDown :: Double -> Property)
    prop "nextDown . nextUp == id (unless inf)" $ forAll variousFloats (prop_nextDown_nextUp :: Double -> Property)
    prop "fusedMultiplyAdd vs C fma" $ forAll variousFloats (prop_fma_match :: Double -> Double -> Double -> Property)
  describe "Float" $ do
    prop "nextUp vs C nextafter" $ forAll variousFloats (prop_nextUp_match :: Float -> Property)
    prop "nextDown vs C nextafter" $ forAll variousFloats (prop_nextDown_match :: Float -> Property)
    prop "nextTowardZero vs C nextafter" $ forAll variousFloats (prop_nextTowardZero_match :: Float -> Property)
    prop "nextUp . nextDown == id (unless -inf)" $ forAll variousFloats (prop_nextUp_nextDown :: Float -> Property)
    prop "nextDown . nextUp == id (unless inf)" $ forAll variousFloats (prop_nextDown_nextUp :: Float -> Property)
    prop "fusedMultiplyAdd vs C fma" $ forAll variousFloats (prop_fma_match :: Float -> Float -> Float -> Property)
