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

class Fractional a => CNextAfter a where
  c_nextafter :: a -> a -> a

instance CNextAfter Double where c_nextafter = c_nextafter_double
instance CNextAfter Float where c_nextafter = c_nextafter_float

c_nextUp, c_nextDown, c_nextTowardZero :: (RealFloat a, CNextAfter a) => a -> a
c_nextUp x = c_nextafter x (1/0)
c_nextDown x = c_nextafter x (-1/0)
c_nextTowardZero x | isNegativeZero x = x
                   | otherwise = c_nextafter x 0

prop_nextUp_match :: (RealFloat a, CNextAfter a, Show a) => a -> Property
prop_nextUp_match x = nextUp x `sameFloatP` c_nextUp x

prop_nextDown_match :: (RealFloat a, CNextAfter a, Show a) => a -> Property
prop_nextDown_match x = nextDown x `sameFloatP` c_nextDown x

prop_nextTowardZero_match :: (RealFloat a, CNextAfter a, Show a) => a -> Property
prop_nextTowardZero_match x = nextTowardZero x `sameFloatP` c_nextTowardZero x

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
  describe "Float" $ do
    prop "nextUp vs C nextafter" $ forAll variousFloats (prop_nextUp_match :: Float -> Property)
    prop "nextDown vs C nextafter" $ forAll variousFloats (prop_nextDown_match :: Float -> Property)
    prop "nextTowardZero vs C nextafter" $ forAll variousFloats (prop_nextTowardZero_match :: Float -> Property)
    prop "nextUp . nextDown == id (unless -inf)" $ forAll variousFloats (prop_nextUp_nextDown :: Float -> Property)
    prop "nextDown . nextUp == id (unless inf)" $ forAll variousFloats (prop_nextDown_nextUp :: Float -> Property)
