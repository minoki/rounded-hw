{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module RoundedArithmeticSpec where
import           Data.Proxy
import           Numeric.Rounded.Hardware.Internal
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck

prop_roundedAdd :: (Ord a, Num a, RoundedRing a) => Proxy a -> a -> a -> Property
prop_roundedAdd _proxy x y =
  -- Assume neither x nor y is NaN
  let ne = roundedAdd TowardNearest x y
      ze = roundedAdd TowardZero x y
      inf = roundedAdd TowardInf x y
      ninf = roundedAdd TowardNegInf x y
  in ne == x + y .&&. ninf <= inf .&&. (ne == ninf || ne == inf) .&&. (ze == ninf || ze == inf) .&&. abs ze == min (abs ninf) (abs inf)

prop_roundedSub :: (Ord a, Num a, RoundedRing a) => Proxy a -> a -> a -> Property
prop_roundedSub _proxy x y =
  -- Assume neither x nor y is NaN
  let ne = roundedSub TowardNearest x y
      ze = roundedSub TowardZero x y
      inf = roundedSub TowardInf x y
      ninf = roundedSub TowardNegInf x y
  in ne == x - y .&&. ninf <= inf .&&. (ne == ninf || ne == inf) .&&. (ze == ninf || ze == inf) .&&. abs ze == min (abs ninf) (abs inf)

prop_roundedMul :: (Ord a, Num a, RoundedRing a) => Proxy a -> a -> a -> Property
prop_roundedMul _proxy x y =
  -- Assume neither x nor y is NaN
  let ne = roundedMul TowardNearest x y
      ze = roundedMul TowardZero x y
      inf = roundedMul TowardInf x y
      ninf = roundedMul TowardNegInf x y
  in ne == x * y .&&. ninf <= inf .&&. (ne == ninf || ne == inf) .&&. (ze == ninf || ze == inf) .&&. abs ze == min (abs ninf) (abs inf)

prop_roundedDiv :: (Ord a, Fractional a, RoundedFractional a) => Proxy a -> a -> NonZero a -> Property
prop_roundedDiv _proxy x (NonZero y) =
  -- Assume neither x nor y is NaN
  let ne = roundedDiv TowardNearest x y
      ze = roundedDiv TowardZero x y
      inf = roundedDiv TowardInf x y
      ninf = roundedDiv TowardNegInf x y
  in ne == x / y .&&. ninf <= inf .&&. (ne == ninf || ne == inf) .&&. (ze == ninf || ze == inf) .&&. abs ze == min (abs ninf) (abs inf)

specT :: (Ord a, Fractional a, RoundedFractional a, Arbitrary a, Show a) => Proxy a -> Spec
specT proxy = do
  prop "roundedAdd" $ prop_roundedAdd proxy
  prop "roundedSub" $ prop_roundedSub proxy
  prop "roundedMul" $ prop_roundedMul proxy
  prop "roundedDiv" $ prop_roundedDiv proxy

spec :: Spec
spec = do
  describe "Double" $ specT (Proxy :: Proxy Double)
  describe "Float" $ specT (Proxy :: Proxy Float)
