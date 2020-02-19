{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module RoundedArithmeticSpec where
import           Data.Coerce
import           Data.Proxy
import qualified Numeric.Rounded.Hardware.Backend.C as Backend.C
import           Numeric.Rounded.Hardware.Backend.ViaRational
import           Numeric.Rounded.Hardware.Internal
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck
import           Util

infix 4 ==.
(==.) :: (RealFloat a, Show a) => a -> a -> Property
(==.) = sameFloatP

prop_roundedAdd :: (RealFloat a, Show a, RoundedRing a) => Proxy a -> a -> a -> Property
prop_roundedAdd _proxy x y =
  -- Assume neither x nor y is NaN
  let ne = roundedAdd ToNearest x y
      ze = roundedAdd TowardZero x y
      inf = roundedAdd TowardInf x y
      ninf = roundedAdd TowardNegInf x y
  in ne ==. x + y .&&. ninf <= inf .&&. (ne ==. ninf .||. ne ==. inf) .&&. (ze ==. ninf .||. ze ==. inf) .&&. abs ze ==. min (abs ninf) (abs inf)

prop_roundedSub :: (RealFloat a, Show a, RoundedRing a) => Proxy a -> a -> a -> Property
prop_roundedSub _proxy x y =
  -- Assume neither x nor y is NaN
  let ne = roundedSub ToNearest x y
      ze = roundedSub TowardZero x y
      inf = roundedSub TowardInf x y
      ninf = roundedSub TowardNegInf x y
  in ne ==. x - y .&&. ninf <= inf .&&. (ne ==. ninf .||. ne ==. inf) .&&. (ze ==. ninf .||. ze ==. inf) .&&. abs ze ==. min (abs ninf) (abs inf)

prop_roundedMul :: (RealFloat a, Show a, RoundedRing a) => Proxy a -> a -> a -> Property
prop_roundedMul _proxy x y =
  -- Assume neither x nor y is NaN
  let ne = roundedMul ToNearest x y
      ze = roundedMul TowardZero x y
      inf = roundedMul TowardInf x y
      ninf = roundedMul TowardNegInf x y
  in ne ==. x * y .&&. ninf <= inf .&&. (ne ==. ninf .||. ne ==. inf) .&&. (ze ==. ninf .||. ze ==. inf) .&&. abs ze ==. min (abs ninf) (abs inf)

prop_roundedDiv :: (RealFloat a, Show a, RoundedFractional a) => Proxy a -> a -> NonZero a -> Property
prop_roundedDiv _proxy x (NonZero y) =
  -- Assume neither x nor y is NaN
  let ne = roundedDiv ToNearest x y
      ze = roundedDiv TowardZero x y
      inf = roundedDiv TowardInf x y
      ninf = roundedDiv TowardNegInf x y
  in ne ==. x / y .&&. ninf <= inf .&&. (ne ==. ninf .||. ne ==. inf) .&&. (ze ==. ninf .||. ze ==. inf) .&&. abs ze ==. min (abs ninf) (abs inf)

prop_roundedSqrt :: (RealFloat a, Show a, RoundedSqrt a) => Proxy a -> a -> Property
prop_roundedSqrt _proxy x =
  -- Assume neither x nor y is NaN
  let ne = roundedSqrt ToNearest x
      ze = roundedSqrt TowardZero x
      inf = roundedSqrt TowardInf x
      ninf = roundedSqrt TowardNegInf x
  in if isNaN x || x < 0
     then isNaN ne .&&. isNaN ze .&&. isNaN inf .&&. isNaN ninf
     else ne ==. sqrt x .&&. ninf <= inf .&&. (ne ==. ninf .||. ne ==. inf) .&&. (ze ==. ninf .||. ze ==. inf) .&&. abs ze ==. min (abs ninf) (abs inf)

specT :: (RealFloat a, RoundedFractional a, RoundedSqrt a, Arbitrary a, Show a) => Proxy a -> Spec
specT proxy = do
  prop "roundedAdd" $ prop_roundedAdd proxy
  prop "roundedSub" $ prop_roundedSub proxy
  prop "roundedMul" $ prop_roundedMul proxy
  prop "roundedDiv" $ prop_roundedDiv proxy
  prop "roundedSqrt" $ prop_roundedSqrt proxy

verifyImplementation :: forall base a. (RealFloat base, RoundedFractional a, RoundedSqrt a, Arbitrary base, Show base, RealFloatConstants base, Coercible a base) => Proxy base -> Proxy a -> Spec
verifyImplementation _ _ = do
  let unVR (ViaRational x) = x
      c :: base -> a
      c x = coerce x
  prop "roundedAdd" $ \r x y -> unVR (roundedAdd r (ViaRational x) (ViaRational y)) ==. coerce (roundedAdd r (c x) (c y))
  prop "roundedSub" $ \r x y -> unVR (roundedSub r (ViaRational x) (ViaRational y)) ==. coerce (roundedSub r (c x) (c y))
  prop "roundedMul" $ \r x y -> unVR (roundedMul r (ViaRational x) (ViaRational y)) ==. coerce (roundedMul r (c x) (c y))
  prop "roundedDiv" $ \r x y -> unVR (roundedDiv r (ViaRational x) (ViaRational y)) ==. coerce (roundedDiv r (c x) (c y))
  prop "roundedSqrt" $ \r x -> unVR (roundedSqrt r (ViaRational x)) ==. coerce (roundedSqrt r (c x))

spec :: Spec
spec = do
  describe "Double" $ specT (Proxy :: Proxy Double)
  describe "Float" $ specT (Proxy :: Proxy Float)
  describe "Double default" $ verifyImplementation (Proxy :: Proxy Double) (Proxy :: Proxy Double)
  describe "Float default" $ verifyImplementation (Proxy :: Proxy Float) (Proxy :: Proxy Float)

  -- TODO: Disable when `pure-hs` is on
  describe "Double C" $ verifyImplementation (Proxy :: Proxy Double) (Proxy :: Proxy Backend.C.CDouble)
  describe "Float C" $ verifyImplementation (Proxy :: Proxy Float) (Proxy :: Proxy Backend.C.CFloat)
