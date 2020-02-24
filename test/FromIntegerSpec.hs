{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module FromIntegerSpec where
import           Control.Monad
import           Data.Int
import           Data.Proxy
import           Data.Word
import           Numeric.Rounded.Hardware.Internal
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck
import           Util

prop_fromInteger_nearest_stock :: forall a. (RealFloat a, RoundedRing a) => Proxy a -> Integer -> Property
prop_fromInteger_nearest_stock _proxy x
  = (roundedFromInteger ToNearest x :: a)
    `sameFloatP` (fromInteger x :: a)

prop_roundedFromInteger_check :: forall a. (RealFloat a, RoundedRing a) => Proxy a -> RoundingMode -> Integer -> Property
prop_roundedFromInteger_check _proxy r x
  = (roundedFromInteger r x :: a)
    `sameFloatP` (fromInt r x :: a)

prop_roundedFromInt64_check :: forall a. (RealFloat a, RoundedRing a) => Proxy a -> RoundingMode -> Int64 -> Property
prop_roundedFromInt64_check _proxy r x
  = (roundedFromInteger r (fromIntegral x) :: a)
    `sameFloatP` (fromInt r (fromIntegral x) :: a)

prop_roundedFromWord64_check :: forall a. (RealFloat a, RoundedRing a) => Proxy a -> RoundingMode -> Word64 -> Property
prop_roundedFromWord64_check _proxy r x
  = (roundedFromInteger r (fromIntegral x) :: a)
    `sameFloatP` (fromInt r (fromIntegral x) :: a)

prop_fromInt_order :: forall a. RealFloat a => Proxy a -> Integer -> Property
prop_fromInt_order _proxy x
  = let ne   = fromInt ToNearest    x :: a
        ze   = fromInt TowardZero   x :: a
        inf  = fromInt TowardInf    x :: a
        ninf = fromInt TowardNegInf x :: a
    in ninf <= inf
       .&&. (ne == ninf || ne == inf)
       .&&. (if x < 0 then ze == inf else ze == ninf)

prop_fromInt_exact :: forall a. RealFloat a => Proxy a -> Integer -> Property
prop_fromInt_exact _proxy x
  = let inf  = fromInt TowardInf    x :: a
        ninf = fromInt TowardNegInf x :: a
    in if ninf == inf
       then not (isInfinite inf) .&&. toRational inf === fromInteger x
       else if isInfinite inf
            then inf > 0
                 .&&. not (isInfinite ninf)
                 .&&. toRational ninf =/= fromInteger x
            else if isInfinite ninf
                 then ninf < 0
                      .&&. not (isInfinite inf)
                      .&&. toRational inf =/= fromInteger x
                 else toRational inf =/= fromInteger x
                      .&&. toRational ninf =/= fromInteger x

specT :: forall a. (RealFloat a, RoundedRing a) => Proxy a -> Bool -> Spec
specT proxy checkAgainstStock = do
  when checkAgainstStock $ do
    prop "fromInteger (nearest) coincides with stock fromInteger" $
      -- fromInteger for Double/Float do not necessarily round to nearest.
      forAllShrink variousIntegers shrinkIntegral (prop_fromInteger_nearest_stock proxy)
  prop "roundedFromInteger coincides with the standard implementation" $ \r ->
    forAllShrink variousIntegers shrinkIntegral (prop_roundedFromInteger_check proxy r)
  prop "roundedFromInteger/Int64" $ \r ->
    prop_roundedFromInt64_check proxy r
  prop "roundedFromInteger/Word64" $ \r ->
    prop_roundedFromWord64_check proxy r
  prop "order" $
    forAllShrink variousIntegers shrinkIntegral (prop_fromInt_order proxy)
  prop "exactness" $
    forAllShrink variousIntegers shrinkIntegral (prop_fromInt_exact proxy)

spec :: Spec
spec = do
  describe "Double" $ specT (Proxy :: Proxy Double) False
  describe "Float" $ specT (Proxy :: Proxy Float) False
