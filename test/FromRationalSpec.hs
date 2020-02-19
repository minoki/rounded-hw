{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module FromRationalSpec where
import           Control.Monad
import           Data.Proxy
import           Data.Ratio
import           Numeric.Rounded.Hardware.Internal
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck
import           Util

prop_fromRational_nearest_stock :: forall a. (RealFloat a, RoundedFractional a) => Proxy a -> Rational -> Property
prop_fromRational_nearest_stock _proxy x
  = (roundedFromRational TowardNearest x :: a)
    `sameFloatP` (fromRational x :: a)

prop_roundedFromRational_check :: forall a. (RealFloat a, RoundedFractional a) => Proxy a -> RoundingMode -> Rational -> Property
prop_roundedFromRational_check _proxy r x
  = (fromRatio r (numerator x) (denominator x) :: a) -- the standard implementation
    `sameFloatP` (roundedFromRational r x :: a) -- may be optimized

prop_fromRatio_order :: forall a. RealFloat a => Proxy a -> Rational -> Property
prop_fromRatio_order _proxy x
  = let ne   = fromRatio TowardNearest (numerator x) (denominator x) :: a
        ze   = fromRatio TowardZero    (numerator x) (denominator x) :: a
        inf  = fromRatio TowardInf     (numerator x) (denominator x) :: a
        ninf = fromRatio TowardNegInf  (numerator x) (denominator x) :: a
    in ninf <= inf
       .&&. (ne == ninf || ne == inf)
       .&&. (if x < 0 then ze == inf else ze == ninf)

prop_fromRatio_exact :: forall a. RealFloat a => Proxy a -> Rational -> Property
prop_fromRatio_exact _proxy x
  = let inf  = fromRatio TowardInf    (numerator x) (denominator x) :: a
        ninf = fromRatio TowardNegInf (numerator x) (denominator x) :: a
    in if ninf == inf
       then not (isInfinite inf) .&&. toRational inf === x
       else if isInfinite inf
            then inf > 0
                 .&&. not (isInfinite ninf)
                 .&&. toRational ninf =/= x
            else if isInfinite ninf
                 then ninf < 0
                      .&&. not (isInfinite inf)
                      .&&. toRational inf =/= x
                 else toRational inf =/= x
                      .&&. toRational ninf =/= x

specT :: forall a. (RealFloat a, RoundedFractional a) => Proxy a -> Bool -> Spec
specT proxy checkAgainstStock = do
  when checkAgainstStock $ do
    -- Although fromRational for Double/Float correctly round to nearest, other types may not.
    prop "fromRational (nearest) coincides with stock fromRational" $
      forAllShrink variousRationals shrinkRealFrac $ prop_fromRational_nearest_stock proxy
  prop "roundedFromRational coincides with the standard implementation" $ \r ->
    forAllShrink variousRationals shrinkRealFrac $ prop_roundedFromRational_check proxy r
  prop "order" $
    forAllShrink variousRationals shrinkRealFrac $ prop_fromRatio_order proxy
  prop "exactness" $
    forAllShrink variousRationals shrinkRealFrac $ prop_fromRatio_exact proxy

spec :: Spec
spec = do
  describe "Double" $ specT (Proxy :: Proxy Double) True
  describe "Float" $ specT (Proxy :: Proxy Float) True
