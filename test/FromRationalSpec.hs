{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module FromRationalSpec where
import           Data.Proxy
import           Data.Ratio
import           Numeric.Rounded.Hardware.Internal
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck
import           Util

prop_fromRational_nearest_stock :: forall a. (RealFloat a, RoundedFractional a) => Proxy a -> Rational -> Property
prop_fromRational_nearest_stock _proxy x
  = ShowHexFloat (getRounded (fromRational x :: Rounded 'TowardNearest a))
    === ShowHexFloat (fromRational x :: a)

prop_fromRational :: forall rn a. (Rounding rn, RealFloat a, RealFloatConstants a, RoundedFractional a) => Proxy rn -> Proxy a -> Rational -> Property
prop_fromRational rnProxy _proxy x
  = ShowHexFloat (fromRatio (rounding rnProxy) (numerator x) (denominator x))
    === ShowHexFloat (getRounded (fromRational x :: Rounded rn a))

prop_fromRatio_order :: forall a. (RealFloat a, RealFloatConstants a) => Proxy a -> Rational -> Property
prop_fromRatio_order _proxy x
  = let ne   = fromRatio TowardNearest (numerator x) (denominator x) :: a
        ze   = fromRatio TowardZero    (numerator x) (denominator x) :: a
        inf  = fromRatio TowardInf     (numerator x) (denominator x) :: a
        ninf = fromRatio TowardNegInf  (numerator x) (denominator x) :: a
    in ninf <= inf
       .&&. (ne == ninf || ne == inf)
       .&&. (if x < 0 then ze == inf else ze == ninf)

prop_fromRatio_exact :: forall a. (RealFloat a, RealFloatConstants a) => Proxy a -> Rational -> Property
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

specT :: forall a. (RealFloat a, RoundedFractional a, RealFloatConstants a) => Proxy a -> Spec
specT proxy = do
  prop "fromRational (nearest) coincides with stock fromRational" $
    prop_fromRational_nearest_stock proxy
  prop "fromRational for small numbers coincides with fromRationl (nearest)" $
    prop_fromRational (Proxy :: Proxy 'TowardNearest) proxy
  prop "fromRational for small numbers coincides with fromRationl (toward zero)" $
    prop_fromRational (Proxy :: Proxy 'TowardZero) proxy
  prop "fromRational for small numbers coincides with fromRationl (toward inf)" $
    prop_fromRational (Proxy :: Proxy 'TowardInf) proxy
  prop "fromRational for small numbers coincides with fromRationl (toward neg inf)" $
    prop_fromRational (Proxy :: Proxy 'TowardNegInf) proxy
  prop "order" $
    prop_fromRatio_order proxy
  prop "exactness" $
    prop_fromRatio_exact proxy

spec :: Spec
spec = do
  describe "Double" $ specT (Proxy :: Proxy Double)
  describe "Float" $ specT (Proxy :: Proxy Float)
