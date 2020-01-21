{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module FromIntegerSpec where
import           Data.Proxy
import           Numeric.Rounded.Hardware.Internal
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck
import           Util

prop_fromInteger_nearest_stock :: forall a. (RealFloat a, RoundedRing a) => Proxy a -> Integer -> Property
prop_fromInteger_nearest_stock _proxy x
  = ShowHexFloat (getRounded (fromInteger x :: Rounded 'TowardNearest a))
    === ShowHexFloat (fromInteger x :: a)

prop_fromInt_order :: forall a. (RealFloat a, RealFloatConstants a) => Proxy a -> Integer -> Property
prop_fromInt_order _proxy x
  = let ne   = fromInt TowardNearest x :: a
        ze   = fromInt TowardZero    x :: a
        inf  = fromInt TowardInf     x :: a
        ninf = fromInt TowardNegInf  x :: a
    in ninf <= inf
       .&&. (ne == ninf || ne == inf)
       .&&. (if x < 0 then ze == inf else ze == ninf)

prop_fromInt_exact :: forall a. (RealFloat a, RealFloatConstants a) => Proxy a -> Integer -> Property
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

specT :: forall a. (RealFloat a, RealFloatConstants a, RoundedRing a) => Proxy a -> Spec
specT proxy = do
  prop "fromInteger (nearest) coincides with stock fromInteger" $
    prop_fromInteger_nearest_stock proxy
  prop "order" $
    prop_fromInt_order proxy
  prop "exactness" $
    prop_fromInt_exact proxy

spec = do
  describe "Double" $ specT (Proxy :: Proxy Double)
  describe "Float" $ specT (Proxy :: Proxy Float)
