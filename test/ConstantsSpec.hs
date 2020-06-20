{-# LANGUAGE DataKinds #-}
module ConstantsSpec (spec, specT) where
import           Data.Proxy
import           Numeric.Rounded.Hardware.Internal
import           Test.Hspec
import           Util

prop_maxFinite :: (RealFloat a, RealFloatConstants a) => Proxy a -> Bool
prop_maxFinite proxy = nextUp maxFinite `sameFloat` (positiveInfinity `asProxyTypeOf` proxy)

prop_minPositive :: (RealFloat a, RealFloatConstants a) => Proxy a -> Bool
prop_minPositive proxy = nextDown minPositive `sameFloat` (0 `asProxyTypeOf` proxy)

prop_almostExact :: RealFloat a => Proxy a -> Rounded 'TowardNegInf a -> Rounded 'TowardInf a -> Bool
prop_almostExact _proxy (Rounded x) (Rounded y) = (x `sameFloat` nextDown y) && (nextUp x `sameFloat` y)

specT :: (RealFloat a, RealFloatConstants a) => Proxy a -> Spec
specT proxy = do
  it "maxFinite" $ prop_maxFinite proxy
  it "minPositive" $ prop_minPositive proxy
  it "pi" $ prop_almostExact proxy pi_down pi_up
  it "3*pi" $ prop_almostExact proxy three_pi_down three_pi_up
  it "5*pi" $ prop_almostExact proxy five_pi_down five_pi_up
  it "log(2)" $ prop_almostExact proxy log2_down log2_up
  it "exp(1)" $ prop_almostExact proxy exp1_down exp1_up
  it "exp(1/2)" $ prop_almostExact proxy exp1_2_down exp1_2_up
  it "exp(-1/2)" $ prop_almostExact proxy expm1_2_down expm1_2_up
  it "sqrt(2)" $ prop_almostExact proxy sqrt2_down sqrt2_up
  it "sqrt(2)-1" $ prop_almostExact proxy sqrt2m1_down sqrt2m1_up
  it "sqrt(1/2)" $ prop_almostExact proxy sqrt1_2_down sqrt1_2_up
  it "3-2*sqrt(2)" $ prop_almostExact proxy three_minus_2sqrt2_down three_minus_2sqrt2_up
  it "2-sqrt(2)" $ prop_almostExact proxy two_minus_sqrt2_down two_minus_sqrt2_up

spec :: Spec
spec = do
  describe "Double" $ specT (Proxy :: Proxy Double)
  describe "Float" $ specT (Proxy :: Proxy Float)
