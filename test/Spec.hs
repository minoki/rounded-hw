{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Numeric.Rounded.Hardware.Internal
import Test.Hspec
import Test.QuickCheck
import Data.Ratio
import Data.Proxy
import Numeric

newtype ShowHexFloat a = ShowHexFloat a deriving (Eq,Ord)

instance RealFloat a => Show (ShowHexFloat a) where
  showsPrec _prec (ShowHexFloat x) = showHFloat x

prop_fromRational_nearest_stock :: Rational -> Property
prop_fromRational_nearest_stock x
  = ShowHexFloat (getRoundedDouble (fromRational x :: RoundedDouble TowardNearest))
    === ShowHexFloat (fromRational x :: Double)

prop_fromRational :: forall rn. RoundedPrim rn => Proxy rn -> Rational -> Property
prop_fromRational proxy x
  = ShowHexFloat (fromRatio (rounding proxy) (numerator x) (denominator x))
    === ShowHexFloat (getRoundedDouble (fromRational x :: RoundedDouble rn))

prop_order :: Rational -> Property
prop_order x = let ne   = fromRatio TowardNearest (numerator x) (denominator x)
                   ze   = fromRatio TowardZero    (numerator x) (denominator x)
                   inf  = fromRatio TowardInf     (numerator x) (denominator x)
                   ninf = fromRatio TowardNegInf  (numerator x) (denominator x)
               in ninf <= inf
                  .&&. (ne == ninf || ne == inf)
                  .&&. (if x < 0 then ze == inf else ze == ninf)

prop_exact :: Rational -> Property
prop_exact x = let ne   = fromRatio TowardNearest (numerator x) (denominator x)
                   ze   = fromRatio TowardZero    (numerator x) (denominator x)
                   inf  = fromRatio TowardInf     (numerator x) (denominator x)
                   ninf = fromRatio TowardNegInf  (numerator x) (denominator x)
               in if ninf == inf
                  then toRational inf === x
                  else toRational inf =/= x .&&. toRational ninf =/= x

main :: IO ()
main = hspec $ do
  describe "fromRatio" $ do
    it "fromRational (nearest) coincides with stock fromRational" $
      property $ prop_fromRational_nearest_stock
    it "fromRational for small numbers coincides with fromRationl (nearest)" $
      property $ prop_fromRational (Proxy :: Proxy TowardNearest)
    it "fromRational for small numbers coincides with fromRationl (toward zero)" $
      property $ prop_fromRational (Proxy :: Proxy TowardZero)
    it "fromRational for small numbers coincides with fromRationl (toward inf)" $
      property $ prop_fromRational (Proxy :: Proxy TowardInf)
    it "fromRational for small numbers coincides with fromRationl (toward neg inf)" $
      property $ prop_fromRational (Proxy :: Proxy TowardNegInf)
    it "order" $
      property prop_order
    it "exactness" $
      property $ prop_exact
