{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Numeric.Rounded.Hardware.Internal
import Test.Hspec
import Test.QuickCheck
import Data.Ratio
import Data.Proxy
import Data.Int
import Numeric

getRoundedDouble :: Rounded rn Double -> Double
getRoundedDouble (Rounded x) = x

newtype ShowHexFloat a = ShowHexFloat a deriving (Eq,Ord)

instance RealFloat a => Show (ShowHexFloat a) where
  showsPrec _prec (ShowHexFloat x) = showHFloat x

prop_fromInteger_nearest_stock :: Integer -> Property
prop_fromInteger_nearest_stock x
  = ShowHexFloat (getRoundedDouble (fromInteger x :: Rounded 'TowardNearest Double))
    === ShowHexFloat (fromInteger x :: Double)

prop_fromInt_order :: Integer -> Property
prop_fromInt_order x
  = let ne   = fromInt TowardNearest x :: Double
        ze   = fromInt TowardZero    x :: Double
        inf  = fromInt TowardInf     x :: Double
        ninf = fromInt TowardNegInf  x :: Double
    in ninf <= inf
       .&&. (ne == ninf || ne == inf)
       .&&. (if x < 0 then ze == inf else ze == ninf)

prop_fromInt_exact :: Integer -> Property
prop_fromInt_exact x
  = let inf  = fromInt TowardInf    x :: Double
        ninf = fromInt TowardNegInf x :: Double
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

prop_fromRational_nearest_stock :: Rational -> Property
prop_fromRational_nearest_stock x
  = ShowHexFloat (getRoundedDouble (fromRational x :: Rounded 'TowardNearest Double))
    === ShowHexFloat (fromRational x :: Double)

prop_fromRational :: forall rn. Rounding rn => Proxy rn -> Rational -> Property
prop_fromRational proxy x
  = ShowHexFloat (fromRatio (rounding proxy) (numerator x) (denominator x))
    === ShowHexFloat (getRoundedDouble (fromRational x :: Rounded rn Double))

prop_fromRatio_order :: Rational -> Property
prop_fromRatio_order x
  = let ne   = fromRatio TowardNearest (numerator x) (denominator x) :: Double
        ze   = fromRatio TowardZero    (numerator x) (denominator x) :: Double
        inf  = fromRatio TowardInf     (numerator x) (denominator x) :: Double
        ninf = fromRatio TowardNegInf  (numerator x) (denominator x) :: Double
    in ninf <= inf
       .&&. (ne == ninf || ne == inf)
       .&&. (if x < 0 then ze == inf else ze == ninf)

prop_fromRatio_exact :: Rational -> Property
prop_fromRatio_exact x
  = let inf  = fromRatio TowardInf    (numerator x) (denominator x) :: Double
        ninf = fromRatio TowardNegInf (numerator x) (denominator x) :: Double
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

prop_showEFloatP :: Int -> Int32 -> Property
prop_showEFloatP prec x = showEFloat mprec x' "" === showEFloatRn TowardNearest mprec x' ""
  where mprec = Just prec
        x' = fromIntegral x / 64

prop_showFFloatP ::  Int -> Int32 -> Property
prop_showFFloatP prec x = showFFloat mprec x' "" === showFFloatRn TowardNearest mprec x' ""
  where mprec = Just prec
        x' = fromIntegral x / 64

prop_showGFloatP :: Int -> Int32 -> Property
prop_showGFloatP prec x = showGFloat mprec x' "" === showGFloatRn TowardNearest mprec x' ""
  where mprec = Just prec
        x' = fromIntegral x / 64

main :: IO ()
main = hspec $ do
  describe "fromInteger" $ do
    it "fromInteger (nearest) coincides with stock fromInteger" $
      property $ prop_fromInteger_nearest_stock
    it "order" $
      property prop_fromInt_order
    it "exactness" $
      property $ prop_fromInt_exact
  describe "fromRational" $ do
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
      property prop_fromRatio_order
    it "exactness" $
      property $ prop_fromRatio_exact
  describe "showFloat" $ do
    it "showEFloat (nearest)" $
      property prop_showEFloatP
    it "showFFloat (nearest)" $
      property prop_showFFloatP
    it "showGFloat (nearest)" $
      property prop_showGFloatP
    it "showEFloat/Int32" $
      property (\mprec (x :: Int32) -> showEFloat mprec (fromIntegral x) "" === showEFloatRn TowardNearest mprec (fromIntegral x) "")
    it "showFFloat/Int32" $
      property (\mprec (x :: Int32) -> showFFloat mprec (fromIntegral x) "" === showFFloatRn TowardNearest mprec (fromIntegral x) "")
    it "showGFloat/Int32" $
      property (\mprec (x :: Int32) -> showGFloat mprec (fromIntegral x) "" === showGFloatRn TowardNearest mprec (fromIntegral x) "")
