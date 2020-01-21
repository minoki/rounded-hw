{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ShowFloatSpec where
import           Data.Int
import           Data.Proxy
import           Numeric
import           Numeric.Rounded.Hardware.Internal
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck

prop_showEFloatP :: forall a. RealFloat a => Proxy a -> Int -> Int32 -> Property
prop_showEFloatP _proxy prec x = showEFloat mprec x' "" === showEFloatRn TowardNearest mprec x' ""
  where mprec = Just prec
        x' = fromIntegral x / 64 :: a

prop_showFFloatP :: forall a. RealFloat a => Proxy a -> Int -> Int32 -> Property
prop_showFFloatP _proxy prec x = showFFloat mprec x' "" === showFFloatRn TowardNearest mprec x' ""
  where mprec = Just prec
        x' = fromIntegral x / 64 :: a

prop_showGFloatP :: forall a. RealFloat a => Proxy a -> Int -> Int32 -> Property
prop_showGFloatP _proxy prec x = showGFloat mprec x' "" === showGFloatRn TowardNearest mprec x' ""
  where mprec = Just prec
        x' = fromIntegral x / 64 :: a

specT :: forall a. RealFloat a => Proxy a -> Spec
specT proxy = do
  describe "showFloat" $ do
    prop "showEFloat (nearest)" $ prop_showEFloatP proxy
    prop "showFFloat (nearest)" $ prop_showFFloatP proxy
    prop "showGFloat (nearest)" $ prop_showGFloatP proxy
    prop "showEFloat/Int32" $ \mprec (x :: Int32) ->
      showEFloat mprec (fromIntegral x :: a) "" === showEFloatRn TowardNearest mprec (fromIntegral x :: a) ""
    prop "showFFloat/Int32" $ \mprec (x :: Int32) ->
      showFFloat mprec (fromIntegral x :: a) "" === showFFloatRn TowardNearest mprec (fromIntegral x :: a) ""
    prop "showGFloat/Int32" $ \mprec (x :: Int32) ->
      showGFloat mprec (fromIntegral x :: a) "" === showGFloatRn TowardNearest mprec (fromIntegral x :: a) ""

spec :: Spec
spec = do
  describe "Double" $ specT (Proxy :: Proxy Double)
  describe "Float" $ specT (Proxy :: Proxy Float)
