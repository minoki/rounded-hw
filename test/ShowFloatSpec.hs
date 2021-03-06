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
import           Util ()

prop_showEFloat_nearest :: forall a. RealFloat a => Proxy a -> Int -> Int32 -> Property
prop_showEFloat_nearest _proxy prec x = showEFloat mprec x' "" === showEFloatRn ToNearest mprec x' ""
  where mprec = Just prec
        x' = fromIntegral x / 64 :: a

prop_showFFloat_nearest :: forall a. RealFloat a => Proxy a -> Int -> Int32 -> Property
prop_showFFloat_nearest _proxy prec x = showFFloat mprec x' "" === showFFloatRn ToNearest mprec x' ""
  where mprec = Just prec
        x' = fromIntegral x / 64 :: a

prop_showGFloat_nearest :: forall a. RealFloat a => Proxy a -> Int -> Int32 -> Property
prop_showGFloat_nearest _proxy prec x = showGFloat mprec x' "" === showGFloatRn ToNearest mprec x' ""
  where mprec = Just prec
        x' = fromIntegral x / 64 :: a

prop_showEFloat :: forall a. RealFloat a => Proxy a -> Maybe Int -> a -> Property
prop_showEFloat _proxy mprec x =
  let rn = showEFloatRn ToNearest mprec x ""
      ru = showEFloatRn TowardInf mprec x ""
      rd = showEFloatRn TowardNegInf mprec x ""
      rz = showEFloatRn TowardZero mprec x ""
  in rn === ru .||. rn === rd .||. rz === (if x > 0 then rd else ru)

prop_showFFloat :: forall a. RealFloat a => Proxy a -> Maybe Int -> a -> Property
prop_showFFloat _proxy mprec x =
  let rn = showFFloatRn ToNearest mprec x ""
      ru = showFFloatRn TowardInf mprec x ""
      rd = showFFloatRn TowardNegInf mprec x ""
      rz = showFFloatRn TowardZero mprec x ""
  in rn === ru .||. rn === rd .||. rz === (if x > 0 then rd else ru)

prop_showGFloat :: forall a. RealFloat a => Proxy a -> Maybe Int -> a -> Property
prop_showGFloat _proxy mprec x =
  let rn = showGFloatRn ToNearest mprec x ""
      ru = showGFloatRn TowardInf mprec x ""
      rd = showGFloatRn TowardNegInf mprec x ""
      rz = showGFloatRn TowardZero mprec x ""
  in rn === ru .||. rn === rd .||. rz === (if x > 0 then rd else ru)

testAgainstNumeric :: forall a. RealFloat a => Proxy a -> Spec
testAgainstNumeric proxy = do
  describe "showFloat" $ do
    prop "showEFloat (nearest)" $ prop_showEFloat_nearest proxy
    prop "showFFloat (nearest)" $ prop_showFFloat_nearest proxy
    prop "showGFloat (nearest)" $ prop_showGFloat_nearest proxy
    prop "showEFloat/Int32" $ \mprec (x :: Int32) ->
      showEFloat mprec (fromIntegral x :: a) "" === showEFloatRn ToNearest mprec (fromIntegral x :: a) ""
    prop "showFFloat/Int32" $ \mprec (x :: Int32) ->
      showFFloat mprec (fromIntegral x :: a) "" === showFFloatRn ToNearest mprec (fromIntegral x :: a) ""
    prop "showGFloat/Int32" $ \mprec (x :: Int32) ->
      showGFloat mprec (fromIntegral x :: a) "" === showGFloatRn ToNearest mprec (fromIntegral x :: a) ""

specT :: forall a. (RealFloat a, Arbitrary a, Show a) => Proxy a -> Spec
specT proxy = do
  prop "showEFloat" $ prop_showEFloat proxy
  prop "showFFloat" $ prop_showFFloat proxy
  prop "showGFloat" $ prop_showGFloat proxy

  -- 0.5 should be exactly representable in the type...
  prop "showFFloatRn Nothing 0.5"  $ \r -> showFFloatRn r Nothing  (0.5 :: a) "" === "0.5"
  prop "showFFloatRn (Just 0) 0.5" $ \r -> showFFloatRn r (Just 0) (0.5 :: a) "" === (if r == TowardInf then "1" else "0")
  prop "showFFloatRn (Just 3) 0.5" $ \r -> showFFloatRn r (Just 3) (0.5 :: a) "" === "0.500"
  prop "showGFloatRn Nothing 0.5"  $ \r -> showGFloatRn r Nothing  (0.5 :: a) "" === "0.5"
  prop "showGFloatRn (Just 0) 0.5" $ \r -> showGFloatRn r (Just 0) (0.5 :: a) "" === (if r == TowardInf then "1" else "0")
  prop "showGFloatRn (Just 3) 0.5" $ \r -> showGFloatRn r (Just 3) (0.5 :: a) "" === "0.500"
  prop "showEFloatRn Nothing 0.5"  $ \r -> showEFloatRn r Nothing  (0.5 :: a) "" === "5.0e-1"
  prop "showEFloatRn (Just 0) 0.5" $ \r -> showEFloatRn r (Just 0) (0.5 :: a) "" === "5e-1"
  prop "showEFloatRn (Just 3) 0.5" $ \r -> showEFloatRn r (Just 3) (0.5 :: a) "" === "5.000e-1"

spec :: Spec
spec = do
  describe "Double" $ testAgainstNumeric (Proxy :: Proxy Double)
  -- The functions in Numeric yields a rounded value:
  -- >>> showFFloat Nothing (137846.59375 :: Float) ""
  -- "137846.6"
  -- So comparing them
  -- describe "Float" $ testAgainstNumeric (Proxy :: Proxy Float)
  describe "Double" $ specT (Proxy :: Proxy Double)
  describe "Float" $ specT (Proxy :: Proxy Float)
