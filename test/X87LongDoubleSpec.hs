{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module X87LongDoubleSpec where
import           Data.Proxy
import qualified FromIntegerSpec
import qualified FromRationalSpec
import           Numeric.LongDouble (LongDouble)
import           Numeric.Rounded.Hardware.Backend.X87LongDouble ()
import qualified RoundedArithmeticSpec
import qualified ShowFloatSpec
import           Test.Hspec
import           Test.QuickCheck.Arbitrary

instance Arbitrary LongDouble where
  arbitrary = arbitrarySizedFractional
  shrink = shrinkDecimal

spec :: Spec
spec = do
  let ldProxy :: Proxy LongDouble
      ldProxy = Proxy
  describe "rounded arithmetic" $ RoundedArithmeticSpec.specT ldProxy
  describe "rounded arithmetic" $ RoundedArithmeticSpec.verifyImplementation ldProxy ldProxy
  describe "fromInteger"        $ FromIntegerSpec.specT ldProxy
  describe "fromRational"       $ FromRationalSpec.specT ldProxy
  describe "showFloat"          $ ShowFloatSpec.specT ldProxy
