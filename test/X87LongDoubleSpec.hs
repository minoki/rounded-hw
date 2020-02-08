{-# OPTIONS_GHC -Wno-orphans #-}
module X87LongDoubleSpec where
import           Data.Proxy
import qualified FromIntegerSpec
import qualified FromRationalSpec
import qualified IntervalArithmeticSpec
import           Numeric.LongDouble (LongDouble)
import qualified RoundedArithmeticSpec
import qualified ShowFloatSpec
import           Test.Hspec
import           Test.QuickCheck.Arbitrary

-- orphan instance
instance Arbitrary LongDouble where
  arbitrary = arbitrarySizedFractional
  shrink = shrinkDecimal

spec :: Spec
spec = do
  describe "rounded arithmetic"  $ RoundedArithmeticSpec.specT ldProxy
  describe "rounded arithmetic"  $ RoundedArithmeticSpec.verifyImplementation ldProxy ldProxy
  describe "interval arithmetic" $ IntervalArithmeticSpec.verifyImplementation ldProxy
  describe "fromInteger"         $ FromIntegerSpec.specT ldProxy
  describe "fromRational"        $ FromRationalSpec.specT ldProxy
  describe "showFloat"           $ ShowFloatSpec.specT ldProxy
  where
    ldProxy :: Proxy LongDouble
    ldProxy = Proxy
