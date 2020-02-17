{-# OPTIONS_GHC -Wno-orphans #-}
module X87LongDoubleSpec where
import           Data.Int
import           Data.Proxy
import qualified FloatUtilSpec
import qualified FromIntegerSpec
import qualified FromRationalSpec
import qualified IntervalArithmeticSpec
import           Numeric.LongDouble (LongDouble)
import qualified RoundedArithmeticSpec
import qualified ShowFloatSpec
import           System.Random
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck
import           Util

-- orphan instances
instance Arbitrary LongDouble where
  arbitrary = arbitrarySizedFractional
  shrink = shrinkDecimal

instance Random LongDouble where
  randomR (lo,hi) g = let (x,g') = random g
                      in (lo + x * (hi - lo), g') -- TODO: avoid overflow
  random g = let x :: Int64
                 (x,g') = random g
             in (fromIntegral x / 2^(63 :: Int), g') -- TODO: do better

spec :: Spec
spec = do
  describe "rounded arithmetic"  $ RoundedArithmeticSpec.specT ldProxy
  describe "rounded arithmetic"  $ RoundedArithmeticSpec.verifyImplementation ldProxy ldProxy
  describe "interval arithmetic" $ IntervalArithmeticSpec.verifyImplementation ldProxy
  describe "fromInteger"         $ FromIntegerSpec.specT ldProxy
  describe "fromRational"        $ FromRationalSpec.specT ldProxy
  describe "showFloat"           $ ShowFloatSpec.specT ldProxy
  prop "nextUp . nextDown == id (unless -inf)" $ forAll variousFloats (FloatUtilSpec.prop_nextUp_nextDown :: LongDouble -> Property)
  prop "nextDown . nextUp == id (unless inf)" $ forAll variousFloats (FloatUtilSpec.prop_nextDown_nextUp :: LongDouble -> Property)
  where
    ldProxy :: Proxy LongDouble
    ldProxy = Proxy
