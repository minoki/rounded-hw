{-# OPTIONS_GHC -Wno-orphans #-}
module Float128Spec where
import qualified ConstantsSpec
import           Data.Int
import           Data.Proxy
import qualified FloatUtilSpec
import qualified FromIntegerSpec
import qualified FromRationalSpec
import qualified IntervalArithmeticSpec
import           Numeric.Float128 (Float128)
import qualified RoundedArithmeticSpec
import qualified ShowFloatSpec
import           System.Random
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck
import           Util

-- orphan instances
instance Arbitrary Float128 where
  arbitrary = arbitrarySizedFractional
  shrink = shrinkDecimal

instance Random Float128 where
  randomR (lo,hi) g = let (x,g') = random g
                      in (lo + x * (hi - lo), g') -- TODO: avoid overflow
  random g = let x :: Int64
                 (x,g') = random g
             in (fromIntegral x / 2^(63 :: Int), g') -- TODO: do better

spec :: Spec
spec = do
  describe "rounded arithmetic"  $ RoundedArithmeticSpec.specT f128Proxy
  describe "rounded arithmetic"  $ RoundedArithmeticSpec.verifyImplementation f128Proxy f128Proxy
  describe "interval arithmetic" $ IntervalArithmeticSpec.verifyImplementation f128Proxy
  describe "fromInteger"         $ FromIntegerSpec.specT f128Proxy False
  describe "fromRational"        $ FromRationalSpec.specT f128Proxy False
  describe "showFloat"           $ ShowFloatSpec.specT f128Proxy
  describe "constants"           $ ConstantsSpec.specT f128Proxy
  prop "nextUp . nextDown == id (unless -inf)" $ forAll variousFloats (FloatUtilSpec.prop_nextUp_nextDown :: Float128 -> Property)
  prop "nextDown . nextUp == id (unless inf)" $ forAll variousFloats (FloatUtilSpec.prop_nextDown_nextUp :: Float128 -> Property)
  where
    f128Proxy :: Proxy Float128
    f128Proxy = Proxy
