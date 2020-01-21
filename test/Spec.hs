{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
import qualified FromIntegerSpec
import qualified FromRationalSpec
import qualified RoundedArithmeticSpec
import qualified ShowFloatSpec
import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "fromInteger" FromIntegerSpec.spec
  describe "fromRational" FromRationalSpec.spec
  describe "showFloat" ShowFloatSpec.spec
  describe "rounded arithmetic" RoundedArithmeticSpec.spec
