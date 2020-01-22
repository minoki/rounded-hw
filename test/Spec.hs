{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
import qualified FromIntegerSpec
import qualified FromRationalSpec
import qualified RoundedArithmeticSpec
import qualified ShowFloatSpec
import           Test.Hspec
#ifdef TEST_X87_LONG_DOUBLE
import qualified X87LongDoubleSpec
#endif

main :: IO ()
main = hspec $ do
  describe "fromInteger" FromIntegerSpec.spec
  describe "fromRational" FromRationalSpec.spec
  describe "showFloat" ShowFloatSpec.spec
  describe "rounded arithmetic" RoundedArithmeticSpec.spec
#ifdef TEST_X87_LONG_DOUBLE
  describe "x87 long double" X87LongDoubleSpec.spec
#endif
