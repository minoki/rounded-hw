{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Data.Proxy
import qualified FromIntegerSpec
import qualified FromRationalSpec
import qualified IntervalArithmeticSpec
import           Numeric.Rounded.Hardware.Backend (backendName)
import qualified RoundedArithmeticSpec
import qualified ShowFloatSpec
import qualified FloatUtilSpec
import           Test.Hspec
import qualified VectorSpec
#ifdef TEST_X87_LONG_DOUBLE
import           Numeric.LongDouble (LongDouble)
import qualified X87LongDoubleSpec
#endif
#ifdef TEST_FLOAT128
import           Numeric.Float128 (Float128)
import qualified Float128Spec
#endif

printBackends :: IO ()
printBackends = do
  putStrLn $ "Backend for Double: " ++ backendName (Proxy :: Proxy Double)
  putStrLn $ "Backend for Float: " ++ backendName (Proxy :: Proxy Float)
#ifdef TEST_X87_LONG_DOUBLE
  putStrLn $ "Backend for LongDouble: " ++ backendName (Proxy :: Proxy LongDouble)
#endif
#ifdef TEST_FLOAT128
  putStrLn $ "Backend for Float128: " ++ backendName (Proxy :: Proxy Float128)
#endif

main :: IO ()
main = do
  printBackends
  hspec $ do
    describe "fromInteger" FromIntegerSpec.spec
    describe "fromRational" FromRationalSpec.spec
    describe "showFloat" ShowFloatSpec.spec
    describe "rounded arithmetic" RoundedArithmeticSpec.spec
    describe "interval arithmetic" IntervalArithmeticSpec.spec
    describe "FloatUtil" FloatUtilSpec.spec
    describe "Vector" VectorSpec.spec
#ifdef TEST_X87_LONG_DOUBLE
    describe "x87 long double" X87LongDoubleSpec.spec
#endif
#ifdef TEST_FLOAT128
    describe "Float128" Float128Spec.spec
#endif
