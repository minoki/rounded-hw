import Test.DocTest

main :: IO ()
main = doctest [ "-isrc"
               , "src/Numeric/Rounded/Hardware/Internal/Rounding.hs"
               , "src/Numeric/Rounded/Hardware/Internal/Conversion.hs"
               , "src/Numeric/Rounded/Hardware/Internal/Show.hs"
               , "src/Numeric/Rounded/Hardware/Internal/FloatUtil.hs"
               ]
