import Test.DocTest

main = doctest [ "-isrc"
               , "src/Numeric/Rounded/Hardware/Rounding.hs"
               , "src/Numeric/Rounded/Hardware/Util/Conversion.hs"
               , "src/Numeric/Rounded/Hardware/Util/Show.hs"
               ]
