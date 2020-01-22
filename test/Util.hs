{-# OPTIONS_GHC -Wno-orphans #-}
module Util where
import           Numeric
import           Numeric.Rounded.Hardware.Internal
import           Test.QuickCheck

newtype ShowHexFloat a = ShowHexFloat a deriving (Eq,Ord)

instance RealFloat a => Show (ShowHexFloat a) where
  showsPrec _prec (ShowHexFloat x) = showHFloat x

instance Arbitrary RoundingMode where
  arbitrary = elements [TowardNearest, TowardNegInf, TowardInf, TowardZero]
  shrink TowardNearest = []
  shrink TowardInf     = [TowardNearest]
  shrink TowardNegInf  = [TowardNearest, TowardInf]
  shrink TowardZero    = [TowardNearest, TowardInf, TowardNegInf]

-- | Compares two floating point values.
--
-- Unlike @(==)@, @+0@ and @-0@ are considered distinct and NaNs are equal.
--
-- >>> sameFloat 0 (-0 :: Double)
-- False
-- >>> sameFloat (0/0) (0/0 :: Double)
-- True
sameFloat :: RealFloat a => a -> a -> Bool
sameFloat x y | isNaN x && isNaN y = True
              | x == 0 && y == 0 = isNegativeZero x == isNegativeZero y
              | otherwise = x == y

sameFloatP :: (RealFloat a, Show a) => a -> a -> Property
sameFloatP x y = counterexample (showHFloat x . showString (interpret res) . showHFloat y $ "") res
  where
    res = sameFloat x y
    interpret True = " === "
    interpret False = " =/= "

infix 4 `sameFloat`, `sameFloatP`

