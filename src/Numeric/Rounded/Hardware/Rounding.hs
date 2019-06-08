module Numeric.Rounded.Hardware.Rounding where

-- TODO: Use rounded package (Numeric.Rounded)
data RoundingMode
  = TowardNearest
  | TowardZero
  | TowardInf
  | TowardNegInf
  deriving (Eq, Ord, Read, Show, Bounded)

oppositeRoundingMode :: RoundingMode -> RoundingMode
oppositeRoundingMode TowardNearest = TowardNearest
oppositeRoundingMode TowardZero = TowardZero
oppositeRoundingMode TowardInf = TowardNegInf
oppositeRoundingMode TowardNegInf = TowardInf
