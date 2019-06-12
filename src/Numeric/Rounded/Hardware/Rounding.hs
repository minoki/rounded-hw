{-# LANGUAGE DeriveGeneric #-}
module Numeric.Rounded.Hardware.Rounding where
import GHC.Generics (Generic)
import Control.DeepSeq (NFData(..))

-- TODO: Use rounded package (Numeric.Rounded)
data RoundingMode
  = TowardNearest
  | TowardZero
  | TowardInf
  | TowardNegInf
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Generic)

instance NFData RoundingMode

oppositeRoundingMode :: RoundingMode -> RoundingMode
oppositeRoundingMode TowardNearest = TowardNearest
oppositeRoundingMode TowardZero = TowardZero
oppositeRoundingMode TowardInf = TowardNegInf
oppositeRoundingMode TowardNegInf = TowardInf
