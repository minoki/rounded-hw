{-# LANGUAGE DeriveGeneric #-}
module Numeric.Rounded.Hardware.Rounding where
import GHC.Generics (Generic)
import Control.DeepSeq (NFData(..))

-- See cbits/rounded.c for the ordering
data RoundingMode
  = TowardNearest
  | TowardNegInf
  | TowardInf
  | TowardZero
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Generic)

instance NFData RoundingMode

oppositeRoundingMode :: RoundingMode -> RoundingMode
oppositeRoundingMode TowardNearest = TowardNearest
oppositeRoundingMode TowardZero = TowardZero
oppositeRoundingMode TowardInf = TowardNegInf
oppositeRoundingMode TowardNegInf = TowardInf
