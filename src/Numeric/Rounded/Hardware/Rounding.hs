{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
module Numeric.Rounded.Hardware.Rounding where
import GHC.Generics (Generic)
import Control.DeepSeq (NFData(..))
import Data.Proxy

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

class Rounding (rn :: RoundingMode) where
  rounding :: proxy rn -> RoundingMode

instance Rounding TowardNearest where
  rounding _ = TowardNearest

instance Rounding TowardInf where
  rounding _ = TowardInf

instance Rounding TowardNegInf where
  rounding _ = TowardNegInf

instance Rounding TowardZero where
  rounding _ = TowardZero

reifyRounding :: RoundingMode -> (forall s. Rounding s => Proxy s -> r) -> r
reifyRounding TowardNearest f = f (Proxy :: Proxy TowardNearest)
reifyRounding TowardInf f     = f (Proxy :: Proxy TowardInf)
reifyRounding TowardNegInf f  = f (Proxy :: Proxy TowardNegInf)
reifyRounding TowardZero f    = f (Proxy :: Proxy TowardZero)
{-# INLINE reifyRounding #-}
