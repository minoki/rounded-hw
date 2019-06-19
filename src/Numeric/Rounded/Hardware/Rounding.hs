{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
module Numeric.Rounded.Hardware.Rounding where
import GHC.Generics (Generic)
import Control.DeepSeq (NFData(..))
import Data.Proxy
import Data.Tagged

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
  roundingT :: Tagged rn RoundingMode

instance Rounding TowardNearest where
  roundingT = Tagged TowardNearest

instance Rounding TowardInf where
  roundingT = Tagged TowardInf

instance Rounding TowardNegInf where
  roundingT = Tagged TowardNegInf

instance Rounding TowardZero where
  roundingT = Tagged TowardZero

rounding :: Rounding rn => proxy rn -> RoundingMode
rounding = Data.Tagged.proxy roundingT
{-# INLINE rounding #-}

reifyRounding :: RoundingMode -> (forall s. Rounding s => Proxy s -> r) -> r
reifyRounding TowardNearest f = f (Proxy :: Proxy TowardNearest)
reifyRounding TowardInf f     = f (Proxy :: Proxy TowardInf)
reifyRounding TowardNegInf f  = f (Proxy :: Proxy TowardNegInf)
reifyRounding TowardZero f    = f (Proxy :: Proxy TowardZero)
{-# INLINE reifyRounding #-}
