{-# LANGUAGE HexFloatLiterals #-}
module Numeric.Rounded.Hardware.Util.Constants where

class RealFloatConstants a where
  positiveInfinity :: a
  negativeInfinity :: a
  maxFinite :: a
  -- minPositiveNormal :: a
  -- minPositiveSubnormal :: a
  -- pi_up, pi_down

instance RealFloatConstants Double where
  positiveInfinity = 1/0
  negativeInfinity = -1/0
  maxFinite = 0x1.fffffffffffffp+1023
  -- minPositiveNormal = 0x1p-1022
  -- minPositiveSubnormal = 0x1p-1074
  -- pi_up      = 0x1.921fb54442d19p1
  -- pi_down    = 0x1.921fb54442d18p1
  -- (pi :: Double) == 0x1.921fb54442d18p1
  {-# INLINE positiveInfinity #-}
  {-# INLINE negativeInfinity #-}
  {-# INLINE maxFinite #-}

instance RealFloatConstants Float where
  positiveInfinity = 1/0
  negativeInfinity = -1/0
  maxFinite = 0x1.fffffep+127
  {-# INLINE positiveInfinity #-}
  {-# INLINE negativeInfinity #-}
  {-# INLINE maxFinite #-}
