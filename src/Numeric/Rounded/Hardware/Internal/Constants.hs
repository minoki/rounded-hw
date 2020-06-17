{-# LANGUAGE DataKinds #-}
{-# LANGUAGE HexFloatLiterals #-}
module Numeric.Rounded.Hardware.Internal.Constants where
import Numeric.Rounded.Hardware.Internal.Rounding

class RealFloatConstants a where
  -- | \(+\infty\)
  positiveInfinity :: a
  -- | \(-\infty\)
  negativeInfinity :: a
  maxFinite :: a
  -- minPositiveNormal :: a
  minPositive :: a

  -- | The correctly-rounded value of \(\pi\)
  pi_down :: Rounded 'TowardNegInf a
  -- | The correctly-rounded value of \(\pi\)
  pi_up :: Rounded 'TowardInf a

  -- | The correctly-rounded value of \(3\pi\)
  three_pi_down :: Rounded 'TowardNegInf a
  -- | The correctly-rounded value of \(3\pi\)
  three_pi_up :: Rounded 'TowardInf a

  -- | The correctly-rounded value of \(5\pi\)
  five_pi_down :: Rounded 'TowardNegInf a
  -- | The correctly-rounded value of \(5\pi\)
  five_pi_up :: Rounded 'TowardInf a

  -- | The correctly-rounded value of \(\log_e 2\)
  log2_down :: Rounded 'TowardNegInf a
  -- | The correctly-rounded value of \(\log_e 2\)
  log2_up :: Rounded 'TowardInf a

  -- | The correctly-rounded value of \(\exp(1)\)
  exp1_down :: Rounded 'TowardNegInf a
  -- | The correctly-rounded value of \(\exp(1)\)
  exp1_up :: Rounded 'TowardInf a

  -- | The correctly-rounded value of \(\exp(1/2)\)
  exp1_2_down :: Rounded 'TowardNegInf a
  -- | The correctly-rounded value of \(\exp(1/2)\)
  exp1_2_up :: Rounded 'TowardInf a

  -- | The correctly-rounded value of \(\exp(-1/2)\)
  expm1_2_down :: Rounded 'TowardNegInf a
  -- | The correctly-rounded value of \(\exp(-1/2)\)
  expm1_2_up :: Rounded 'TowardInf a

  -- | The correctly-rounded value of \(\sqrt{2}\)
  sqrt2_down :: Rounded 'TowardNegInf a
  -- | The correctly-rounded value of \(\sqrt{2}\)
  sqrt2_up :: Rounded 'TowardInf a

  -- | The correctly-rounded value of \(\sqrt{2}-1\)
  sqrt2m1_down :: Rounded 'TowardNegInf a
  -- | The correctly-rounded value of \(\sqrt{2}-1\)
  sqrt2m1_up :: Rounded 'TowardInf a

  -- | The correctly-rounded value of \(1/\sqrt{2}\)
  sqrt1_2_down :: Rounded 'TowardNegInf a
  -- | The correctly-rounded value of \(1/\sqrt{2}\)
  sqrt1_2_up :: Rounded 'TowardInf a

  -- | The correctly-rounded value of \(3-2\sqrt{2}\)
  three_minus_2sqrt2_down :: Rounded 'TowardNegInf a
  -- | The correctly-rounded value of \(3-2\sqrt{2}\)
  three_minus_2sqrt2_up :: Rounded 'TowardInf a

  -- | The correctly-rounded value of \(2-\sqrt{2}\)
  two_minus_sqrt2_down :: Rounded 'TowardNegInf a
  -- | The correctly-rounded value of \(2-\sqrt{2}\)
  two_minus_sqrt2_up :: Rounded 'TowardInf a

instance RealFloatConstants Double where
  positiveInfinity = 1/0
  negativeInfinity = -1/0
  maxFinite = 0x1.fffffffffffffp+1023
  -- minPositiveNormal = 0x1p-1022
  minPositive = 0x1p-1074 -- subnormal
  -- (pi :: Double) == 0x1.921fb54442d18p1
  pi_down = Rounded 0x1.921fb54442d18p+1
  pi_up   = Rounded 0x1.921fb54442d19p+1
  -- 3*pi
  three_pi_down = Rounded 0x1.2d97c7f3321d2p+3
  three_pi_up   = Rounded 0x1.2d97c7f3321d3p+3
  -- 5*pi
  five_pi_down = Rounded 0x1.f6a7a2955385ep+3
  five_pi_up   = Rounded 0x1.f6a7a2955385fp+3
  -- log(2)
  log2_down = Rounded 0x1.62e42fefa39efp-1
  log2_up   = Rounded 0x1.62e42fefa39f0p-1
  -- exp(1)
  exp1_down = Rounded 0x1.5bf0a8b145769p+1
  exp1_up   = Rounded 0x1.5bf0a8b14576ap+1
  -- exp(1/2)
  exp1_2_down = Rounded 0x1.a61298e1e069bp+0
  exp1_2_up   = Rounded 0x1.a61298e1e069cp+0
  -- exp(-1/2)
  expm1_2_down = Rounded 0x1.368b2fc6f9609p-1
  expm1_2_up   = Rounded 0x1.368b2fc6f960ap-1
  -- sqrt(2)
  sqrt2_down = Rounded 0x1.6a09e667f3bccp+0
  sqrt2_up   = Rounded 0x1.6a09e667f3bcdp+0
  -- sqrt(1/2)
  sqrt1_2_down = Rounded 0x1.6a09e667f3bccp-1
  sqrt1_2_up   = Rounded 0x1.6a09e667f3bcdp-1
  -- sqrt(2)-1
  sqrt2m1_down = Rounded 0x1.a827999fcef32p-2
  sqrt2m1_up   = Rounded 0x1.a827999fcef33p-2
  -- 3 - 2 * sqrt(2)
  three_minus_2sqrt2_down = Rounded 0x1.5f619980c4336p-3
  three_minus_2sqrt2_up   = Rounded 0x1.5f619980c4337p-3
  -- 2 - sqrt(2)
  two_minus_sqrt2_down = Rounded 0x1.2bec333018866p-1
  two_minus_sqrt2_up   = Rounded 0x1.2bec333018867p-1
  {-# INLINE positiveInfinity #-}
  {-# INLINE negativeInfinity #-}
  {-# INLINE maxFinite #-}
  {-# INLINE minPositive #-}

instance RealFloatConstants Float where
  positiveInfinity = 1/0
  negativeInfinity = -1/0
  maxFinite = 0x1.fffffep+127
  minPositive = 0x1p-149
  pi_down = Rounded 0x1.921fb4p+1
  pi_up   = Rounded 0x1.921fb6p+1
  -- 3*pi
  three_pi_down = Rounded 0x1.2d97c6p+3
  three_pi_up   = Rounded 0x1.2d97c8p+3
  -- 5*pi
  five_pi_down = Rounded 0x1.f6a7a2p+3
  five_pi_up   = Rounded 0x1.f6a7a4p+3
  -- log(2)
  log2_down = Rounded 0x1.62e42ep-1
  log2_up   = Rounded 0x1.62e430p-1
  -- exp(1)
  exp1_down = Rounded 0x1.5bf0a8p+1
  exp1_up   = Rounded 0x1.5bf0aap+1
  -- exp(1/2)
  exp1_2_down = Rounded 0x1.a61298p+0
  exp1_2_up   = Rounded 0x1.a6129ap+0
  -- exp(-1/2)
  expm1_2_down = Rounded 0x1.368b2ep-1
  expm1_2_up   = Rounded 0x1.368b30p-1
  -- sqrt(2)
  sqrt2_down = Rounded 0x1.6a09e6p+0
  sqrt2_up   = Rounded 0x1.6a09e8p+0
  -- sqrt(1/2)
  sqrt1_2_down = Rounded 0x1.6a09e6p-1
  sqrt1_2_up   = Rounded 0x1.6a09e8p-1
  -- sqrt(2)-1
  sqrt2m1_down = Rounded 0x1.a82798p-2
  sqrt2m1_up   = Rounded 0x1.a8279ap-2
  -- 3 - 2 * sqrt(2)
  three_minus_2sqrt2_down = Rounded 0x1.5f6198p-3
  three_minus_2sqrt2_up   = Rounded 0x1.5f619ap-3
  -- 2 - sqrt(2)
  two_minus_sqrt2_down = Rounded 0x1.2bec32p-1
  two_minus_sqrt2_up   = Rounded 0x1.2bec34p-1
  {-# INLINE positiveInfinity #-}
  {-# INLINE negativeInfinity #-}
  {-# INLINE maxFinite #-}
  {-# INLINE minPositive #-}
