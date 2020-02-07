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
  -- pi_up      = 0x1.921fb54442d19p1
  -- pi_down    = 0x1.921fb54442d18p1
  -- (pi :: Double) == 0x1.921fb54442d18p1
  pi_down = Rounded 0x3.243f6a8885a3p+0
  pi_up   = Rounded 0x3.243f6a8885a32p+0
  -- 3*pi
  three_pi_down = Rounded 0x9.6cbe3f9990e9p+0
  three_pi_up   = Rounded 0x9.6cbe3f9990e98p+0
  -- 5*pi
  five_pi_down = Rounded 0xf.b53d14aa9c2fp+0
  five_pi_up   = Rounded 0xf.b53d14aa9c2f8p+0
  -- log(2)
  log2_down = Rounded 0xb.17217f7d1cf78p-4
  log2_up   = Rounded 0xb.17217f7d1cf8p-4
  -- exp(1)
  exp1_down = Rounded 0x2.b7e151628aed2p+0
  exp1_up   = Rounded 0x2.b7e151628aed4p+0
  -- exp(1/2)
  exp1_2_down = Rounded 0x1.a61298e1e069bp+0
  exp1_2_up   = Rounded 0x1.a61298e1e069cp+0
  -- exp(-1/2)
  expm1_2_down = Rounded 0x9.b4597e37cb048p-4
  expm1_2_up   = Rounded 0x9.b4597e37cb05p-4
  -- sqrt(2)
  sqrt2_down = Rounded 0x1.6a09e667f3bccp+0
  sqrt2_up   = Rounded 0x1.6a09e667f3bcdp+0
  -- sqrt(1/2)
  sqrt1_2_down = Rounded 0xb.504f333f9de6p-4
  sqrt1_2_up   = Rounded 0xb.504f333f9de68p-4
  -- sqrt(2)-1
  sqrt2m1_down = Rounded 0x6.a09e667f3bcc8p-4
  sqrt2m1_up   = Rounded 0x6.a09e667f3bcccp-4
  -- 3 - 2 * sqrt(2)
  three_minus_2sqrt2_down = Rounded 0x2.bec333018866cp-4
  three_minus_2sqrt2_up   = Rounded 0x2.bec333018866ep-4
  -- 2 - sqrt(2)
  two_minus_sqrt2_down = Rounded 0x9.5f619980c433p-4
  two_minus_sqrt2_up   = Rounded 0x9.5f619980c4338p-4
  {-# INLINE positiveInfinity #-}
  {-# INLINE negativeInfinity #-}
  {-# INLINE maxFinite #-}
  {-# INLINE minPositive #-}

instance RealFloatConstants Float where
  positiveInfinity = 1/0
  negativeInfinity = -1/0
  maxFinite = 0x1.fffffep+127
  minPositive = 0x1p-149
  pi_down = Rounded 0x3.243f68p+0
  pi_up   = Rounded 0x3.243f6cp+0
  -- 3*pi
  three_pi_down = Rounded 0x9.6cbe3p+0
  three_pi_up   = Rounded 0x9.6cbe4p+0
  -- 5*pi
  five_pi_down = Rounded 0xf.b53d1p+0
  five_pi_up   = Rounded 0xf.b53d2p+0
  -- log(2)
  log2_down = Rounded 0xb.17217p-4
  log2_up   = Rounded 0xb.17218p-4
  -- exp(1)
  exp1_down = Rounded 0x2.b7e15p+0
  exp1_up   = Rounded 0x2.b7e154p+0
  -- exp(1/2)
  exp1_2_down = Rounded 0x1.a61298p+0
  exp1_2_up   = Rounded 0x1.a6129ap+0
  -- exp(-1/2)
  expm1_2_down = Rounded 0x9.b4597p-4
  expm1_2_up   = Rounded 0x9.b4598p-4
  -- sqrt(2)
  sqrt2_down = Rounded 0x1.6a09e6p+0
  sqrt2_up   = Rounded 0x1.6a09e8p+0
  -- sqrt(1/2)
  sqrt1_2_down = Rounded 0xb.504f3p-4
  sqrt1_2_up   = Rounded 0xb.504f4p-4
  -- sqrt(2)-1
  sqrt2m1_down = Rounded 0x6.a09e6p-4
  sqrt2m1_up   = Rounded 0x6.a09e68p-4
  -- 3 - 2 * sqrt(2)
  three_minus_2sqrt2_down = Rounded 0x2.bec33p-4
  three_minus_2sqrt2_up   = Rounded 0x2.bec334p-4
  -- 2 - sqrt(2)
  two_minus_sqrt2_down = Rounded 0x9.5f619p-4
  two_minus_sqrt2_up   = Rounded 0x9.5f61ap-4
  {-# INLINE positiveInfinity #-}
  {-# INLINE negativeInfinity #-}
  {-# INLINE maxFinite #-}
  {-# INLINE minPositive #-}
