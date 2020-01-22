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
  -- sqrt(2)-1
  sqrt2m1_down = Rounded 0x6.a09e667f3bcc8p-4
  sqrt2m1_up   = Rounded 0x6.a09e667f3bcccp-4
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
  -- sqrt(2)-1
  sqrt2m1_down = Rounded 0x6.a09e6p-4
  sqrt2m1_up   = Rounded 0x6.a09e68p-4
  {-# INLINE positiveInfinity #-}
  {-# INLINE negativeInfinity #-}
  {-# INLINE maxFinite #-}
  {-# INLINE minPositive #-}
