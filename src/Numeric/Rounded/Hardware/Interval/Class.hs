{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Numeric.Rounded.Hardware.Interval.Class where
import Numeric.Rounded.Hardware.Internal

class IsInterval i where
  type EndPoint i
  withEndPoints :: (Rounded 'TowardNegInf (EndPoint i) -> Rounded 'TowardInf (EndPoint i) -> i) -> i -> i
  singleton :: EndPoint i -> i
  makeInterval :: Rounded 'TowardNegInf (EndPoint i) -> Rounded 'TowardInf (EndPoint i) -> i
  width :: i -> Rounded 'TowardInf (EndPoint i)
  hull :: i -> i -> i

  -- default definition
  singleton x = makeInterval (Rounded x) (Rounded x)
