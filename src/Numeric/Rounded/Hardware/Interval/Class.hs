{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Numeric.Rounded.Hardware.Interval.Class where
import Numeric.Rounded.Hardware.Internal

infix 4 `equalAsSet`, `subset`, `weaklyLess`, `precedes`, `interior`, `strictLess`, `strictPrecedes`, `disjoint`

class IsInterval i where
  type EndPoint i
  withEndPoints :: (Rounded 'TowardNegInf (EndPoint i) -> Rounded 'TowardInf (EndPoint i) -> i) -> i -> i
  singleton :: EndPoint i -> i
  makeInterval :: Rounded 'TowardNegInf (EndPoint i) -> Rounded 'TowardInf (EndPoint i) -> i
  width :: i -> Rounded 'TowardInf (EndPoint i)
  hull :: i -> i -> i
  intersection :: i -> i -> i
  maybeIntersection :: i -> i -> Maybe i

  equalAsSet     :: i -> i -> Bool
  -- | @a@ is a subset of @b@
  subset         :: i -- ^ @a@
                 -> i -- ^ @b@
                 -> Bool
  weaklyLess     :: i -> i -> Bool
  precedes       :: i -> i -> Bool
  interior       :: i -> i -> Bool
  strictLess     :: i -> i -> Bool
  strictPrecedes :: i -> i -> Bool
  disjoint       :: i -> i -> Bool

  -- default definition
  singleton x = makeInterval (Rounded x) (Rounded x)
