{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
module Numeric.Rounded.Hardware.Sum where
import Numeric.Rounded.Hardware.Internal
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Unboxed.Base as VU
import Data.Primitive.ByteArray
import Data.Proxy
import qualified FFIWrapper.Double as D

sumPrimitiveVector :: RoundingMode -> VP.Vector Double -> Double
sumPrimitiveVector mode (VP.Vector offset length (ByteArray arr#)) = D.roundedSumByteArray mode offset length arr#
{-# INLINE sumPrimitiveVector #-}

sumUnboxedVector :: RoundingMode -> VU.Vector Double -> Double
sumUnboxedVector mode (VU.V_Double primVec) = sumPrimitiveVector mode primVec
{-# INLINE sumUnboxedVector #-}

sumUnboxedVector' :: forall rn. (Rounding rn) => VU.Vector (RoundedDouble rn) -> RoundedDouble rn
sumUnboxedVector' (V_RoundedDouble vec) = RoundedDouble (sumUnboxedVector (rounding (Proxy :: Proxy rn)) vec)
{-# INLINE sumUnboxedVector' #-}
