{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
module Numeric.Rounded.Hardware.Sum where
import Numeric.Rounded.Hardware.Internal
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Unboxed.Base as VU
import Data.Primitive.ByteArray
import Data.Proxy

foreign import ccall unsafe "rounded_hw_sum"
  c_rounded_sum :: Int -> Int -> Int -> ByteArray# -> Double
foreign import ccall unsafe "rounded_hw_sum_up"
  c_rounded_sum_up :: Int -> Int -> ByteArray# -> Double
foreign import ccall unsafe "rounded_hw_sum_down"
  c_rounded_sum_down :: Int -> Int -> ByteArray# -> Double
foreign import ccall unsafe "rounded_hw_sum_zero"
  c_rounded_sum_zero :: Int -> Int -> ByteArray# -> Double

sumPrimitiveVector :: RoundingMode -> VP.Vector Double -> Double
sumPrimitiveVector mode (VP.Vector offset length (ByteArray arr#)) = c_rounded_sum (fromEnum mode) offset length arr#
{-# INLINE sumPrimitiveVector #-}

sumUnboxedVector :: RoundingMode -> VU.Vector Double -> Double
sumUnboxedVector mode (VU.V_Double primVec) = sumPrimitiveVector mode primVec
{-# INLINE sumUnboxedVector #-}

sumUnboxedVector' :: forall rn. (RoundedPrim rn) => VU.Vector (RoundedDouble rn) -> RoundedDouble rn
sumUnboxedVector' (V_RoundedDouble vec) = RoundedDouble (sumUnboxedVector (rounding (Proxy :: Proxy rn)) vec)
{-# INLINE sumUnboxedVector' #-}
