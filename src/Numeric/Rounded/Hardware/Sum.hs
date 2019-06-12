{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE DataKinds #-}
module Numeric.Rounded.Hardware.Sum where
import Numeric.Rounded.Hardware.Internal
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Unboxed.Base as VU
import Data.Primitive.ByteArray

foreign import ccall unsafe "rounded_hw_sum_up"
  c_rounded_sum_up :: Int -> Int -> ByteArray# -> Double

foreign import ccall unsafe "rounded_hw_sum_down"
  c_rounded_sum_down :: Int -> Int -> ByteArray# -> Double

sumPrimitiveVectorUp :: VP.Vector Double -> Double
sumPrimitiveVectorUp (VP.Vector offset length (ByteArray arr#)) = c_rounded_sum_up offset length arr#
{-# INLINE sumPrimitiveVectorUp #-}

sumPrimitiveVectorDown :: VP.Vector Double -> Double
sumPrimitiveVectorDown (VP.Vector offset length (ByteArray arr#)) = c_rounded_sum_down offset length arr#

sumUnboxedVectorUp :: VU.Vector Double -> Double
sumUnboxedVectorUp (VU.V_Double primVec) = sumPrimitiveVectorUp primVec
{-# INLINE sumUnboxedVectorUp #-}

sumUnboxedVectorDown :: VU.Vector Double -> Double
sumUnboxedVectorDown (VU.V_Double primVec) = sumPrimitiveVectorDown primVec

sumUnboxedVectorUp' :: VU.Vector (RoundedDouble TowardInf) -> RoundedDouble TowardInf
sumUnboxedVectorUp' (V_RoundedDouble vec) = RoundedDouble (sumUnboxedVectorUp vec)
{-# INLINE sumUnboxedVectorUp' #-}

sumUnboxedVectorDown' :: VU.Vector (RoundedDouble TowardNegInf) -> RoundedDouble TowardNegInf
sumUnboxedVectorDown' (V_RoundedDouble vec) = RoundedDouble (sumUnboxedVectorDown vec)
