-- This file was generated by etc/gen-ffi-wrapper.sh
-- DO NOT EDIT this file directly!
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
module FFIWrapper.Float
  ( roundedAdd
  , roundedSub
  , roundedMul
  , roundedDiv
  , roundedSqrt
  , roundedFMA
  , roundedFMAIfFast
  , roundedFromInt64
  , roundedFromWord64
  , roundedSumPtr
  , roundedSumByteArray
  , intervalMul_down
  , intervalMul_up
  , intervalDiv_down
  , intervalDiv_up
  , intervalMulAdd_down
  , intervalMulAdd_up
  , intervalDivAdd_down
  , intervalDivAdd_up
  ) where
import Data.Int (Int64)
import Data.Word (Word64)
import Foreign.Ptr (Ptr)
import GHC.Exts (ByteArray#)
import Numeric.Rounded.Hardware.Internal.Rounding (RoundingMode(..))

foreign import ccall unsafe "rounded_hw_add_float"
  c_rounded_add :: Int -> Float -> Float -> Float
foreign import ccall unsafe "rounded_hw_add_float_up"
  c_rounded_add_up :: Float -> Float -> Float
foreign import ccall unsafe "rounded_hw_add_float_down"
  c_rounded_add_down :: Float -> Float -> Float
foreign import ccall unsafe "rounded_hw_add_float_zero"
  c_rounded_add_zero :: Float -> Float -> Float

roundedAdd :: RoundingMode -> Float -> Float -> Float
roundedAdd r = c_rounded_add (fromEnum r)
{-# INLINE [1] roundedAdd #-}
{-# RULES
"roundedAdd/TowardNegInf" [~1] roundedAdd TowardNegInf = c_rounded_add_down
"roundedAdd/TowardInf" [~1] roundedAdd TowardInf = c_rounded_add_up
"roundedAdd/TowardZero" [~1] roundedAdd TowardZero = c_rounded_add_zero
  #-}

foreign import ccall unsafe "rounded_hw_sub_float"
  c_rounded_sub :: Int -> Float -> Float -> Float
foreign import ccall unsafe "rounded_hw_sub_float_up"
  c_rounded_sub_up :: Float -> Float -> Float
foreign import ccall unsafe "rounded_hw_sub_float_down"
  c_rounded_sub_down :: Float -> Float -> Float
foreign import ccall unsafe "rounded_hw_sub_float_zero"
  c_rounded_sub_zero :: Float -> Float -> Float

roundedSub :: RoundingMode -> Float -> Float -> Float
roundedSub r = c_rounded_sub (fromEnum r)
{-# INLINE [1] roundedSub #-}
{-# RULES
"roundedSub/TowardNegInf" [~1] roundedSub TowardNegInf = c_rounded_sub_down
"roundedSub/TowardInf" [~1] roundedSub TowardInf = c_rounded_sub_up
"roundedSub/TowardZero" [~1] roundedSub TowardZero = c_rounded_sub_zero
  #-}

foreign import ccall unsafe "rounded_hw_mul_float"
  c_rounded_mul :: Int -> Float -> Float -> Float
foreign import ccall unsafe "rounded_hw_mul_float_up"
  c_rounded_mul_up :: Float -> Float -> Float
foreign import ccall unsafe "rounded_hw_mul_float_down"
  c_rounded_mul_down :: Float -> Float -> Float
foreign import ccall unsafe "rounded_hw_mul_float_zero"
  c_rounded_mul_zero :: Float -> Float -> Float

roundedMul :: RoundingMode -> Float -> Float -> Float
roundedMul r = c_rounded_mul (fromEnum r)
{-# INLINE [1] roundedMul #-}
{-# RULES
"roundedMul/TowardNegInf" [~1] roundedMul TowardNegInf = c_rounded_mul_down
"roundedMul/TowardInf" [~1] roundedMul TowardInf = c_rounded_mul_up
"roundedMul/TowardZero" [~1] roundedMul TowardZero = c_rounded_mul_zero
  #-}

foreign import ccall unsafe "rounded_hw_div_float"
  c_rounded_div :: Int -> Float -> Float -> Float
foreign import ccall unsafe "rounded_hw_div_float_up"
  c_rounded_div_up :: Float -> Float -> Float
foreign import ccall unsafe "rounded_hw_div_float_down"
  c_rounded_div_down :: Float -> Float -> Float
foreign import ccall unsafe "rounded_hw_div_float_zero"
  c_rounded_div_zero :: Float -> Float -> Float

roundedDiv :: RoundingMode -> Float -> Float -> Float
roundedDiv r = c_rounded_div (fromEnum r)
{-# INLINE [1] roundedDiv #-}
{-# RULES
"roundedDiv/TowardNegInf" [~1] roundedDiv TowardNegInf = c_rounded_div_down
"roundedDiv/TowardInf" [~1] roundedDiv TowardInf = c_rounded_div_up
"roundedDiv/TowardZero" [~1] roundedDiv TowardZero = c_rounded_div_zero
  #-}

foreign import ccall unsafe "rounded_hw_sqrt_float"
  c_rounded_sqrt :: Int -> Float -> Float
foreign import ccall unsafe "rounded_hw_sqrt_float_up"
  c_rounded_sqrt_up :: Float -> Float
foreign import ccall unsafe "rounded_hw_sqrt_float_down"
  c_rounded_sqrt_down :: Float -> Float
foreign import ccall unsafe "rounded_hw_sqrt_float_zero"
  c_rounded_sqrt_zero :: Float -> Float

roundedSqrt :: RoundingMode -> Float -> Float
roundedSqrt r = c_rounded_sqrt (fromEnum r)
{-# INLINE [1] roundedSqrt #-}
{-# RULES
"roundedSqrt/TowardNegInf" [~1] roundedSqrt TowardNegInf = c_rounded_sqrt_down
"roundedSqrt/TowardInf" [~1] roundedSqrt TowardInf = c_rounded_sqrt_up
"roundedSqrt/TowardZero" [~1] roundedSqrt TowardZero = c_rounded_sqrt_zero
  #-}

foreign import ccall unsafe "rounded_hw_fma_float"
  c_rounded_fma :: Int -> Float -> Float -> Float -> Float
foreign import ccall unsafe "rounded_hw_fma_float_up"
  c_rounded_fma_up :: Float -> Float -> Float -> Float
foreign import ccall unsafe "rounded_hw_fma_float_down"
  c_rounded_fma_down :: Float -> Float -> Float -> Float
foreign import ccall unsafe "rounded_hw_fma_float_zero"
  c_rounded_fma_zero :: Float -> Float -> Float -> Float

roundedFMA :: RoundingMode -> Float -> Float -> Float -> Float
roundedFMA r = c_rounded_fma (fromEnum r)
{-# INLINE [1] roundedFMA #-}
{-# RULES
"roundedFMA/TowardNegInf" [~1] roundedFMA TowardNegInf = c_rounded_fma_down
"roundedFMA/TowardInf" [~1] roundedFMA TowardInf = c_rounded_fma_up
"roundedFMA/TowardZero" [~1] roundedFMA TowardZero = c_rounded_fma_zero
  #-}

foreign import ccall unsafe "rounded_hw_fma_if_fast_float"
  c_rounded_fma_if_fast :: Int -> Float -> Float -> Float -> Float
foreign import ccall unsafe "rounded_hw_fma_if_fast_float_up"
  c_rounded_fma_if_fast_up :: Float -> Float -> Float -> Float
foreign import ccall unsafe "rounded_hw_fma_if_fast_float_down"
  c_rounded_fma_if_fast_down :: Float -> Float -> Float -> Float
foreign import ccall unsafe "rounded_hw_fma_if_fast_float_zero"
  c_rounded_fma_if_fast_zero :: Float -> Float -> Float -> Float

roundedFMAIfFast :: RoundingMode -> Float -> Float -> Float -> Float
roundedFMAIfFast r = c_rounded_fma_if_fast (fromEnum r)
{-# INLINE [1] roundedFMAIfFast #-}
{-# RULES
"roundedFMAIfFast/TowardNegInf" [~1] roundedFMAIfFast TowardNegInf = c_rounded_fma_if_fast_down
"roundedFMAIfFast/TowardInf" [~1] roundedFMAIfFast TowardInf = c_rounded_fma_if_fast_up
"roundedFMAIfFast/TowardZero" [~1] roundedFMAIfFast TowardZero = c_rounded_fma_if_fast_zero
  #-}

foreign import ccall unsafe "rounded_hw_int64_to_float"
  c_rounded_from_int64 :: Int -> Int64 -> Float
foreign import ccall unsafe "rounded_hw_int64_to_float_up"
  c_rounded_from_int64_up :: Int64 -> Float
foreign import ccall unsafe "rounded_hw_int64_to_float_down"
  c_rounded_from_int64_down :: Int64 -> Float
foreign import ccall unsafe "rounded_hw_int64_to_float_zero"
  c_rounded_from_int64_zero :: Int64 -> Float

roundedFromInt64 :: RoundingMode -> Int64 -> Float
roundedFromInt64 r = c_rounded_from_int64 (fromEnum r)
{-# INLINE [1] roundedFromInt64 #-}
{-# RULES
"roundedFromInt64/TowardNegInf" [~1] roundedFromInt64 TowardNegInf = c_rounded_from_int64_down
"roundedFromInt64/TowardInf" [~1] roundedFromInt64 TowardInf = c_rounded_from_int64_up
"roundedFromInt64/TowardZero" [~1] roundedFromInt64 TowardZero = c_rounded_from_int64_zero
  #-}

foreign import ccall unsafe "rounded_hw_word64_to_float"
  c_rounded_from_word64 :: Int -> Word64 -> Float
foreign import ccall unsafe "rounded_hw_word64_to_float_up"
  c_rounded_from_word64_up :: Word64 -> Float
foreign import ccall unsafe "rounded_hw_word64_to_float_down"
  c_rounded_from_word64_down :: Word64 -> Float
foreign import ccall unsafe "rounded_hw_word64_to_float_zero"
  c_rounded_from_word64_zero :: Word64 -> Float

roundedFromWord64 :: RoundingMode -> Word64 -> Float
roundedFromWord64 r = c_rounded_from_word64 (fromEnum r)
{-# INLINE [1] roundedFromWord64 #-}
{-# RULES
"roundedFromWord64/TowardNegInf" [~1] roundedFromWord64 TowardNegInf = c_rounded_from_word64_down
"roundedFromWord64/TowardInf" [~1] roundedFromWord64 TowardInf = c_rounded_from_word64_up
"roundedFromWord64/TowardZero" [~1] roundedFromWord64 TowardZero = c_rounded_from_word64_zero
  #-}

foreign import ccall unsafe "rounded_hw_sum_float"
  c_rounded_sum_ptr :: Int -> Int -> Int -> Ptr Float -> IO Float
foreign import ccall unsafe "rounded_hw_sum_float_up"
  c_rounded_sum_ptr_up :: Int -> Int -> Ptr Float -> IO Float
foreign import ccall unsafe "rounded_hw_sum_float_down"
  c_rounded_sum_ptr_down :: Int -> Int -> Ptr Float -> IO Float
foreign import ccall unsafe "rounded_hw_sum_float_zero"
  c_rounded_sum_ptr_zero :: Int -> Int -> Ptr Float -> IO Float

roundedSumPtr :: RoundingMode -> Int -> Int -> Ptr Float -> IO Float
roundedSumPtr r = c_rounded_sum_ptr (fromEnum r)
{-# INLINE [1] roundedSumPtr #-}
{-# RULES
"roundedSumPtr/TowardNegInf" [~1] roundedSumPtr TowardNegInf = c_rounded_sum_ptr_down
"roundedSumPtr/TowardInf" [~1] roundedSumPtr TowardInf = c_rounded_sum_ptr_up
"roundedSumPtr/TowardZero" [~1] roundedSumPtr TowardZero = c_rounded_sum_ptr_zero
  #-}

foreign import ccall unsafe "rounded_hw_sum_float"
  c_rounded_sum_bytearr :: Int -> Int -> Int -> ByteArray# -> Float
foreign import ccall unsafe "rounded_hw_sum_float_up"
  c_rounded_sum_bytearr_up :: Int -> Int -> ByteArray# -> Float
foreign import ccall unsafe "rounded_hw_sum_float_down"
  c_rounded_sum_bytearr_down :: Int -> Int -> ByteArray# -> Float
foreign import ccall unsafe "rounded_hw_sum_float_zero"
  c_rounded_sum_bytearr_zero :: Int -> Int -> ByteArray# -> Float

roundedSumByteArray :: RoundingMode -> Int -> Int -> ByteArray# -> Float
roundedSumByteArray r = c_rounded_sum_bytearr (fromEnum r)
{-# INLINE [1] roundedSumByteArray #-}
{-# RULES
"roundedSumByteArray/TowardNegInf" [~1] roundedSumByteArray TowardNegInf = c_rounded_sum_bytearr_down
"roundedSumByteArray/TowardInf" [~1] roundedSumByteArray TowardInf = c_rounded_sum_bytearr_up
"roundedSumByteArray/TowardZero" [~1] roundedSumByteArray TowardZero = c_rounded_sum_bytearr_zero
  #-}

foreign import ccall unsafe "rounded_hw_interval_mul_float_down"
  intervalMul_down :: Float -> Float -> Float -> Float -> Float
foreign import ccall unsafe "rounded_hw_interval_mul_float_up"
  intervalMul_up :: Float -> Float -> Float -> Float -> Float

foreign import ccall unsafe "rounded_hw_interval_div_float_down"
  intervalDiv_down :: Float -> Float -> Float -> Float -> Float
foreign import ccall unsafe "rounded_hw_interval_div_float_up"
  intervalDiv_up :: Float -> Float -> Float -> Float -> Float

foreign import ccall unsafe "rounded_hw_interval_mul_add_float_down"
  intervalMulAdd_down :: Float -> Float -> Float -> Float -> Float -> Float
foreign import ccall unsafe "rounded_hw_interval_mul_add_float_up"
  intervalMulAdd_up :: Float -> Float -> Float -> Float -> Float -> Float

foreign import ccall unsafe "rounded_hw_interval_div_add_float_down"
  intervalDivAdd_down :: Float -> Float -> Float -> Float -> Float -> Float
foreign import ccall unsafe "rounded_hw_interval_div_add_float_up"
  intervalDivAdd_up :: Float -> Float -> Float -> Float -> Float -> Float
