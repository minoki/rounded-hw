{-# LANGUAGE HexFloatLiterals #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Numeric.Rounded.Hardware.Backend.Float128
  (
  ) where
import           Data.Ratio
import           Data.Tagged
import           Foreign.C.String (CString, peekCString)
import           Foreign.Marshal (alloca, with)
import           Foreign.Ptr (Ptr)
import           Foreign.Storable (peek)
import           Numeric.Float128 (Float128)
import           Numeric.Rounded.Hardware.Internal.Class
import           Numeric.Rounded.Hardware.Internal.Constants
import           Numeric.Rounded.Hardware.Internal.Conversion
import           System.IO.Unsafe

foreign import ccall unsafe "rounded_hw_add_float128"
  c_rounded_add_float128 :: Int -> Ptr Float128 -> Ptr Float128 -> Ptr Float128 -> IO ()
foreign import ccall unsafe "rounded_hw_sub_float128"
  c_rounded_sub_float128 :: Int -> Ptr Float128 -> Ptr Float128 -> Ptr Float128 -> IO ()
foreign import ccall unsafe "rounded_hw_mul_float128"
  c_rounded_mul_float128 :: Int -> Ptr Float128 -> Ptr Float128 -> Ptr Float128 -> IO ()
foreign import ccall unsafe "rounded_hw_div_float128"
  c_rounded_div_float128 :: Int -> Ptr Float128 -> Ptr Float128 -> Ptr Float128 -> IO ()
foreign import ccall unsafe "rounded_hw_sqrt_float128"
  c_rounded_sqrt_float128 :: Int -> Ptr Float128 -> Ptr Float128 -> IO ()
foreign import ccall unsafe "rounded_hw_fma_float128"
  c_rounded_fma_float128 :: Int -> Ptr Float128 -> Ptr Float128 -> Ptr Float128 -> Ptr Float128 -> IO ()

roundedAdd_f128 :: RoundingMode -> Float128 -> Float128 -> Float128
roundedAdd_f128 mode x y = unsafePerformIO $
  with x $ \xPtr ->
  with y $ \yPtr ->
  alloca $ \resultPtr -> do
  c_rounded_add_float128 (fromEnum mode) resultPtr xPtr yPtr
  peek resultPtr

roundedSub_f128 :: RoundingMode -> Float128 -> Float128 -> Float128
roundedSub_f128 mode x y = unsafePerformIO $
  with x $ \xPtr ->
  with y $ \yPtr ->
  alloca $ \resultPtr -> do
  c_rounded_sub_float128 (fromEnum mode) resultPtr xPtr yPtr
  peek resultPtr

roundedMul_f128 :: RoundingMode -> Float128 -> Float128 -> Float128
roundedMul_f128 mode x y = unsafePerformIO $
  with x $ \xPtr ->
  with y $ \yPtr ->
  alloca $ \resultPtr -> do
  c_rounded_mul_float128 (fromEnum mode) resultPtr xPtr yPtr
  peek resultPtr

roundedDiv_f128 :: RoundingMode -> Float128 -> Float128 -> Float128
roundedDiv_f128 mode x y = unsafePerformIO $
  with x $ \xPtr ->
  with y $ \yPtr ->
  alloca $ \resultPtr -> do
  c_rounded_div_float128 (fromEnum mode) resultPtr xPtr yPtr
  peek resultPtr

roundedSqrt_f128 :: RoundingMode -> Float128 -> Float128
roundedSqrt_f128 mode x = unsafePerformIO $
  with x $ \xPtr ->
  alloca $ \resultPtr -> do
  c_rounded_sqrt_float128 (fromEnum mode) resultPtr xPtr
  peek resultPtr

roundedFMA_f128 :: RoundingMode -> Float128 -> Float128 -> Float128 -> Float128
roundedFMA_f128 mode x y z = unsafePerformIO $
  with x $ \xPtr ->
  with y $ \yPtr ->
  with z $ \zPtr ->
  alloca $ \resultPtr -> do
  c_rounded_fma_float128 (fromEnum mode) resultPtr xPtr yPtr zPtr
  peek resultPtr

instance RealFloatConstants Float128 where
  positiveInfinity = 1/0
  negativeInfinity = -1/0
  -- 113 bits = (1 + 4 * 28) bits = (1 + 4 * 4 * 7) bits
  maxFinite = 0x1.ffff_ffff_ffff_ffff_ffff_ffff_ffffp+16383
  minPositive = encodeFloat 1 (-16494) -- The literal 0x1p-16494 may yield 0 on float128-0.1
  -- minPositiveNormal = encodeFloat 1 (-16382) -- emin = 1 - emax = 1 - 16383
  pi_down = Rounded 0x1.921fb54442d18469898cc51701b8p+1
  pi_up   = Rounded 0x1.921fb54442d18469898cc51701b9p+1
  -- 3*pi
  three_pi_down = Rounded 0x1.2d97c7f3321d234f272993d1414ap+3
  three_pi_up   = Rounded 0x1.2d97c7f3321d234f272993d1414bp+3
  -- 5*pi
  five_pi_down = Rounded 0x1.f6a7a2955385e583ebeff65cc226p+3
  five_pi_up   = Rounded 0x1.f6a7a2955385e583ebeff65cc227p+3
  -- log(2)
  log2_down = Rounded 0x1.62e42fefa39ef35793c7673007e5p-1
  log2_up   = Rounded 0x1.62e42fefa39ef35793c7673007e6p-1
  -- exp(1)
  exp1_down = Rounded 0x1.5bf0a8b1457695355fb8ac404e7ap+1
  exp1_up   = Rounded 0x1.5bf0a8b1457695355fb8ac404e7bp+1
  -- exp(1/2)
  exp1_2_down = Rounded 0x1.a61298e1e069bc972dfefab6df33p+0
  exp1_2_up   = Rounded 0x1.a61298e1e069bc972dfefab6df34p+0
  -- exp(-1/2)
  expm1_2_down = Rounded 0x1.368b2fc6f9609fe7aceb46aa619bp-1
  expm1_2_up   = Rounded 0x1.368b2fc6f9609fe7aceb46aa619cp-1
  -- sqrt(2)
  sqrt2_down = Rounded 0x1.6a09e667f3bcc908b2fb1366ea95p+0
  sqrt2_up   = Rounded 0x1.6a09e667f3bcc908b2fb1366ea96p+0
  -- sqrt(1/2)
  sqrt1_2_down = Rounded 0x1.6a09e667f3bcc908b2fb1366ea95p-1
  sqrt1_2_up   = Rounded 0x1.6a09e667f3bcc908b2fb1366ea96p-1
  -- sqrt(2)-1
  sqrt2m1_down = Rounded 0x1.a827999fcef32422cbec4d9baa55p-2
  sqrt2m1_up   = Rounded 0x1.a827999fcef32422cbec4d9baa56p-2
  -- 3 - 2 * sqrt(2)
  three_minus_2sqrt2_down = Rounded 0x1.5f619980c4336f74d04ec99156a8p-3
  three_minus_2sqrt2_up   = Rounded 0x1.5f619980c4336f74d04ec99156a9p-3
  -- 2 - sqrt(2)
  two_minus_sqrt2_down = Rounded 0x1.2bec333018866dee9a09d9322ad5p-1
  two_minus_sqrt2_up   = Rounded 0x1.2bec333018866dee9a09d9322ad6p-1

instance RoundedRing Float128 where
  roundedAdd = roundedAdd_f128
  roundedSub = roundedSub_f128
  roundedMul = roundedMul_f128
  roundedFusedMultiplyAdd = roundedFMA_f128
  roundedFromInteger = fromInt
  intervalFromInteger = intervalFromInteger_default
  backendNameT = Tagged cBackendName
  {-# INLINE roundedAdd #-}
  {-# INLINE roundedSub #-}
  {-# INLINE roundedMul #-}
  {-# INLINE roundedFusedMultiplyAdd #-}
  {-# INLINE roundedFromInteger #-}
  {-# INLINE intervalFromInteger #-}

instance RoundedFractional Float128 where
  roundedDiv = roundedDiv_f128
  roundedFromRational r x = fromRatio r (numerator x) (denominator x)
  intervalFromRational = intervalFromRational_default
  {-# INLINE roundedDiv #-}
  {-# INLINE roundedFromRational #-}
  {-# INLINE intervalFromRational #-}

instance RoundedSqrt Float128 where
  roundedSqrt = roundedSqrt_f128
  {-# INLINE roundedSqrt #-}

--
-- Backend name
--

foreign import ccall unsafe "rounded_hw_backend_name_float128"
  c_backend_name :: CString

cBackendName :: String
cBackendName = unsafePerformIO (peekCString c_backend_name)
