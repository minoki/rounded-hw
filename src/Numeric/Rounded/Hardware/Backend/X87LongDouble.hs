{-# LANGUAGE HexFloatLiterals #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Numeric.Rounded.Hardware.Backend.X87LongDouble
  (
  ) where
import           Data.Ratio
import           Data.Tagged
import           Foreign.C.String (CString, peekCString)
import           Foreign.Marshal (alloca, with)
import           Foreign.Ptr (Ptr)
import           Foreign.Storable (peek)
import           Numeric.LongDouble (LongDouble)
import           Numeric.Rounded.Hardware.Internal.Class
import           Numeric.Rounded.Hardware.Internal.Constants
import           Numeric.Rounded.Hardware.Internal.Conversion
import           System.IO.Unsafe

foreign import ccall unsafe "rounded_hw_add_longdouble"
  c_rounded_add_longdouble :: Int -> Ptr LongDouble -> Ptr LongDouble -> Ptr LongDouble -> IO ()
foreign import ccall unsafe "rounded_hw_sub_longdouble"
  c_rounded_sub_longdouble :: Int -> Ptr LongDouble -> Ptr LongDouble -> Ptr LongDouble -> IO ()
foreign import ccall unsafe "rounded_hw_mul_longdouble"
  c_rounded_mul_longdouble :: Int -> Ptr LongDouble -> Ptr LongDouble -> Ptr LongDouble -> IO ()
foreign import ccall unsafe "rounded_hw_div_longdouble"
  c_rounded_div_longdouble :: Int -> Ptr LongDouble -> Ptr LongDouble -> Ptr LongDouble -> IO ()
foreign import ccall unsafe "rounded_hw_sqrt_longdouble"
  c_rounded_sqrt_longdouble :: Int -> Ptr LongDouble -> Ptr LongDouble -> IO ()
foreign import ccall unsafe "rounded_hw_fma_longdouble"
  c_rounded_fma_longdouble :: Int -> Ptr LongDouble -> Ptr LongDouble -> Ptr LongDouble -> Ptr LongDouble -> IO ()

roundedAdd_ld :: RoundingMode -> LongDouble -> LongDouble -> LongDouble
roundedAdd_ld mode x y = unsafePerformIO $
  with x $ \xPtr ->
  with y $ \yPtr ->
  alloca $ \resultPtr -> do
  c_rounded_add_longdouble (fromEnum mode) resultPtr xPtr yPtr
  peek resultPtr

roundedSub_ld :: RoundingMode -> LongDouble -> LongDouble -> LongDouble
roundedSub_ld mode x y = unsafePerformIO $
  with x $ \xPtr ->
  with y $ \yPtr ->
  alloca $ \resultPtr -> do
  c_rounded_sub_longdouble (fromEnum mode) resultPtr xPtr yPtr
  peek resultPtr

roundedMul_ld :: RoundingMode -> LongDouble -> LongDouble -> LongDouble
roundedMul_ld mode x y = unsafePerformIO $
  with x $ \xPtr ->
  with y $ \yPtr ->
  alloca $ \resultPtr -> do
  c_rounded_mul_longdouble (fromEnum mode) resultPtr xPtr yPtr
  peek resultPtr

roundedDiv_ld :: RoundingMode -> LongDouble -> LongDouble -> LongDouble
roundedDiv_ld mode x y = unsafePerformIO $
  with x $ \xPtr ->
  with y $ \yPtr ->
  alloca $ \resultPtr -> do
  c_rounded_div_longdouble (fromEnum mode) resultPtr xPtr yPtr
  peek resultPtr

roundedSqrt_ld :: RoundingMode -> LongDouble -> LongDouble
roundedSqrt_ld mode x = unsafePerformIO $
  with x $ \xPtr ->
  alloca $ \resultPtr -> do
  c_rounded_sqrt_longdouble (fromEnum mode) resultPtr xPtr
  peek resultPtr

roundedFMA_ld :: RoundingMode -> LongDouble -> LongDouble -> LongDouble -> LongDouble
roundedFMA_ld mode x y z = unsafePerformIO $
  with x $ \xPtr ->
  with y $ \yPtr ->
  with z $ \zPtr ->
  alloca $ \resultPtr -> do
  c_rounded_fma_longdouble (fromEnum mode) resultPtr xPtr yPtr zPtr
  peek resultPtr

instance RealFloatConstants LongDouble where
  positiveInfinity = 1/0
  negativeInfinity = -1/0
  maxFinite = 0x1.fffffffffffffffep+16383
  minPositive = encodeFloat 1 (-16445) -- The literal 0x1p-16445 yields 0 on long-double-0.1.1
  pi_down = Rounded 0x1.921fb54442d18468p+1
  pi_up   = Rounded 0x1.921fb54442d1846ap+1
  -- 3*pi
  three_pi_down = Rounded 0x1.2d97c7f3321d234ep+3
  three_pi_up   = Rounded 0x1.2d97c7f3321d2350p+3
  -- 5*pi
  five_pi_down = Rounded 0x1.f6a7a2955385e582p+3
  five_pi_up   = Rounded 0x1.f6a7a2955385e584p+3
  -- log(2)
  log2_down = Rounded 0x1.62e42fefa39ef356p-1
  log2_up   = Rounded 0x1.62e42fefa39ef358p-1
  -- exp(1)
  exp1_down = Rounded 0x1.5bf0a8b145769534p+1
  exp1_up   = Rounded 0x1.5bf0a8b145769536p+1
  -- exp(1/2)
  exp1_2_down = Rounded 0x1.a61298e1e069bc96p+0
  exp1_2_up   = Rounded 0x1.a61298e1e069bc98p+0
  -- exp(-1/2)
  expm1_2_down = Rounded 0x1.368b2fc6f9609fe6p-1
  expm1_2_up   = Rounded 0x1.368b2fc6f9609fe8p-1
  -- sqrt(2)
  sqrt2_down = Rounded 0x1.6a09e667f3bcc908p+0
  sqrt2_up   = Rounded 0x1.6a09e667f3bcc90ap+0
  -- sqrt(1/2)
  sqrt1_2_down = Rounded 0x1.6a09e667f3bcc908p-1
  sqrt1_2_up   = Rounded 0x1.6a09e667f3bcc90ap-1
  -- sqrt(2)-1
  sqrt2m1_down = Rounded 0x1.a827999fcef32422p-2
  sqrt2m1_up   = Rounded 0x1.a827999fcef32424p-2
  -- 3 - 2 * sqrt(2)
  three_minus_2sqrt2_down = Rounded 0x1.5f619980c4336f74p-3
  three_minus_2sqrt2_up   = Rounded 0x1.5f619980c4336f76p-3
  -- 2 - sqrt(2)
  two_minus_sqrt2_down = Rounded 0x1.2bec333018866deep-1
  two_minus_sqrt2_up   = Rounded 0x1.2bec333018866df0p-1

-- | Only available on x86/x86_64 systems.
-- Note that 'LongDouble' may not work correctly on Win64.
instance RoundedRing LongDouble where
  roundedAdd = roundedAdd_ld
  roundedSub = roundedSub_ld
  roundedMul = roundedMul_ld
  roundedFusedMultiplyAdd = roundedFMA_ld
  roundedFromInteger = fromInt
  intervalFromInteger = intervalFromInteger_default
  backendNameT = Tagged cBackendName
  {-# INLINE roundedAdd #-}
  {-# INLINE roundedSub #-}
  {-# INLINE roundedMul #-}
  {-# INLINE roundedFusedMultiplyAdd #-}
  {-# INLINE roundedFromInteger #-}
  {-# INLINE intervalFromInteger #-}

-- | Only available on x86/x86_64 systems.
-- Note that 'LongDouble' may not work correctly on Win64.
instance RoundedFractional LongDouble where
  roundedDiv = roundedDiv_ld
  roundedFromRational r x = fromRatio r (numerator x) (denominator x)
  intervalFromRational = intervalFromRational_default
  {-# INLINE roundedDiv #-}
  {-# INLINE roundedFromRational #-}
  {-# INLINE intervalFromRational #-}

-- | Only available on x86/x86_64 systems.
-- Note that 'LongDouble' may not work correctly on Win64.
instance RoundedSqrt LongDouble where
  roundedSqrt = roundedSqrt_ld
  {-# INLINE roundedSqrt #-}

--
-- Backend name
--

foreign import ccall unsafe "rounded_hw_backend_name_longdouble"
  c_backend_name :: CString

cBackendName :: String
cBackendName = unsafePerformIO (peekCString c_backend_name)
