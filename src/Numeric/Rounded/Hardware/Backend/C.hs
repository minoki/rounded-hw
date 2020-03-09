{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Numeric.Rounded.Hardware.Backend.C
  ( CFloat(..)
  , CDouble(..)
  , VUM.MVector(..)
  , VU.Vector(..)
  ) where
import           Control.DeepSeq (NFData (..))
import           Data.Bifunctor
import           Data.Coerce
import           Data.Int (Int64)
import           Data.Primitive.ByteArray
import           Data.Ratio
import           Data.Tagged
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed.Base as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import           Data.Word (Word64)
import qualified FFIWrapper.Double as D
import qualified FFIWrapper.Float as F
import           Foreign.C.String (CString, peekCString)
import           Foreign.Ptr (castPtr)
import           Foreign.Storable (Storable)
import           GHC.Generics (Generic)
import           Numeric.Rounded.Hardware.Internal.Class
import           Numeric.Rounded.Hardware.Internal.Conversion
import           System.IO.Unsafe (unsafePerformIO)

--
-- Float
--

newtype CFloat = CFloat Float
  deriving (Eq,Ord,Show,Generic,Num,Storable)

instance NFData CFloat

roundedFloatFromInt64 :: RoundingMode -> Int64 -> Float
roundedFloatFromInt64 r x = staticIf
  (-0x1000000 <= x && x <= 0x1000000 {- abs x <= 2^24 -}) -- if input is known to be small enough
  (fromIntegral x)
  (F.roundedFromInt64 r x)
{-# INLINE roundedFloatFromInt64 #-}

roundedFloatFromWord64 :: RoundingMode -> Word64 -> Float
roundedFloatFromWord64 r x = staticIf
  (x <= 0x1000000 {- x <= 2^24 -}) -- if input is known to be small enough
  (fromIntegral x)
  (F.roundedFromWord64 r x)
{-# INLINE roundedFloatFromWord64 #-}

roundedFloatFromInteger :: RoundingMode -> Integer -> Float
roundedFloatFromInteger r x
  | -0x1000000 <= x && x <= 0x1000000 {- abs x <= 2^24 -} = fromInteger x
  | otherwise = fromInt r x
{-# NOINLINE [1] roundedFloatFromInteger #-}

{-# RULES
"roundeFloatFromInteger/Int" forall r (x :: Int).
  roundedFloatFromInteger r (fromIntegral x) = roundedFloatFromInt64 r (fromIntegral x)
"roundeFloatFromInteger/Int64" forall r (x :: Int64).
  roundedFloatFromInteger r (fromIntegral x) = roundedFloatFromInt64 r x
"roundeFloatFromInteger/Word" forall r (x :: Word).
  roundedFloatFromInteger r (fromIntegral x) = roundedFloatFromWord64 r (fromIntegral x)
"roundeFloatFromInteger/Word64" forall r (x :: Word64).
  roundedFloatFromInteger r (fromIntegral x) = roundedFloatFromWord64 r x
  #-}

intervalFloatFromInteger :: Integer -> (Rounded 'TowardNegInf Float, Rounded 'TowardInf Float)
intervalFloatFromInteger x
  | -0x1000000 <= x && x <= 0x1000000 {- abs x <= 2^24 -} = (Rounded (fromInteger x), Rounded (fromInteger x))
  | otherwise = intervalFromInteger_default x

roundedFloatFromRealFloat :: RealFloat a => RoundingMode -> a -> Float
roundedFloatFromRealFloat r x | isNaN x = 0/0
                              | isInfinite x = if x > 0 then 1/0 else -1/0
                              | isNegativeZero x = -0
                              | otherwise = coerce (roundedFromRational r (toRational x) :: CFloat)
{-# NOINLINE [1] roundedFloatFromRealFloat #-}
{-# RULES
"roundedFloatFromRealFloat/Float" forall r (x :: Float).
  roundedFloatFromRealFloat r x = x
  #-}

instance RoundedRing CFloat where
  roundedAdd = coerce F.roundedAdd
  roundedSub = coerce F.roundedSub
  roundedMul = coerce F.roundedMul
  intervalMul x x' y y' = (coerce F.intervalMul_down x x' y y', coerce F.intervalMul_up x x' y y')
  intervalMulAdd x x' y y' z z' = (coerce F.intervalMulAdd_down x x' y y' z, coerce F.intervalMulAdd_up x x' y y' z')
  roundedFromInteger r x = CFloat (roundedFloatFromInteger r x)
  intervalFromInteger = coerce intervalFloatFromInteger
  backendNameT = Tagged cBackendName
  {-# INLINE roundedAdd #-}
  {-# INLINE roundedSub #-}
  {-# INLINE roundedMul #-}
  {-# INLINE intervalMul #-}
  {-# INLINE roundedFromInteger #-}
  {-# INLINE intervalFromInteger #-}

instance RoundedFractional CFloat where
  roundedDiv = coerce F.roundedDiv
  intervalDiv x x' y y' = (coerce F.intervalDiv_down x x' y y', coerce F.intervalDiv_up x x' y y')
  intervalDivAdd x x' y y' z z' = (coerce F.intervalDivAdd_down x x' y y' z, coerce F.intervalDivAdd_up x x' y y' z')
  roundedFromRational r x = CFloat $ fromRatio r (numerator x) (denominator x)
  intervalFromRational = (coerce `asTypeOf` (bimap (CFloat <$>) (CFloat <$>) .)) intervalFromRational_default
  roundedFromRealFloat r x = coerce (roundedFloatFromRealFloat r x)
  {-# INLINE roundedDiv #-}
  {-# INLINE intervalDiv #-}
  {-# INLINE roundedFromRational #-}
  {-# INLINE intervalFromRational #-}
  {-# INLINE roundedFromRealFloat #-}

instance RoundedSqrt CFloat where
  roundedSqrt = coerce F.roundedSqrt
  {-# INLINE roundedSqrt #-}

instance RoundedVectorOperation CFloat where
  roundedSum_StorableVector mode vec = CFloat $ unsafePerformIO $
    VS.unsafeWith vec $ \ptr -> F.roundedSumPtr mode 0 (VS.length vec) (castPtr ptr)
  roundedSum_UnboxedVector mode (V_CFloat (VU.V_Float (VP.Vector off len (ByteArray arr)))) =
    CFloat $ F.roundedSumByteArray mode off len arr

--
-- Double
--

newtype CDouble = CDouble Double
  deriving (Eq,Ord,Show,Generic,Num,Storable)

instance NFData CDouble

roundedDoubleFromInt64 :: RoundingMode -> Int64 -> Double
roundedDoubleFromInt64 r x = staticIf
  (-0x20000000000000 <= x && x <= 0x20000000000000 {- abs x <= 2^53 -}) -- if input is known to be small enough
  (fromIntegral x)
  (D.roundedFromInt64 r x)
{-# INLINE roundedDoubleFromInt64 #-}

roundedDoubleFromWord64 :: RoundingMode -> Word64 -> Double
roundedDoubleFromWord64 r x = staticIf
  (x <= 0x20000000000000 {- x <= 2^53 -}) -- if input is known to be small enough
  (fromIntegral x)
  (D.roundedFromWord64 r x)
{-# INLINE roundedDoubleFromWord64 #-}

roundedDoubleFromInteger :: RoundingMode -> Integer -> Double
roundedDoubleFromInteger r x
  | -0x20000000000000 <= x && x <= 0x20000000000000 {- abs x <= 2^53 -} = fromInteger x
  | otherwise = fromInt r x
{-# NOINLINE [1] roundedDoubleFromInteger #-}

{-# RULES
"roundedDoubleFromInteger/Int" forall r (x :: Int).
  roundedDoubleFromInteger r (fromIntegral x) = roundedDoubleFromInt64 r (fromIntegral x)
"roundedDoubleFromInteger/Int64" forall r (x :: Int64).
  roundedDoubleFromInteger r (fromIntegral x) = roundedDoubleFromInt64 r x
"roundedDoubleFromInteger/Word" forall r (x :: Word).
  roundedDoubleFromInteger r (fromIntegral x) = roundedDoubleFromWord64 r (fromIntegral x)
"roundedDoubleFromInteger/Word64" forall r (x :: Word64).
  roundedDoubleFromInteger r (fromIntegral x) = roundedDoubleFromWord64 r x
  #-}

intervalDoubleFromInteger :: Integer -> (Rounded 'TowardNegInf Double, Rounded 'TowardInf Double)
intervalDoubleFromInteger x
  | -0x20000000000000 <= x && x <= 0x20000000000000 {- abs x <= 2^53 -} = (Rounded (fromInteger x), Rounded (fromInteger x))
  | otherwise = intervalFromInteger_default x

roundedDoubleFromRealFloat :: RealFloat a => RoundingMode -> a -> Double
roundedDoubleFromRealFloat r x | isNaN x = 0/0
                               | isInfinite x = if x > 0 then 1/0 else -1/0
                               | isNegativeZero x = -0
                               | otherwise = coerce (roundedFromRational r (toRational x) :: CDouble)
{-# NOINLINE [1] roundedDoubleFromRealFloat #-}
{-# RULES
"roundedDoubleFromRealFloat/Double" forall r (x :: Double).
  roundedDoubleFromRealFloat r x = x
"roundedDoubleFromRealFloat/Float" forall r (x :: Float).
  roundedDoubleFromRealFloat r x = realToFrac x -- should be rewritten into float2Double
  #-}

instance RoundedRing CDouble where
  roundedAdd = coerce D.roundedAdd
  roundedSub = coerce D.roundedSub
  roundedMul = coerce D.roundedMul
  intervalMul x x' y y' = (coerce D.intervalMul_down x x' y y', coerce D.intervalMul_up x x' y y')
  intervalMulAdd x x' y y' z z' = (coerce D.intervalMulAdd_down x x' y y' z, coerce D.intervalMulAdd_up x x' y y' z')
  roundedFromInteger = coerce roundedDoubleFromInteger
  intervalFromInteger = coerce intervalDoubleFromInteger
  backendNameT = Tagged cBackendName
  {-# INLINE roundedAdd #-}
  {-# INLINE roundedSub #-}
  {-# INLINE roundedMul #-}
  {-# INLINE intervalMul #-}
  {-# INLINE roundedFromInteger #-}
  {-# INLINE intervalFromInteger #-}

instance RoundedFractional CDouble where
  roundedDiv = coerce D.roundedDiv
  intervalDiv x x' y y' = (coerce D.intervalDiv_down x x' y y', coerce D.intervalDiv_up x x' y y')
  intervalDivAdd x x' y y' z z' = (coerce D.intervalDivAdd_down x x' y y' z, coerce D.intervalDivAdd_up x x' y y' z')
  roundedFromRational r x = CDouble $ fromRatio r (numerator x) (denominator x)
  intervalFromRational = (coerce `asTypeOf` (bimap (CDouble <$>) (CDouble <$>) .)) intervalFromRational_default
  -- TODO: Specialize small case in ***FromRational?
  roundedFromRealFloat r x = coerce (roundedDoubleFromRealFloat r x)
  {-# INLINE roundedDiv #-}
  {-# INLINE intervalDiv #-}
  {-# INLINE roundedFromRational #-}
  {-# INLINE intervalFromRational #-}
  {-# INLINE roundedFromRealFloat #-}

instance RoundedSqrt CDouble where
  roundedSqrt = coerce D.roundedSqrt
  {-# INLINE roundedSqrt #-}

instance RoundedVectorOperation CDouble where
  roundedSum_StorableVector mode vec = CDouble $ unsafePerformIO $
    VS.unsafeWith vec $ \ptr -> D.roundedSumPtr mode 0 (VS.length vec) (castPtr ptr)
  roundedSum_UnboxedVector mode (V_CDouble (VU.V_Double (VP.Vector off len (ByteArray arr)))) =
    CDouble $ D.roundedSumByteArray mode off len arr

--
-- Backend name
--

foreign import ccall unsafe "rounded_hw_backend_name"
  c_backend_name :: CString

cBackendName :: String
cBackendName = unsafePerformIO (peekCString c_backend_name)

--
-- Utility function for constant folding
--

staticIf :: Bool -> a -> a -> a
staticIf _ _ x = x
{-# INLINE [0] staticIf #-}

{-# RULES
"staticIf/True" forall x y. staticIf True x y = x
"staticIf/False" forall x y. staticIf False x y = y
  #-}

--
-- instance for Data.Vector.Unboxed.Unbox
--

newtype instance VUM.MVector s CFloat = MV_CFloat (VUM.MVector s Float)
newtype instance VU.Vector CFloat = V_CFloat (VU.Vector Float)

instance VGM.MVector VUM.MVector CFloat where
  basicLength (MV_CFloat mv) = VGM.basicLength mv
  basicUnsafeSlice i l (MV_CFloat mv) = MV_CFloat (VGM.basicUnsafeSlice i l mv)
  basicOverlaps (MV_CFloat mv) (MV_CFloat mv') = VGM.basicOverlaps mv mv'
  basicUnsafeNew l = MV_CFloat <$> VGM.basicUnsafeNew l
  basicInitialize (MV_CFloat mv) = VGM.basicInitialize mv
  basicUnsafeReplicate i x = MV_CFloat <$> VGM.basicUnsafeReplicate i (coerce x)
  basicUnsafeRead (MV_CFloat mv) i = coerce <$> VGM.basicUnsafeRead mv i
  basicUnsafeWrite (MV_CFloat mv) i x = VGM.basicUnsafeWrite mv i (coerce x)
  basicClear (MV_CFloat mv) = VGM.basicClear mv
  basicSet (MV_CFloat mv) x = VGM.basicSet mv (coerce x)
  basicUnsafeCopy (MV_CFloat mv) (MV_CFloat mv') = VGM.basicUnsafeCopy mv mv'
  basicUnsafeMove (MV_CFloat mv) (MV_CFloat mv') = VGM.basicUnsafeMove mv mv'
  basicUnsafeGrow (MV_CFloat mv) n = MV_CFloat <$> VGM.basicUnsafeGrow mv n

instance VG.Vector VU.Vector CFloat where
  basicUnsafeFreeze (MV_CFloat mv) = V_CFloat <$> VG.basicUnsafeFreeze mv
  basicUnsafeThaw (V_CFloat v) = MV_CFloat <$> VG.basicUnsafeThaw v
  basicLength (V_CFloat v) = VG.basicLength v
  basicUnsafeSlice i l (V_CFloat v) = V_CFloat (VG.basicUnsafeSlice i l v)
  basicUnsafeIndexM (V_CFloat v) i = coerce <$> VG.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_CFloat mv) (V_CFloat v) = VG.basicUnsafeCopy mv v
  elemseq (V_CFloat v) x y = VG.elemseq v (coerce x) y

instance VU.Unbox CFloat

newtype instance VUM.MVector s CDouble = MV_CDouble (VUM.MVector s Double)
newtype instance VU.Vector CDouble = V_CDouble (VU.Vector Double)

instance VGM.MVector VUM.MVector CDouble where
  basicLength (MV_CDouble mv) = VGM.basicLength mv
  basicUnsafeSlice i l (MV_CDouble mv) = MV_CDouble (VGM.basicUnsafeSlice i l mv)
  basicOverlaps (MV_CDouble mv) (MV_CDouble mv') = VGM.basicOverlaps mv mv'
  basicUnsafeNew l = MV_CDouble <$> VGM.basicUnsafeNew l
  basicInitialize (MV_CDouble mv) = VGM.basicInitialize mv
  basicUnsafeReplicate i x = MV_CDouble <$> VGM.basicUnsafeReplicate i (coerce x)
  basicUnsafeRead (MV_CDouble mv) i = coerce <$> VGM.basicUnsafeRead mv i
  basicUnsafeWrite (MV_CDouble mv) i x = VGM.basicUnsafeWrite mv i (coerce x)
  basicClear (MV_CDouble mv) = VGM.basicClear mv
  basicSet (MV_CDouble mv) x = VGM.basicSet mv (coerce x)
  basicUnsafeCopy (MV_CDouble mv) (MV_CDouble mv') = VGM.basicUnsafeCopy mv mv'
  basicUnsafeMove (MV_CDouble mv) (MV_CDouble mv') = VGM.basicUnsafeMove mv mv'
  basicUnsafeGrow (MV_CDouble mv) n = MV_CDouble <$> VGM.basicUnsafeGrow mv n

instance VG.Vector VU.Vector CDouble where
  basicUnsafeFreeze (MV_CDouble mv) = V_CDouble <$> VG.basicUnsafeFreeze mv
  basicUnsafeThaw (V_CDouble v) = MV_CDouble <$> VG.basicUnsafeThaw v
  basicLength (V_CDouble v) = VG.basicLength v
  basicUnsafeSlice i l (V_CDouble v) = V_CDouble (VG.basicUnsafeSlice i l v)
  basicUnsafeIndexM (V_CDouble v) i = coerce <$> VG.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_CDouble mv) (V_CDouble v) = VG.basicUnsafeCopy mv v
  elemseq (V_CDouble v) x y = VG.elemseq v (coerce x) y

instance VU.Unbox CDouble
