{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
module Numeric.Rounded.Hardware.Backend.C
  ( CFloat(..)
  , CDouble(..)
  , VUM.MVector(..)
  , VU.Vector(..)
  ) where
import           Control.DeepSeq                          (NFData (..))
import           Data.Coerce
import           Data.Functor.Product
import           Data.Primitive.ByteArray
import           Data.Ratio
import           Data.Tagged
import qualified Data.Vector.Generic                      as VG
import qualified Data.Vector.Generic.Mutable              as VGM
import qualified Data.Vector.Primitive                    as VP
import qualified Data.Vector.Storable                     as VS
import qualified Data.Vector.Unboxed.Base                 as VU
import qualified Data.Vector.Unboxed.Mutable              as VUM
import           FFIImports
import qualified FFIWrapper.Double                        as D
import qualified FFIWrapper.Float                         as F
import           Foreign.C.String                         (CString, peekCString)
import           Foreign.Ptr                              (castPtr)
import           Foreign.Storable                         (Storable)
import           GHC.Generics                             (Generic)
import           System.IO.Unsafe                         (unsafePerformIO)
import           Numeric.Rounded.Hardware.Internal.Class
import           Numeric.Rounded.Hardware.Internal.Conversion

--
-- Float
--

newtype CFloat = CFloat Float
  deriving (Eq,Ord,Show,Generic,Num,Storable)

instance NFData CFloat

instance RoundedRing CFloat where
  roundedAdd = coerce F.roundedAdd
  roundedSub = coerce F.roundedSub
  roundedMul = coerce F.roundedMul
  intervalMul x x' y y' = (coerce c_interval_mul_float_down x x' y y', coerce c_interval_mul_float_up x x' y y')
  roundedFromInteger rn x = CFloat (fromInt rn x)
  intervalFromInteger x = case fromIntF x :: Product (Rounded 'TowardNegInf) (Rounded 'TowardInf) Float of
    Pair a b -> (CFloat <$> a, CFloat <$> b)
  backendNameT = Tagged cBackendName
  {-# INLINE roundedAdd #-}
  {-# INLINE roundedSub #-}
  {-# INLINE roundedMul #-}
  {-# INLINE intervalMul #-}
  {-# INLINE roundedFromInteger #-}
  {-# INLINE intervalFromInteger #-}

instance RoundedFractional CFloat where
  roundedDiv = coerce F.roundedDiv
  intervalDiv x x' y y' = (coerce c_interval_div_float_down x x' y y', coerce c_interval_div_float_up x x' y y')
  roundedFromRational rn x = CFloat $ fromRatio rn (numerator x) (denominator x)
  intervalFromRational x = case fromRatioF (numerator x) (denominator x) :: Product (Rounded 'TowardNegInf) (Rounded 'TowardInf) Float of
    Pair a b -> (CFloat <$> a, CFloat <$> b)
  {-# INLINE roundedDiv #-}
  {-# INLINE intervalDiv #-}
  {-# INLINE roundedFromRational #-}
  {-# INLINE intervalFromRational #-}

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

instance RoundedRing CDouble where
  roundedAdd = coerce D.roundedAdd
  roundedSub = coerce D.roundedSub
  roundedMul = coerce D.roundedMul
  intervalMul x x' y y' = (coerce c_interval_mul_double_down x x' y y', coerce c_interval_mul_double_up x x' y y')
  roundedFromInteger rn x = CDouble (fromInt rn x)
  intervalFromInteger x = case fromIntF x :: Product (Rounded 'TowardNegInf) (Rounded 'TowardInf) Double of
    Pair a b -> (CDouble <$> a, CDouble <$> b)
  backendNameT = Tagged cBackendName
  {-# INLINE roundedAdd #-}
  {-# INLINE roundedSub #-}
  {-# INLINE roundedMul #-}
  {-# INLINE intervalMul #-}
  {-# INLINE roundedFromInteger #-}
  {-# INLINE intervalFromInteger #-}

instance RoundedFractional CDouble where
  roundedDiv = coerce D.roundedDiv
  intervalDiv x x' y y' = (coerce c_interval_div_double_down x x' y y', coerce c_interval_div_double_up x x' y y')
  roundedFromRational rn x = CDouble $ fromRatio rn (numerator x) (denominator x)
  intervalFromRational x = case fromRatioF (numerator x) (denominator x) :: Product (Rounded 'TowardNegInf) (Rounded 'TowardInf) Double of
    Pair a b -> (CDouble <$> a, CDouble <$> b)
  -- TODO: Specialize small case in ***FromRational?
  {-# INLINE roundedDiv #-}
  {-# INLINE intervalDiv #-}
  {-# INLINE roundedFromRational #-}
  {-# INLINE intervalFromRational #-}

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
