{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
module Numeric.Rounded.Hardware.Base.Rounding
  ( RoundingMode(..)
  , oppositeRoundingMode
  , Rounding
  , rounding
  , reifyRounding
  , Rounded(..)
  , VUM.MVector(MV_Rounded)
  , VU.Vector(V_Rounded)
  ) where
import           Control.DeepSeq             (NFData (..))
import           Data.Coerce
import           Data.Proxy
import           Data.Tagged
import qualified Data.Vector.Generic         as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import           Foreign.Storable            (Storable)
import           GHC.Generics                (Generic)

-- See cbits/rounded.c for the ordering
data RoundingMode
  = TowardNearest
  | TowardNegInf
  | TowardInf
  | TowardZero
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Generic)

instance NFData RoundingMode

oppositeRoundingMode :: RoundingMode -> RoundingMode
oppositeRoundingMode TowardNearest = TowardNearest
oppositeRoundingMode TowardZero    = TowardZero
oppositeRoundingMode TowardInf     = TowardNegInf
oppositeRoundingMode TowardNegInf  = TowardInf

class Rounding (rn :: RoundingMode) where
  roundingT :: Tagged rn RoundingMode

instance Rounding 'TowardNearest where
  roundingT = Tagged TowardNearest

instance Rounding 'TowardInf where
  roundingT = Tagged TowardInf

instance Rounding 'TowardNegInf where
  roundingT = Tagged TowardNegInf

instance Rounding 'TowardZero where
  roundingT = Tagged TowardZero

rounding :: Rounding rn => proxy rn -> RoundingMode
rounding = Data.Tagged.proxy roundingT
{-# INLINE rounding #-}

reifyRounding :: RoundingMode -> (forall s. Rounding s => Proxy s -> r) -> r
reifyRounding TowardNearest f = f (Proxy :: Proxy 'TowardNearest)
reifyRounding TowardInf f     = f (Proxy :: Proxy 'TowardInf)
reifyRounding TowardNegInf f  = f (Proxy :: Proxy 'TowardNegInf)
reifyRounding TowardZero f    = f (Proxy :: Proxy 'TowardZero)
{-# INLINE reifyRounding #-}

newtype Rounded (rn :: RoundingMode) a = Rounded a
  deriving (Eq,Ord,Show,Generic,Functor,Storable)

instance NFData a => NFData (Rounded rn a)

-- Orphan instances:
-- instance Num (Rounded rn a) is defined in Numeric.Rounded.Hardware.Class.
-- instance Fractional (Rounded rn a) is defined in Numeric.Rounded.Hardware.Class.
-- instance Real (Rounded rn a) is defined in Numeric.Rounded.Hardware.Class.
-- instance RealFrac (Rounded rn a) is defined in Numeric.Rounded.Hardware.Class.
-- instance Floating (Rounded rn a) is not implemented yet...

newtype instance VUM.MVector s (Rounded rn a) = MV_Rounded (VUM.MVector s a)
newtype instance VU.Vector (Rounded rn a) = V_Rounded (VU.Vector a)

instance VU.Unbox a => VGM.MVector VUM.MVector (Rounded rn a) where
  basicLength (MV_Rounded mv) = VGM.basicLength mv
  basicUnsafeSlice i l (MV_Rounded mv) = MV_Rounded (VGM.basicUnsafeSlice i l mv)
  basicOverlaps (MV_Rounded mv) (MV_Rounded mv') = VGM.basicOverlaps mv mv'
  basicUnsafeNew l = MV_Rounded <$> VGM.basicUnsafeNew l
  basicInitialize (MV_Rounded mv) = VGM.basicInitialize mv
  basicUnsafeReplicate i x = MV_Rounded <$> VGM.basicUnsafeReplicate i (coerce x)
  basicUnsafeRead (MV_Rounded mv) i = coerce <$> VGM.basicUnsafeRead mv i
  basicUnsafeWrite (MV_Rounded mv) i x = VGM.basicUnsafeWrite mv i (coerce x)
  basicClear (MV_Rounded mv) = VGM.basicClear mv
  basicSet (MV_Rounded mv) x = VGM.basicSet mv (coerce x)
  basicUnsafeCopy (MV_Rounded mv) (MV_Rounded mv') = VGM.basicUnsafeCopy mv mv'
  basicUnsafeMove (MV_Rounded mv) (MV_Rounded mv') = VGM.basicUnsafeMove mv mv'
  basicUnsafeGrow (MV_Rounded mv) n = MV_Rounded <$> VGM.basicUnsafeGrow mv n

instance VU.Unbox a => VG.Vector VU.Vector (Rounded rn a) where
  basicUnsafeFreeze (MV_Rounded mv) = V_Rounded <$> VG.basicUnsafeFreeze mv
  basicUnsafeThaw (V_Rounded v) = MV_Rounded <$> VG.basicUnsafeThaw v
  basicLength (V_Rounded v) = VG.basicLength v
  basicUnsafeSlice i l (V_Rounded v) = V_Rounded (VG.basicUnsafeSlice i l v)
  basicUnsafeIndexM (V_Rounded v) i = coerce <$> VG.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_Rounded mv) (V_Rounded v) = VG.basicUnsafeCopy mv v
  elemseq (V_Rounded v) x y = VG.elemseq v (coerce x) y

instance VU.Unbox a => VU.Unbox (Rounded rn a)
