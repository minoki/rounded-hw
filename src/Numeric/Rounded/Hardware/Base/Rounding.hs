{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Numeric.Rounded.Hardware.Base.Rounding where
import           Control.DeepSeq (NFData (..))
import           Data.Coerce
import           Data.Proxy
import           Data.Tagged
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import           GHC.Generics    (Generic)
import           Foreign.Storable            (Storable)

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

newtype instance UM.MVector s (Rounded rn a) = MV_Rounded (UM.MVector s a)
newtype instance U.Vector (Rounded rn a) = V_Rounded (U.Vector a)

instance U.Unbox a => GM.MVector UM.MVector (Rounded rn a) where
  basicLength (MV_Rounded mv) = GM.basicLength mv
  basicUnsafeSlice i l (MV_Rounded mv) = MV_Rounded (GM.basicUnsafeSlice i l mv)
  basicOverlaps (MV_Rounded mv) (MV_Rounded mv') = GM.basicOverlaps mv mv'
  basicUnsafeNew l = MV_Rounded <$> GM.basicUnsafeNew l
  basicInitialize (MV_Rounded mv) = GM.basicInitialize mv
  basicUnsafeReplicate i x = MV_Rounded <$> GM.basicUnsafeReplicate i (coerce x)
  basicUnsafeRead (MV_Rounded mv) i = coerce <$> GM.basicUnsafeRead mv i
  basicUnsafeWrite (MV_Rounded mv) i x = GM.basicUnsafeWrite mv i (coerce x)
  basicClear (MV_Rounded mv) = GM.basicClear mv
  basicSet (MV_Rounded mv) x = GM.basicSet mv (coerce x)
  basicUnsafeCopy (MV_Rounded mv) (MV_Rounded mv') = GM.basicUnsafeCopy mv mv'
  basicUnsafeMove (MV_Rounded mv) (MV_Rounded mv') = GM.basicUnsafeMove mv mv'
  basicUnsafeGrow (MV_Rounded mv) n = MV_Rounded <$> GM.basicUnsafeGrow mv n

instance U.Unbox a => G.Vector U.Vector (Rounded rn a) where
  basicUnsafeFreeze (MV_Rounded mv) = V_Rounded <$> G.basicUnsafeFreeze mv
  basicUnsafeThaw (V_Rounded v) = MV_Rounded <$> G.basicUnsafeThaw v
  basicLength (V_Rounded v) = G.basicLength v
  basicUnsafeSlice i l (V_Rounded v) = V_Rounded (G.basicUnsafeSlice i l v)
  basicUnsafeIndexM (V_Rounded v) i = coerce <$> G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_Rounded mv) (V_Rounded v) = G.basicUnsafeCopy mv v
  elemseq (V_Rounded v) x y = G.elemseq v (coerce x) y

instance U.Unbox a => U.Unbox (Rounded rn a)
