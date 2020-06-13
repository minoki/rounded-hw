{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Numeric.Rounded.Hardware.Internal.Rounding
  ( RoundingMode(..)
  , oppositeRoundingMode
  , Rounding
  , rounding
  , reifyRounding
  , Rounded(..)
  , VUM.MVector(MV_Rounded)
  , VU.Vector(V_Rounded)
  ) where
import           Control.DeepSeq (NFData (..))
import           Data.Coerce
import           Data.Proxy
import           Data.Tagged
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import           Foreign.Storable (Storable)
import           GHC.Generics (Generic)

-- See cbits/rounded.c for the ordering
-- | The type for IEEE754 rounding-direction attributes.
data RoundingMode
  = ToNearest     -- ^ Round to the nearest value (IEEE754 roundTiesToEven)
  | TowardNegInf  -- ^ Round downward (IEEE754 roundTowardNegative)
  | TowardInf     -- ^ Round upward (IEEE754 roundTowardPositive)
  | TowardZero    -- ^ Round toward zero (IEEE754 roundTowardZero)
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Generic)

instance NFData RoundingMode

-- | Returns the opposite rounding direction.
--
-- @TowardNegInf@ and @TowardInf@ are swapped.
oppositeRoundingMode :: RoundingMode -> RoundingMode
oppositeRoundingMode ToNearest    = ToNearest
oppositeRoundingMode TowardZero   = TowardZero
oppositeRoundingMode TowardInf    = TowardNegInf
oppositeRoundingMode TowardNegInf = TowardInf

-- | This class allows you to recover the runtime value from a type-level rounding mode.
--
-- See 'rounding'.
class Rounding (r :: RoundingMode) where
  roundingT :: Tagged r RoundingMode

instance Rounding 'ToNearest where
  roundingT = Tagged ToNearest

instance Rounding 'TowardInf where
  roundingT = Tagged TowardInf

instance Rounding 'TowardNegInf where
  roundingT = Tagged TowardNegInf

instance Rounding 'TowardZero where
  roundingT = Tagged TowardZero

-- | Recovers the value from type-level rounding mode.
rounding :: Rounding r => proxy r -> RoundingMode
rounding = Data.Tagged.proxy roundingT
{-# INLINE rounding #-}

-- | Lifts a rounding mode to type-level.
reifyRounding :: RoundingMode -> (forall s. Rounding s => Proxy s -> a) -> a
reifyRounding ToNearest f    = f (Proxy :: Proxy 'ToNearest)
reifyRounding TowardInf f    = f (Proxy :: Proxy 'TowardInf)
reifyRounding TowardNegInf f = f (Proxy :: Proxy 'TowardNegInf)
reifyRounding TowardZero f   = f (Proxy :: Proxy 'TowardZero)
{-# INLINE reifyRounding #-}

-- | A type tagged with a rounding direction.
--
-- The rounding direction is effective for a /single/ operation.
-- You won't get the correctly-rounded result for a compound expression like @(a - b * c) :: Rounded 'TowardInf Double@.
--
-- In particular, a negative literal like @-0.1 :: Rounded r Double@ doesn't yield the correctly-rounded value for @-0.1@.
-- To get the correct value, call 'fromRational' explicitly (i.e. @fromRational (-0.1) :: Rounded r Double@) or use @NegativeLiterals@ extension.
newtype Rounded (r :: RoundingMode) a = Rounded { getRounded :: a }
  deriving (Eq, Ord, Generic, Functor, Storable)

instance Show a => Show (Rounded r a) where
  -- TODO: Take the rounding direction into account
  showsPrec prec (Rounded x) = showParen (prec > 10) $ showString "Rounded " . showsPrec 11 x

instance NFData a => NFData (Rounded r a)

-- Orphan instances:
-- instance Num (Rounded r a) is defined in Numeric.Rounded.Hardware.Internal.Class.
-- instance Fractional (Rounded r a) is defined in Numeric.Rounded.Hardware.Internal.Class.
-- instance Real (Rounded r a) is defined in Numeric.Rounded.Hardware.Internal.Class.
-- instance RealFrac (Rounded r a) is defined in Numeric.Rounded.Hardware.Internal.Class.
-- instance Floating (Rounded r a) is not implemented (something like CRlibm would be needed)

newtype instance VUM.MVector s (Rounded r a) = MV_Rounded (VUM.MVector s a)
newtype instance VU.Vector (Rounded r a) = V_Rounded (VU.Vector a)

instance VU.Unbox a => VGM.MVector VUM.MVector (Rounded r a) where
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

instance VU.Unbox a => VG.Vector VU.Vector (Rounded r a) where
  basicUnsafeFreeze (MV_Rounded mv) = V_Rounded <$> VG.basicUnsafeFreeze mv
  basicUnsafeThaw (V_Rounded v) = MV_Rounded <$> VG.basicUnsafeThaw v
  basicLength (V_Rounded v) = VG.basicLength v
  basicUnsafeSlice i l (V_Rounded v) = V_Rounded (VG.basicUnsafeSlice i l v)
  basicUnsafeIndexM (V_Rounded v) i = coerce <$> VG.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_Rounded mv) (V_Rounded v) = VG.basicUnsafeCopy mv v
  elemseq (V_Rounded v) x y = VG.elemseq v (coerce x) y

instance VU.Unbox a => VU.Unbox (Rounded r a)
