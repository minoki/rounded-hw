{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies  #-}
module Numeric.Rounded.Hardware.Interval
  ( Interval(..)
  , increasing
  , maxI
  , minI
  , powInt
  ) where
import           Control.DeepSeq                          (NFData (..))
import           Control.Monad
import           Control.Monad.ST
import qualified Data.Array.Base as A
import           Data.Coerce
import           Data.Ix
import           Data.Primitive
import qualified Data.Vector.Generic         as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import           GHC.Generics                             (Generic)
import           Numeric.Rounded.Hardware.Internal

data Interval a
  = I !(Rounded 'TowardNegInf a) !(Rounded 'TowardInf a)
  | Empty
  deriving (Show,Generic)

instance NFData a => NFData (Interval a)

increasing :: (forall r. Rounding r => Rounded r a -> Rounded r a) -> Interval a -> Interval a
increasing f (I a b) = I (f a) (f b)
increasing _ Empty   = Empty

instance (Num a, RoundedRing a) => Num (Interval a) where
  I a b + I a' b' = case intervalAdd a b a' b' of
                      (a'', b'') -> I a'' b''
  _ + _ = Empty
  I a b - I a' b' = case intervalSub a b a' b' of
                      (a'', b'') -> I a'' b''
  _ - _ = Empty
  negate (I a b) = I (negate (coerce b)) (negate (coerce a))
  negate Empty   = Empty
  I a b * I a' b' = case intervalMul a b a' b' of
                      (a'', b'') -> I a'' b''
  _ * _ = Empty
  abs x@(I a b)
    | a >= 0 = x
    | b <= 0 = negate x
    | otherwise = I 0 (max (negate (coerce a)) b)
  abs Empty = Empty
  signum = increasing signum
  fromInteger x = case intervalFromInteger x of
                    (y, y') -> I y y'
  {-# SPECIALIZE instance Num (Interval Float) #-}
  {-# SPECIALIZE instance Num (Interval Double) #-}

instance (Num a, RoundedFractional a) => Fractional (Interval a) where
  recip Empty = Empty
  recip (I a b)
    -- TODO: Allow a' == 0 || b' == 0?
    | 0 < a || b < 0 = case intervalRecip a b of
                         (x, x') -> I x x'
    | otherwise = error "divide by zero"
  I a b / I a' b'
    -- TODO: Allow a' == 0 || b' == 0?
    | 0 < a' || b' < 0 = case intervalDiv a b a' b' of
                           (a'', b'') -> I a'' b''
                           -- TODO: Allow a' == 0 || b' == 0?
    | otherwise = error "divide by zero"
  _ / Empty = Empty
  Empty / _ = Empty
  fromRational x = case intervalFromRational x of
                     (y, y') -> I y y'
  {-# SPECIALIZE instance Fractional (Interval Float) #-}
  {-# SPECIALIZE instance Fractional (Interval Double) #-}

maxI :: Ord a => Interval a -> Interval a -> Interval a
maxI (I a a') (I b b') = I (max a b) (max a' b')
maxI _ _               = Empty
{-# SPECIALIZE maxI :: Interval Float -> Interval Float -> Interval Float #-}
{-# SPECIALIZE maxI :: Interval Double -> Interval Double -> Interval Double #-}

minI :: Ord a => Interval a -> Interval a -> Interval a
minI (I a a') (I b b') = I (min a b) (min a' b')
minI _ _               = Empty
{-# SPECIALIZE minI :: Interval Float -> Interval Float -> Interval Float #-}
{-# SPECIALIZE minI :: Interval Double -> Interval Double -> Interval Double #-}

powInt :: (Ord a, Num a, RoundedRing a) => Interval a -> Int -> Interval a
powInt (I a a') n | odd n || 0 <= a = I (a^n) (a'^n)
                  | a' <= 0 = I ((coerce (abs a'))^n) ((coerce (abs a))^n)
                  | otherwise = I 0 (max ((coerce (abs a))^n) (a'^n))
powInt Empty _ = Empty
{-# SPECIALIZE powInt :: Interval Float -> Int -> Interval Float #-}
{-# SPECIALIZE powInt :: Interval Double -> Int -> Interval Double #-}

--
-- Instance for Data.Vector.Unboxed.Unbox
--

newtype instance VUM.MVector s (Interval a) = MV_Interval (VUM.MVector s (a, a))
newtype instance VU.Vector (Interval a) = V_Interval (VU.Vector (a, a))

intervalToPair :: Fractional a => Interval a -> (a, a)
intervalToPair (I (Rounded x) (Rounded y)) = (x, y)
intervalToPair Empty = (1/0, -1/0)
{-# INLINE intervalToPair #-}

pairToInterval :: Ord a => (a, a) -> Interval a
pairToInterval (x, y) | y < x = Empty
                      | otherwise = I (Rounded x) (Rounded y)
{-# INLINE pairToInterval #-}

instance (VU.Unbox a, Ord a, Fractional a) => VGM.MVector VUM.MVector (Interval a) where
  basicLength (MV_Interval mv) = VGM.basicLength mv
  basicUnsafeSlice i l (MV_Interval mv) = MV_Interval (VGM.basicUnsafeSlice i l mv)
  basicOverlaps (MV_Interval mv) (MV_Interval mv') = VGM.basicOverlaps mv mv'
  basicUnsafeNew l = MV_Interval <$> VGM.basicUnsafeNew l
  basicInitialize (MV_Interval mv) = VGM.basicInitialize mv
  basicUnsafeReplicate i x = MV_Interval <$> VGM.basicUnsafeReplicate i (intervalToPair x)
  basicUnsafeRead (MV_Interval mv) i = pairToInterval <$> VGM.basicUnsafeRead mv i
  basicUnsafeWrite (MV_Interval mv) i x = VGM.basicUnsafeWrite mv i (intervalToPair x)
  basicClear (MV_Interval mv) = VGM.basicClear mv
  basicSet (MV_Interval mv) x = VGM.basicSet mv (intervalToPair x)
  basicUnsafeCopy (MV_Interval mv) (MV_Interval mv') = VGM.basicUnsafeCopy mv mv'
  basicUnsafeMove (MV_Interval mv) (MV_Interval mv') = VGM.basicUnsafeMove mv mv'
  basicUnsafeGrow (MV_Interval mv) n = MV_Interval <$> VGM.basicUnsafeGrow mv n
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeMove #-}
  {-# INLINE basicUnsafeGrow #-}

instance (VU.Unbox a, Ord a, Fractional a) => VG.Vector VU.Vector (Interval a) where
  basicUnsafeFreeze (MV_Interval mv) = V_Interval <$> VG.basicUnsafeFreeze mv
  basicUnsafeThaw (V_Interval v) = MV_Interval <$> VG.basicUnsafeThaw v
  basicLength (V_Interval v) = VG.basicLength v
  basicUnsafeSlice i l (V_Interval v) = V_Interval (VG.basicUnsafeSlice i l v)
  basicUnsafeIndexM (V_Interval v) i = pairToInterval <$> VG.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_Interval mv) (V_Interval v) = VG.basicUnsafeCopy mv v
  elemseq (V_Interval _) x y = x `seq` y
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE elemseq #-}

instance (VU.Unbox a, Ord a, Fractional a) => VU.Unbox (Interval a)

--
-- Instances for Data.Array.Unboxed
--

instance (Prim a, Ord a, Fractional a) => A.MArray (A.STUArray s) (Interval a) (ST s) where
  getBounds (A.STUArray l u _ _) = return (l, u)
  getNumElements (A.STUArray _ _ n _) = return n
  -- newArray: Use default
  unsafeNewArray_ = A.newArray_
  newArray_ bounds@(l,u) = do
    let n = rangeSize bounds
    arr@(MutableByteArray arr_) <- newByteArray (2 * sizeOf (undefined :: a))
    setByteArray arr 0 (2 * n) (0 :: a)
    return (A.STUArray l u n arr_)
  unsafeRead (A.STUArray _ _ _ byteArr) i = do
    x <- readByteArray (MutableByteArray byteArr) (2 * i)
    y <- readByteArray (MutableByteArray byteArr) (2 * i + 1)
    return (pairToInterval (x, y))
  unsafeWrite (A.STUArray _ _ _ byteArr) i e = do
    let (x, y) = intervalToPair e
    writeByteArray (MutableByteArray byteArr) (2 * i) x
    writeByteArray (MutableByteArray byteArr) (2 * i + 1) y

instance (Prim a, Ord a, Fractional a) => A.IArray A.UArray (Interval a) where
  bounds (A.UArray l u _ _) = (l,u)
  numElements (A.UArray _ _ n _) = n
  unsafeArray bounds el = runST $ do
    marr <- A.newArray_ bounds
    forM_ el $ \(i,e) -> A.unsafeWrite marr i e
    A.unsafeFreezeSTUArray marr
  unsafeAt (A.UArray _ _ _ byteArr) i =
    let x = indexByteArray (ByteArray byteArr) (2 * i)
        y = indexByteArray (ByteArray byteArr) (2 * i + 1)
    in pairToInterval (x, y)
  -- unsafeReplace, unsafeAccum, unsafeAccumArray: Use default
