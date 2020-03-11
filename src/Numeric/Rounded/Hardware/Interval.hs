{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Numeric.Rounded.Hardware.Interval
  ( Interval(..)
  , increasing
  , maxI
  , minI
  , powInt
  , null
  , inf
  , sup
  , width
  , widthUlp
  , hull
  , intersection
  ) where
import           Control.DeepSeq (NFData (..))
import           Control.Monad
import           Control.Monad.ST
import qualified Data.Array.Base as A
import           Data.Coerce
import           Data.Ix
import           Data.Primitive
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import           GHC.Float (expm1, log1mexp, log1p, log1pexp)
import           GHC.Generics (Generic)
import           Numeric.Rounded.Hardware.Internal
import qualified Numeric.Rounded.Hardware.Interval.Class as C
import qualified Numeric.Rounded.Hardware.Interval.NonEmpty as NE
import           Prelude hiding (null)

data Interval a
  = I !(Rounded 'TowardNegInf a) !(Rounded 'TowardInf a)
  | Empty
  deriving (Show,Generic)

instance NFData a => NFData (Interval a)

increasing :: (forall r. Rounding r => Rounded r a -> Rounded r a) -> Interval a -> Interval a
increasing f (I a b) = I (f a) (f b)
increasing _ Empty   = Empty
{-# INLINE increasing #-}

instance (Num a, RoundedRing a) => Num (Interval a) where
  (+) = liftBinaryNE (+)
  (-) = liftBinaryNE (-)
  negate = liftUnaryNE negate
  (*) = liftBinaryNE (*)
  abs = liftUnaryNE abs
  signum = liftUnaryNE signum
  fromInteger x = case intervalFromInteger x of
                    (y, y') -> I y y'
  {-# INLINE (+) #-}
  {-# INLINE (-) #-}
  {-# INLINE negate #-}
  {-# INLINE (*) #-}
  {-# INLINE abs #-}
  {-# INLINE signum #-}
  {-# INLINE fromInteger #-}

instance (Num a, RoundedFractional a) => Fractional (Interval a) where
  recip = liftUnaryNE recip
  (/) = liftBinaryNE (/)
  fromRational x = case intervalFromRational x of
                     (y, y') -> I y y'
  {-# INLINE recip #-}
  {-# INLINE (/) #-}
  {-# INLINE fromRational #-}

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

null :: Interval a -> Bool
null Empty = True
null _     = False

inf :: Interval a -> Rounded 'TowardNegInf a
inf (I x _) = x
inf _       = error "empty interval"

sup :: Interval a -> Rounded 'TowardInf a
sup (I _ y) = y
sup _       = error "empty interval"

width :: (Num a, RoundedRing a) => Interval a -> Rounded 'TowardInf a
width (I x y) = y - coerce x
width Empty   = 0

widthUlp :: (RealFloat a) => Interval a -> Maybe Integer
widthUlp (I x y) = distanceUlp (getRounded x) (getRounded y)
widthUlp Empty   = Just 0

hull :: RoundedRing a => Interval a -> Interval a -> Interval a
hull (I x y) (I x' y') = I (min x x') (max y y')
hull Empty v           = v
hull u Empty           = u

intersection :: RoundedRing a => Interval a -> Interval a -> Interval a
intersection (I x y) (I x' y') | getRounded x'' <= getRounded y'' = I x'' y''
  where x'' = max x x'
        y'' = min y y'
intersection _ _ = Empty

liftUnaryNE :: (NE.Interval a -> NE.Interval a) -> Interval a -> Interval a
liftUnaryNE f (I x x') = case f (NE.I x x') of
                           NE.I y y' -> I y y'
liftUnaryNE _f Empty = Empty
{-# INLINE [1] liftUnaryNE #-}

liftBinaryNE :: (NE.Interval a -> NE.Interval a -> NE.Interval a) -> Interval a -> Interval a -> Interval a
liftBinaryNE f (I x x') (I y y') = case f (NE.I x x') (NE.I y y') of
                                     NE.I z z' -> I z z'
liftBinaryNE _f _ _ = Empty
{-# INLINE [1] liftBinaryNE #-}

instance (Num a, RoundedFractional a, RoundedSqrt a, Eq a, RealFloat a, RealFloatConstants a) => Floating (Interval a) where
  pi = I pi_down pi_up
  exp = liftUnaryNE exp
  log = liftUnaryNE log
  sqrt = liftUnaryNE sqrt
  (**) = liftBinaryNE (**)
  logBase = liftBinaryNE logBase
  sin = liftUnaryNE sin
  cos = liftUnaryNE cos
  tan = liftUnaryNE tan
  asin = liftUnaryNE asin
  acos = liftUnaryNE acos
  atan = liftUnaryNE atan
  sinh = liftUnaryNE sinh
  cosh = liftUnaryNE cosh
  tanh = liftUnaryNE tanh
  asinh = liftUnaryNE asinh
  acosh = liftUnaryNE acosh
  atanh = liftUnaryNE atanh
  log1p = liftUnaryNE log1p
  expm1 = liftUnaryNE expm1
  log1pexp = liftUnaryNE log1pexp
  log1mexp = liftUnaryNE log1mexp
  {-# INLINE exp #-}
  {-# INLINE log #-}
  {-# INLINE sqrt #-}
  {-# INLINE (**) #-}
  {-# INLINE logBase #-}
  {-# INLINE sin #-}
  {-# INLINE cos #-}
  {-# INLINE tan #-}
  {-# INLINE asin #-}
  {-# INLINE acos #-}
  {-# INLINE atan #-}
  {-# INLINE sinh #-}
  {-# INLINE cosh #-}
  {-# INLINE tanh #-}
  {-# INLINE asinh #-}
  {-# INLINE acosh #-}
  {-# INLINE atanh #-}
  {-# INLINE log1p #-}
  {-# INLINE expm1 #-}
  {-# INLINE log1pexp #-}
  {-# INLINE log1mexp #-}

instance (Num a, RoundedRing a, RealFloat a) => C.IsInterval (Interval a) where
  type EndPoint (Interval a) = a
  makeInterval = I
  width = width
  withEndPoints f (I x y) = f x y
  withEndPoints _ Empty   = Empty
  hull = hull
  intersection = intersection
  maybeIntersection x y = case intersection x y of
                            Empty -> Nothing
                            z     -> Just z
  equalAsSet (I x y) (I x' y') = x == x' && y == y'
  equalAsSet Empty Empty       = True
  equalAsSet _ _               = False
  subset (I x y) (I x' y') = x' <= x && y <= y'
  subset Empty _           = True
  subset I{} Empty         = False
  weaklyLess (I x y) (I x' y') = x <= x' && y <= y'
  weaklyLess Empty Empty       = True
  weaklyLess _ _               = False
  precedes (I _ y) (I x' _) = getRounded y <= getRounded x'
  precedes _ _              = True
  interior (I x y) (I x' y') = getRounded x' <# getRounded x && getRounded y <# getRounded y'
    where s <# t = s < t || (s == t && isInfinite s)
  interior Empty _ = True
  interior I{} Empty = False
  strictLess (I x y) (I x' y') = getRounded x <# getRounded x' && getRounded y <# getRounded y'
    where s <# t = s < t || (s == t && isInfinite s)
  strictLess Empty Empty = True
  strictLess _ _ = False
  strictPrecedes (I _ y) (I x' _) = getRounded y < getRounded x'
  strictPrecedes _ _              = True
  disjoint (I x y) (I x' y') = getRounded y < getRounded x' || getRounded y' < getRounded x
  disjoint _ _ = True

--
-- Instance for Data.Vector.Unboxed.Unbox
--

newtype instance VUM.MVector s (Interval a) = MV_Interval (VUM.MVector s (a, a))
newtype instance VU.Vector (Interval a) = V_Interval (VU.Vector (a, a))

intervalToPair :: Fractional a => Interval a -> (a, a)
intervalToPair (I (Rounded x) (Rounded y)) = (x, y)
intervalToPair Empty                       = (1/0, -1/0)
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
