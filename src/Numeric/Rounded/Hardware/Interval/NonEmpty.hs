{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Numeric.Rounded.Hardware.Interval.NonEmpty
  ( Interval(..)
  , increasing
  , maxI
  , minI
  , powInt
  , null
  , inf
  , sup
  , width
  , hull
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
import           GHC.Float (log1p, expm1)
import           GHC.Generics (Generic)
import           Numeric.Rounded.Hardware.Internal
import qualified Numeric.Rounded.Hardware.Interval.Class as C
import qualified Numeric.Rounded.Hardware.Interval.ElementaryFunctions as C
import           Prelude hiding (null)

data Interval a
  = I !(Rounded 'TowardNegInf a) !(Rounded 'TowardInf a)
  deriving (Show,Generic)

instance NFData a => NFData (Interval a)

increasing :: (forall r. Rounding r => Rounded r a -> Rounded r a) -> Interval a -> Interval a
increasing f (I a b) = I (f a) (f b)

negateI :: (Num a, RoundedRing a) => Interval a -> Interval a
negateI (I a b) = I (negate (coerce b)) (negate (coerce a))
{-# INLINE [0] negateI #-}

addI, subI, mulI :: (Num a, RoundedRing a) => Interval a -> Interval a -> Interval a
I a b `addI` I a' b' = case intervalAdd a b a' b' of
                         (a'', b'') -> I a'' b''
I a b `subI` I a' b' = case intervalSub a b a' b' of
                         (a'', b'') -> I a'' b''
I a b `mulI` I a' b' = case intervalMul a b a' b' of
                         (a'', b'') -> I a'' b''

mulAddI :: (Num a, RoundedRing a) => Interval a -> Interval a -> Interval a -> Interval a
mulAddI (I a b) (I a' b') (I a'' b'') = case intervalMulAdd a b a' b' a'' b'' of
                                          (x, y) -> I x y

normalizeDivisor :: (Ord a, Num a) => Interval a -> Interval a
normalizeDivisor x@(I (Rounded a) (Rounded b))
  | 0 < a || b < 0 = x
  | a == 0 && 0 < b = I (Rounded 0) (Rounded b)
  | a < 0 && b == 0 = I (Rounded a) (Rounded (-0))
  | otherwise = error "divide by zero"

divI :: (Num a, RoundedFractional a) => Interval a -> Interval a -> Interval a
I a b `divI` y = let I a' b' = normalizeDivisor y
                     (z, z') = intervalDiv a b a' b'
                 in I z z'

divAddI :: (Num a, RoundedFractional a) => Interval a -> Interval a -> Interval a -> Interval a
divAddI (I a b) y (I a'' b'') = let I a' b' = normalizeDivisor y
                                    (z, z') = intervalDivAdd a b a' b' a'' b''
                                in I z z'

{-# INLINE [0] addI #-}
{-# INLINE [0] subI #-}
{-# INLINE [0] mulI #-}
{-# INLINE [0] divI #-}
{-# INLINE mulAddI #-}
{-# INLINE divAddI #-}
{-# RULES
"Interval.NonEmpty/x*y+z" forall x y z. addI (mulI x y) z = mulAddI x y z
"Interval.NonEmpty/z+x*y" forall x y z. addI z (mulI x y) = mulAddI x y z
"Interval.NonEmpty/x*y-z" forall x y z. subI (mulI x y) z = mulAddI x y (negateI z)
"Interval.NonEmpty/z-x*y" forall x y z. subI z (mulI x y) = negateI (mulAddI x y (negateI z))
"Interval.NonEmpty/x/y+z" forall x y z. addI (divI x y) z = divAddI x y z
"Interval.NonEmpty/z+x/y" forall x y z. addI z (divI x y) = divAddI x y z
"Interval.NonEmpty/x/y-z" forall x y z. subI (divI x y) z = divAddI x y (negateI z)
"Interval.NonEmpty/z-x/y" forall x y z. subI z (divI x y) = negateI (divAddI x y (negateI z))
"Interval.NonEmpty/negate-negate" forall x. negateI (negateI x) = x
"Interval.NonEmpty/x+(-y)" forall x y. addI x (negateI y) = subI x y
"Interval.NonEmpty/(-y)+x" forall x y. addI (negateI y) x = subI x y
"Interval.NonEmpty/x-(-y)" forall x y. subI x (negateI y) = addI x y
  #-}

instance (Num a, RoundedRing a) => Num (Interval a) where
  (+) = addI
  (-) = subI
  (*) = mulI
  negate = negateI
  abs x@(I a b)
    | a >= 0 = x
    | b <= 0 = negate x
    | otherwise = I 0 (max (negate (coerce a)) b)
  signum = increasing signum
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
  recip x = let I a b = normalizeDivisor x
                (y, y') = intervalRecip a b
            in I y y'
  (/) = divI
  fromRational x = case intervalFromRational x of
                     (y, y') -> I y y'
  {-# INLINE recip #-}
  {-# INLINE (/) #-}
  {-# INLINE fromRational #-}

maxI :: Ord a => Interval a -> Interval a -> Interval a
maxI (I a a') (I b b') = I (max a b) (max a' b')
{-# INLINE maxI #-}

minI :: Ord a => Interval a -> Interval a -> Interval a
minI (I a a') (I b b') = I (min a b) (min a' b')
{-# INLINE minI #-}

powInt :: (Ord a, Num a, RoundedRing a) => Interval a -> Int -> Interval a
powInt (I a a') n | odd n || 0 <= a = I (a^n) (a'^n)
                  | a' <= 0 = I ((coerce (abs a'))^n) ((coerce (abs a))^n)
                  | otherwise = I 0 (max ((coerce (abs a))^n) (a'^n))
{-# SPECIALIZE powInt :: Interval Float -> Int -> Interval Float #-}
{-# SPECIALIZE powInt :: Interval Double -> Int -> Interval Double #-}

null :: Interval a -> Bool
null _     = False

inf :: Interval a -> Rounded 'TowardNegInf a
inf (I x _) = x

sup :: Interval a -> Rounded 'TowardInf a
sup (I _ y) = y

width :: (Num a, RoundedRing a) => Interval a -> Rounded 'TowardInf a
width (I x y) = y - coerce x

hull :: RoundedRing a => Interval a -> Interval a -> Interval a
hull (I x y) (I x' y') = I (min x x') (max y y')

{-# SPECIALIZE C.expP :: Double -> Interval Double #-}
{-# SPECIALIZE C.expI :: Interval Double -> Interval Double #-}
{-# SPECIALIZE C.expm1P :: Double -> Interval Double #-}
{-# SPECIALIZE C.expm1I :: Interval Double -> Interval Double #-}
{-# SPECIALIZE C.logP :: Double -> Interval Double #-}
{-# SPECIALIZE C.logI :: Interval Double -> Interval Double #-}
{-# SPECIALIZE C.log1pP :: Double -> Interval Double #-}
{-# SPECIALIZE C.log1pI :: Interval Double -> Interval Double #-}
{-# SPECIALIZE C.sin_small :: Interval Double -> Interval Double #-}
{-# SPECIALIZE C.cos_small :: Interval Double -> Interval Double #-}
{-# SPECIALIZE C.sinP :: Interval Double -> Interval Double #-}
{-# SPECIALIZE C.cosP :: Interval Double -> Interval Double #-}
{-# SPECIALIZE C.sinI :: Interval Double -> Interval Double #-}
{-# SPECIALIZE C.cosI :: Interval Double -> Interval Double #-}
{-# SPECIALIZE C.tanI :: Interval Double -> Interval Double #-}
{-# SPECIALIZE C.atan_small :: Interval Double -> Interval Double #-}
{-# SPECIALIZE C.atanP :: Double -> Interval Double #-}
{-# SPECIALIZE C.atanI :: Interval Double -> Interval Double #-}
{-# SPECIALIZE C.asinP :: Double -> Interval Double #-}
{-# SPECIALIZE C.asinI :: Interval Double -> Interval Double #-}
{-# SPECIALIZE C.acosP :: Double -> Interval Double #-}
{-# SPECIALIZE C.acosI :: Interval Double -> Interval Double #-}
{-# SPECIALIZE C.sinhP :: Double -> Interval Double #-}
{-# SPECIALIZE C.sinhI :: Interval Double -> Interval Double #-}
{-# SPECIALIZE C.coshP :: Double -> Interval Double #-}
{-# SPECIALIZE C.coshI :: Interval Double -> Interval Double #-}
{-# SPECIALIZE C.tanhP :: Double -> Interval Double #-}
{-# SPECIALIZE C.tanhI :: Interval Double -> Interval Double #-}
{-# SPECIALIZE C.asinhP :: Double -> Interval Double #-}
{-# SPECIALIZE C.asinhI :: Interval Double -> Interval Double #-}
{-# SPECIALIZE C.acoshP :: Double -> Interval Double #-}
{-# SPECIALIZE C.acoshI :: Interval Double -> Interval Double #-}
{-# SPECIALIZE C.atanhP :: Double -> Interval Double #-}
{-# SPECIALIZE C.atanhI :: Interval Double -> Interval Double #-}

instance (Num a, RoundedFractional a, RoundedSqrt a, Eq a, RealFloat a, RealFloatConstants a) => Floating (Interval a) where
  pi = I pi_down pi_up
  exp = C.expI
  log = C.logI
  sqrt = C.sqrtI
  -- x ** y = exp (log x * y) -- default
  -- logBase x y = log y / log x -- default
  sin = C.sinI
  cos = C.cosI
  tan = C.tanI
  asin = C.asinI
  acos = C.acosI
  atan = C.atanI
  sinh = C.sinhI
  cosh = C.coshI
  tanh = C.tanhI
  asinh = C.asinhI
  acosh = C.acoshI
  atanh = C.atanhI
  log1p = C.log1pI
  expm1 = C.expm1I
  -- log1pexp x = log (1 + exp x) -- default
  -- log1mexp x = log (1 - exp x) -- default
  {-# SPECIALIZE instance Floating (Interval Float) #-}
  {-# SPECIALIZE instance Floating (Interval Double) #-}

instance (RealFloat a, RoundedRing a) => C.IsInterval (Interval a) where
  type EndPoint (Interval a) = a
  makeInterval = I
  width = width
  withEndPoints f (I x y) = f x y
  hull = hull
  intersection (I x y) (I x' y') | getRounded x'' <= getRounded y'' = I x'' y''
                                 | otherwise = error "empty intersection"
    where x'' = max x x'
          y'' = min y y'
  maybeIntersection (I x y) (I x' y') | getRounded x'' <= getRounded y'' = Just (I x'' y'')
                                      | otherwise = Nothing
    where x'' = max x x'
          y'' = min y y'
  equalAsSet (I x y) (I x' y') = x == x' && y == y'
  subset (I x y) (I x' y') = x' <= x && y <= y'
  weaklyLess (I x y) (I x' y') = x <= x' && y <= y'
  precedes (I _ y) (I x' _) = getRounded y <= getRounded x'
  interior (I x y) (I x' y') = getRounded x' <# getRounded x && getRounded y <# getRounded y'
    where s <# t = s < t || (s == t && isInfinite s)
  strictLess (I x y) (I x' y') = getRounded x <# getRounded x' && getRounded y <# getRounded y'
    where s <# t = s < t || (s == t && isInfinite s)
  strictPrecedes (I _ y) (I x' _) = getRounded y < getRounded x'
  disjoint (I x y) (I x' y') = getRounded y < getRounded x' || getRounded y' < getRounded x
  {-# INLINE makeInterval #-}
  {-# INLINE width #-}
  {-# INLINE withEndPoints #-}
  {-# INLINE hull #-}
  {-# INLINE intersection #-}
  {-# INLINE maybeIntersection #-}
  {-# INLINE equalAsSet #-}
  {-# INLINE subset #-}
  {-# INLINE weaklyLess #-}
  {-# INLINE precedes #-}
  {-# INLINE interior #-}
  {-# INLINE strictLess #-}
  {-# INLINE strictPrecedes #-}
  {-# INLINE disjoint #-}

--
-- Instance for Data.Vector.Unboxed.Unbox
--

newtype instance VUM.MVector s (Interval a) = MV_Interval (VUM.MVector s (a, a))
newtype instance VU.Vector (Interval a) = V_Interval (VU.Vector (a, a))

intervalToPair :: Fractional a => Interval a -> (a, a)
intervalToPair (I (Rounded x) (Rounded y)) = (x, y)
{-# INLINE intervalToPair #-}

pairToInterval :: Ord a => (a, a) -> Interval a
pairToInterval (x, y) = I (Rounded x) (Rounded y)
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
    arr@(MutableByteArray arr_) <- newByteArray (2 * sizeOf (undefined :: a) * n)
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
