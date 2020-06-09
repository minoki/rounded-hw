{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Numeric.Rounded.Hardware.Vector.Storable
  ( coercion
  , fromVectorOfRounded
  , toVectorOfRounded
  , coercionM
  , fromMVectorOfRounded
  , toMVectorOfRounded
  , roundedSum
  , zipWith_roundedAdd
  , zipWith_roundedSub
  , zipWith_roundedMul
  , zipWith_roundedDiv
  , map_roundedSqrt
  , sum
  , zipWith_add
  , zipWith_sub
  , zipWith_mul
  , zipWith_div
  , map_sqrt
  ) where
import           Data.Coerce
import           Data.Proxy
import           Data.Type.Coercion
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import           Foreign.Storable
import           Numeric.Rounded.Hardware.Internal
import           Prelude hiding (sum)
import           Unsafe.Coerce

--
-- Conversion between 'VS.Vector a' and 'VS.Vector (Rounded r a)'
--
-- 'VS.Vector' will be nominally roled after vector-0.13.
-- See:
--     * https://github.com/haskell/vector/issues/223
--     * https://github.com/haskell/vector/pull/235
--
-- But, we know 'Storable (Rounded r a)' is the same as 'Storable a'
--

coercion :: Coercion (VS.Vector a) (VS.Vector (Rounded r a))
coercion = unsafeCoerce (Coercion :: Coercion (VS.Vector a) (VS.Vector a))

fromVectorOfRounded :: VS.Vector (Rounded r a) -> VS.Vector a
fromVectorOfRounded = unsafeCoerce

toVectorOfRounded :: VS.Vector a -> VS.Vector (Rounded r a)
toVectorOfRounded = unsafeCoerce

coercionM :: Coercion (VSM.MVector s a) (VSM.MVector s (Rounded r a))
coercionM = unsafeCoerce (Coercion :: Coercion (VSM.MVector s a) (VSM.MVector s a))

fromMVectorOfRounded :: VSM.MVector s (Rounded r a) -> VSM.MVector s a
fromMVectorOfRounded = unsafeCoerce

toMVectorOfRounded :: VSM.MVector s a -> VSM.MVector s (Rounded r a)
toMVectorOfRounded = unsafeCoerce

--
-- Vector Operations
--

-- | Equivalent to 'VS.sum'
sum :: forall r a. (Rounding r, Storable a, RoundedRing_Vector VS.Vector a) => VS.Vector (Rounded r a) -> Rounded r a
sum v = coerce (roundedSum r (fromVectorOfRounded v))
  where r = rounding (Proxy :: Proxy r)
{-# INLINE sum #-}

-- | Equivalent to @'VS.zipWith' (+)@
zipWith_add :: forall r a. (Rounding r, Storable a, RoundedRing_Vector VS.Vector a) => VS.Vector (Rounded r a) -> VS.Vector (Rounded r a) -> VS.Vector (Rounded r a)
zipWith_add v1 v2 = toVectorOfRounded (zipWith_roundedAdd r (fromVectorOfRounded v1) (fromVectorOfRounded v2))
  where r = rounding (Proxy :: Proxy r)
{-# INLINE zipWith_add #-}

-- | Equivalent to @'VS.zipWith' (-)@
zipWith_sub :: forall r a. (Rounding r, Storable a, RoundedRing_Vector VS.Vector a) => VS.Vector (Rounded r a) -> VS.Vector (Rounded r a) -> VS.Vector (Rounded r a)
zipWith_sub v1 v2 = toVectorOfRounded (zipWith_roundedSub r (fromVectorOfRounded v1) (fromVectorOfRounded v2))
  where r = rounding (Proxy :: Proxy r)
{-# INLINE zipWith_sub #-}

-- | Equivalent to @'VS.zipWith' (*)@
zipWith_mul :: forall r a. (Rounding r, Storable a, RoundedRing_Vector VS.Vector a) => VS.Vector (Rounded r a) -> VS.Vector (Rounded r a) -> VS.Vector (Rounded r a)
zipWith_mul v1 v2 = toVectorOfRounded (zipWith_roundedMul r (fromVectorOfRounded v1) (fromVectorOfRounded v2))
  where r = rounding (Proxy :: Proxy r)
{-# INLINE zipWith_mul #-}

-- | Equivalent to @'VS.zipWith' (/)@
zipWith_div :: forall r a. (Rounding r, Storable a, RoundedFractional_Vector VS.Vector a) => VS.Vector (Rounded r a) -> VS.Vector (Rounded r a) -> VS.Vector (Rounded r a)
zipWith_div v1 v2 = toVectorOfRounded (zipWith_roundedDiv r (fromVectorOfRounded v1) (fromVectorOfRounded v2))
  where r = rounding (Proxy :: Proxy r)
{-# INLINE zipWith_div #-}

-- | Equivalent to @'VS.map' sqrt@
map_sqrt :: forall r a. (Rounding r, Storable a, RoundedSqrt_Vector VS.Vector a) => VS.Vector (Rounded r a) -> VS.Vector (Rounded r a)
map_sqrt v = toVectorOfRounded (map_roundedSqrt r (fromVectorOfRounded v))
  where r = rounding (Proxy :: Proxy r)
{-# INLINE map_sqrt #-}
