{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Numeric.Rounded.Hardware.Vector.Unboxed
  ( roundedSum
  , zipWith_roundedAdd
  , zipWith_roundedSub
  , zipWith_roundedMul
  , zipWith3_roundedFusedMultiplyAdd
  , zipWith_roundedDiv
  , map_roundedSqrt
  , sum
  , zipWith_add
  , zipWith_sub
  , zipWith_mul
  , zipWith3_fusedMultiplyAdd
  , zipWith_div
  , map_sqrt
  ) where
import           Data.Coerce
import           Data.Proxy
import qualified Data.Vector.Unboxed as VU
import           Numeric.Rounded.Hardware.Internal
import           Prelude hiding (sum)

-- | Equivalent to 'VU.sum'
sum :: forall r a. (Rounding r, VU.Unbox a, RoundedRing_Vector VU.Vector a) => VU.Vector (Rounded r a) -> Rounded r a
sum = coerce (roundedSum r :: VU.Vector a -> a)
  where r = rounding (Proxy :: Proxy r)
{-# INLINE sum #-}

-- | Equivalent to @'VU.zipWith' (+)@
zipWith_add :: forall r a. (Rounding r, VU.Unbox a, RoundedRing_Vector VU.Vector a) => VU.Vector (Rounded r a) -> VU.Vector (Rounded r a) -> VU.Vector (Rounded r a)
zipWith_add = coerce (zipWith_roundedAdd r :: VU.Vector a -> VU.Vector a -> VU.Vector a)
  where r = rounding (Proxy :: Proxy r)
{-# INLINE zipWith_add #-}

-- | Equivalent to @'VU.zipWith' (-)@
zipWith_sub :: forall r a. (Rounding r, VU.Unbox a, RoundedRing_Vector VU.Vector a) => VU.Vector (Rounded r a) -> VU.Vector (Rounded r a) -> VU.Vector (Rounded r a)
zipWith_sub = coerce (zipWith_roundedSub r :: VU.Vector a -> VU.Vector a -> VU.Vector a)
  where r = rounding (Proxy :: Proxy r)
{-# INLINE zipWith_sub #-}

-- | Equivalent to @'VU.zipWith' (*)@
zipWith_mul :: forall r a. (Rounding r, VU.Unbox a, RoundedRing_Vector VU.Vector a) => VU.Vector (Rounded r a) -> VU.Vector (Rounded r a) -> VU.Vector (Rounded r a)
zipWith_mul = coerce (zipWith_roundedMul r :: VU.Vector a -> VU.Vector a -> VU.Vector a)
  where r = rounding (Proxy :: Proxy r)
{-# INLINE zipWith_mul #-}

-- | Equivalent to @'VU.zipWith3' fusedMultiplyAdd@
zipWith3_fusedMultiplyAdd :: forall r a. (Rounding r, VU.Unbox a, RoundedRing_Vector VU.Vector a) => VU.Vector (Rounded r a) -> VU.Vector (Rounded r a) -> VU.Vector (Rounded r a) -> VU.Vector (Rounded r a)
zipWith3_fusedMultiplyAdd = coerce (zipWith3_roundedFusedMultiplyAdd r :: VU.Vector a -> VU.Vector a -> VU.Vector a -> VU.Vector a)
  where r = rounding (Proxy :: Proxy r)
{-# INLINE zipWith3_fusedMultiplyAdd #-}

-- | Equivalent to @'VU.zipWith' (/)@
zipWith_div :: forall r a. (Rounding r, VU.Unbox a, RoundedFractional_Vector VU.Vector a) => VU.Vector (Rounded r a) -> VU.Vector (Rounded r a) -> VU.Vector (Rounded r a)
zipWith_div = coerce (zipWith_roundedDiv r :: VU.Vector a -> VU.Vector a -> VU.Vector a)
  where r = rounding (Proxy :: Proxy r)
{-# INLINE zipWith_div #-}

-- | Equivalent to @'VU.map' sqrt@
map_sqrt :: forall r a. (Rounding r, VU.Unbox a, RoundedSqrt_Vector VU.Vector a) => VU.Vector (Rounded r a) -> VU.Vector (Rounded r a)
map_sqrt = coerce (map_roundedSqrt r :: VU.Vector a -> VU.Vector a)
  where r = rounding (Proxy :: Proxy r)
{-# INLINE map_sqrt #-}
