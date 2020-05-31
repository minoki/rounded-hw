{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Numeric.Rounded.Hardware.Vector.Storable
  ( roundedSum
  , sum
  ) where
import           Data.Proxy
import qualified Data.Vector.Storable as VS
import           Foreign.Storable
import           Numeric.Rounded.Hardware.Internal
import           Prelude hiding (sum)
import           Unsafe.Coerce

-- We know 'Storable (Rounded r a)' is the same as 'Storable a'
unwrapVectorOfRounded :: Storable a => VS.Vector (Rounded r a) -> VS.Vector a
unwrapVectorOfRounded = unsafeCoerce

sum :: forall r a. (Rounding r, Storable a, RoundedRing_Vector VS.Vector a) => VS.Vector (Rounded r a) -> Rounded r a
sum vec = let mode = rounding (Proxy :: Proxy r)
          in Rounded (roundedSum mode (unwrapVectorOfRounded vec))
