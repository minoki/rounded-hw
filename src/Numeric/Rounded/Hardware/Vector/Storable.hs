{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Numeric.Rounded.Hardware.Vector.Storable
  ( roundedSum
  , sum
  ) where
import           Data.Proxy
import qualified Data.Vector.Storable              as VS
import           Foreign.Storable
import           Numeric.Rounded.Hardware.Internal
import           Prelude                           hiding (sum)
import           Unsafe.Coerce

-- We know 'Storable (Rounded r a)' is the same as 'Storable a'
unwrapVectorOfRounded :: Storable a => VS.Vector (Rounded r a) -> VS.Vector a
unwrapVectorOfRounded = unsafeCoerce

roundedSum :: (Storable a, RoundedVectorOperation a) => RoundingMode -> VS.Vector a -> a
roundedSum = roundedSum_StorableVector

sum :: forall r a. (Rounding r, Storable a, RoundedVectorOperation a) => VS.Vector (Rounded r a) -> Rounded r a
sum vec = let mode = rounding (Proxy :: Proxy r)
          in Rounded (roundedSum mode (unwrapVectorOfRounded vec))
