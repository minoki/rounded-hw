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

-- We know 'Storable (Rounded rn a)' is the same as 'Storable a'
unwrapVectorOfRounded :: Storable a => VS.Vector (Rounded rn a) -> VS.Vector a
unwrapVectorOfRounded = unsafeCoerce

roundedSum :: (Storable a, RoundedVectorOperation a) => RoundingMode -> VS.Vector a -> a
roundedSum = roundedSum_StorableVector

sum :: forall rn a. (Rounding rn, Storable a, RoundedVectorOperation a) => VS.Vector (Rounded rn a) -> Rounded rn a
sum vec = let mode = rounding (Proxy :: Proxy rn)
          in Rounded (roundedSum mode (unwrapVectorOfRounded vec))
