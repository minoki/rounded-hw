{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Numeric.Rounded.Hardware.Vector.Unboxed
  ( roundedSum
  , sum
  ) where
import           Data.Proxy
import qualified Data.Vector.Unboxed              as VU
import           Numeric.Rounded.Hardware.Internal
import           Prelude                           hiding (sum)

roundedSum :: (VU.Unbox a, RoundedVectorOperation a) => RoundingMode -> VU.Vector a -> a
roundedSum = roundedSum_UnboxedVector

sum :: forall rn a. (Rounding rn, VU.Unbox a, RoundedVectorOperation a) => VU.Vector (Rounded rn a) -> Rounded rn a
sum (V_Rounded vec) = let mode = rounding (Proxy :: Proxy rn)
                      in Rounded (roundedSum mode vec)
