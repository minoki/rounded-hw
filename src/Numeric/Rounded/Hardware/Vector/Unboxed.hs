{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Numeric.Rounded.Hardware.Vector.Unboxed
  ( roundedSum
  , sum
  ) where
import           Data.Proxy
import qualified Data.Vector.Unboxed as VU
import           Numeric.Rounded.Hardware.Internal
import           Prelude hiding (sum)

sum :: forall r a. (Rounding r, VU.Unbox a, RoundedRing_Vector VU.Vector a) => VU.Vector (Rounded r a) -> Rounded r a
sum (V_Rounded vec) = let mode = rounding (Proxy :: Proxy r)
                      in Rounded (roundedSum mode vec)
