{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Numeric.Rounded.Hardware.Backend.C where
import           Control.DeepSeq                             (NFData (..))
import           Data.Coerce
import           Data.Functor.Product
import           Data.Ratio
import           FFIImports
import qualified FFIWrapper.Double                           as D
import qualified FFIWrapper.Float                            as F
import           GHC.Generics                                (Generic)
import           Numeric.Rounded.Hardware.Class
import           Numeric.Rounded.Hardware.Rounding
import           Numeric.Rounded.Hardware.Util.Constants
import           Numeric.Rounded.Hardware.Util.Conversion
import qualified Numeric.Rounded.Hardware.Util.RoundedResult as RR

--
-- Float
--

newtype CFloat = CFloat Float
  deriving (Eq,Ord,Show,Generic,Num)

instance NFData CFloat

instance RoundedRing CFloat where
  roundedAdd = coerce F.roundedAdd
  roundedSub = coerce F.roundedSub
  roundedMul = coerce F.roundedMul
  intervalMul x x' y y' = (coerce c_interval_mul_float_down x x' y y', coerce c_interval_mul_float_up x x' y y')
  roundedFromInteger rn x = CFloat (fromInt rn x)
  intervalFromInteger x = case fromIntF x :: Product (RR.Rounded TowardNegInf) (RR.Rounded TowardInf) Float of
    Pair (RR.Rounded a) (RR.Rounded b) -> (CFloat a, CFloat b)
  {-# INLINE roundedAdd #-}
  {-# INLINE roundedSub #-}
  {-# INLINE roundedMul #-}
  {-# INLINE intervalMul #-}
  {-# INLINE roundedFromInteger #-}
  {-# INLINE intervalFromInteger #-}

instance RoundedField CFloat where
  roundedDiv = coerce F.roundedDiv
  intervalDiv x x' y y' = (coerce c_interval_div_float_down x x' y y', coerce c_interval_div_float_up x x' y y')
  roundedFromRational rn x = CFloat $ fromRatio rn (numerator x) (denominator x)
  intervalFromRational x = case fromRatioF (numerator x) (denominator x) :: Product (RR.Rounded TowardNegInf) (RR.Rounded TowardInf) Float of
    Pair (RR.Rounded a) (RR.Rounded b) -> (CFloat a, CFloat b)
  {-# INLINE roundedDiv #-}
  {-# INLINE intervalDiv #-}
  {-# INLINE roundedFromRational #-}
  {-# INLINE intervalFromRational #-}

instance RoundedSqrt CFloat where
  roundedSqrt = coerce F.roundedSqrt
  {-# INLINE roundedSqrt #-}

--
-- Double
--

newtype CDouble = CDouble Double
  deriving (Eq,Ord,Show,Generic,Num)

instance NFData CDouble

instance RoundedRing CDouble where
  roundedAdd = coerce D.roundedAdd
  roundedSub = coerce D.roundedSub
  roundedMul = coerce D.roundedMul
  intervalMul x x' y y' = (coerce c_interval_mul_double_down x x' y y', coerce c_interval_mul_double_up x x' y y')
  roundedFromInteger rn x = CDouble (fromInt rn x)
  intervalFromInteger x = case fromIntF x :: Product (RR.Rounded TowardNegInf) (RR.Rounded TowardInf) Double of
    Pair (RR.Rounded a) (RR.Rounded b) -> (CDouble a, CDouble b)
  {-# INLINE roundedAdd #-}
  {-# INLINE roundedSub #-}
  {-# INLINE roundedMul #-}
  {-# INLINE intervalMul #-}
  {-# INLINE roundedFromInteger #-}
  {-# INLINE intervalFromInteger #-}

instance RoundedField CDouble where
  roundedDiv = coerce D.roundedDiv
  intervalDiv x x' y y' = (coerce c_interval_div_double_down x x' y y', coerce c_interval_div_double_up x x' y y')
  roundedFromRational rn x = CDouble $ fromRatio rn (numerator x) (denominator x)
  intervalFromRational x = case fromRatioF (numerator x) (denominator x) :: Product (RR.Rounded TowardNegInf) (RR.Rounded TowardInf) Double of
    Pair (RR.Rounded a) (RR.Rounded b) -> (CDouble a, CDouble b)
  {-# INLINE roundedDiv #-}
  {-# INLINE intervalDiv #-}
  {-# INLINE roundedFromRational #-}
  {-# INLINE intervalFromRational #-}

instance RoundedSqrt CDouble where
  roundedSqrt = coerce D.roundedSqrt
  {-# INLINE roundedSqrt #-}
