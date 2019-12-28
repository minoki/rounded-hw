{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Numeric.Rounded.Hardware.Backend.ViaRational where
import           Control.DeepSeq                             (NFData (..))
import           Data.Functor.Product
import           Data.Ratio
import           GHC.Generics                                (Generic)
import           Numeric.Rounded.Hardware.Base.Class
import           Numeric.Rounded.Hardware.Base.Constants
import           Numeric.Rounded.Hardware.Base.Conversion

newtype ViaRational a = ViaRational a
  deriving (Eq,Ord,Show,Generic,Num)

instance NFData a => NFData (ViaRational a)

instance (RealFloat a, Num a, RealFloatConstants a) => RoundedRing (ViaRational a) where
  roundedAdd rn (ViaRational x) (ViaRational y)
    | isNaN x || isNaN y || isInfinite x || isInfinite y = ViaRational (x + y)
    | x == 0 && y == 0 = ViaRational $ case rn of
                                         _ | isNegativeZero x == isNegativeZero y -> x
                                         TowardNearest -> 0
                                         TowardNegInf -> -0
                                         TowardInf -> 0
                                         TowardZero -> 0
    | otherwise = roundedFromRational rn (toRational x + toRational y)
  roundedSub rn (ViaRational x) (ViaRational y)
    | isNaN x || isNaN y || isInfinite x || isInfinite y = ViaRational (x - y)
    | x == 0 && y == 0 = ViaRational $ case rn of
                                         _ | isNegativeZero x /= isNegativeZero y -> x
                                         TowardNearest -> 0
                                         TowardNegInf -> -0
                                         TowardInf -> 0
                                         TowardZero -> 0
    | otherwise = roundedFromRational rn (toRational x - toRational y)
  roundedMul rn (ViaRational x) (ViaRational y)
    | isNaN x || isNaN y || isInfinite x || isInfinite y || isNegativeZero x || isNegativeZero y = ViaRational (x * y)
    | otherwise = roundedFromRational rn (toRational x * toRational y)
  roundedFromInteger rn x = ViaRational (fromInt rn x)
  intervalFromInteger x = case fromIntF x :: Product (Rounded 'TowardNegInf) (Rounded 'TowardInf) a of
    Pair a b -> (ViaRational <$> a, ViaRational <$> b)
  {-# INLINE roundedFromInteger #-}
  {-# INLINE intervalFromInteger #-}
  {-# SPECIALIZE instance RoundedRing (ViaRational Float) #-}
  {-# SPECIALIZE instance RoundedRing (ViaRational Double) #-}

instance (RealFloat a, Num a, RealFloatConstants a) => RoundedFractional (ViaRational a) where
  roundedDiv rn (ViaRational x) (ViaRational y)
    | isNaN x || isNaN y || isInfinite x || isInfinite y || x == 0 || y == 0 = ViaRational (x / y)
    | otherwise = roundedFromRational rn (toRational x / toRational y)
  roundedFromRational rn x = ViaRational $ fromRatio rn (numerator x) (denominator x)
  intervalFromRational x = case fromRatioF (numerator x) (denominator x) :: Product (Rounded 'TowardNegInf) (Rounded 'TowardInf) a of
    Pair a b -> (ViaRational <$> a, ViaRational <$> b)
  {-# INLINE roundedFromRational #-}
  {-# INLINE intervalFromRational #-}
  {-# SPECIALIZE instance RoundedFractional (ViaRational Float) #-}
  {-# SPECIALIZE instance RoundedFractional (ViaRational Double) #-}

backendName :: String
backendName = "via Rational"
