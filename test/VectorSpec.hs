{-# LANGUAGE DataKinds #-}
module VectorSpec where
import           Data.Proxy
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import           Numeric (showHFloat)
import           Numeric.Rounded.Hardware.Internal
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck
import           Text.Show (showListWith)
import           Util

infix 4 ==^
(==^) :: (VG.Vector vector a, RealFloat a, Show a) => vector a -> vector a -> Property
(==^) v1 v2 = counterexample (showListWith showHFloat (VG.toList v1) . showString (interpret res) . showListWith showHFloat (VG.toList v2) $ "") res
  where
    res = VG.eqBy sameFloat v1 v2
    interpret True  = " === "
    interpret False = " =/= "

arbitraryVector :: (Arbitrary a, VG.Vector vector a) => Gen (vector a)
arbitraryVector = VG.fromList <$> arbitrary

prop_roundedSum :: (RealFloat a, Show a, RoundedRing_Vector vector a, VG.Vector vector a) => Proxy (vector a) -> RoundingMode -> vector a -> Property
prop_roundedSum _proxy r v = VG.foldl' (roundedAdd r) 0 v `sameFloatP` roundedSum r v

prop_roundedAdd :: (RealFloat a, Show a, RoundedRing_Vector vector a, VG.Vector vector a) => Proxy (vector a) -> RoundingMode -> vector a -> vector a -> Property
prop_roundedAdd _proxy r v1 v2 = VG.zipWith (roundedAdd r) v1 v2 ==^ zipWith_roundedAdd r v1 v2

prop_roundedSub :: (RealFloat a, Show a, RoundedRing_Vector vector a, VG.Vector vector a) => Proxy (vector a) -> RoundingMode -> vector a -> vector a -> Property
prop_roundedSub _proxy r v1 v2 = VG.zipWith (roundedSub r) v1 v2 ==^ zipWith_roundedSub r v1 v2

prop_roundedMul :: (RealFloat a, Show a, RoundedRing_Vector vector a, VG.Vector vector a) => Proxy (vector a) -> RoundingMode -> vector a -> vector a -> Property
prop_roundedMul _proxy r v1 v2 = VG.zipWith (roundedMul r) v1 v2 ==^ zipWith_roundedMul r v1 v2

prop_roundedDiv :: (RealFloat a, Show a, RoundedFractional_Vector vector a, VG.Vector vector a) => Proxy (vector a) -> RoundingMode -> vector a -> vector a -> Property
prop_roundedDiv _proxy r v1 v2 = VG.zipWith (roundedDiv r) v1 v2 ==^ zipWith_roundedDiv r v1 v2

prop_roundedSqrt :: (RealFloat a, Show a, RoundedSqrt_Vector vector a, VG.Vector vector a) => Proxy (vector a) -> RoundingMode -> vector a -> Property
prop_roundedSqrt _proxy r v = VG.map (roundedSqrt r) v ==^ map_roundedSqrt r v

specT :: (RealFloat a, Arbitrary a, Show a, Show (vector a), VG.Vector vector a, RoundedFractional_Vector vector a, RoundedSqrt_Vector vector a) => Proxy (vector a) -> Spec
specT proxy = do
  prop "roundedSum" $ forAll arbitraryVector $ \v r -> prop_roundedSum proxy r v
  prop "roundedAdd" $ forAll arbitraryVector $ \v1 -> forAll arbitraryVector $ \v2 r -> prop_roundedAdd proxy r v1 v2
  prop "roundedSub" $ forAll arbitraryVector $ \v1 -> forAll arbitraryVector $ \v2 r -> prop_roundedSub proxy r v1 v2
  prop "roundedMul" $ forAll arbitraryVector $ \v1 -> forAll arbitraryVector $ \v2 r -> prop_roundedMul proxy r v1 v2
  prop "roundedDiv" $ forAll arbitraryVector $ \v1 -> forAll arbitraryVector $ \v2 r -> prop_roundedDiv proxy r v1 v2
  prop "roundedSqrt" $ forAll arbitraryVector $ \v r -> prop_roundedSqrt proxy r v

spec :: Spec
spec = do
  describe "Storable Double" $ specT (Proxy :: Proxy (VS.Vector Double))
  describe "Storable Float" $ specT (Proxy :: Proxy (VS.Vector Float))
  describe "Unboxed Double" $ specT (Proxy :: Proxy (VU.Vector Double))
  describe "Unboxed Float" $ specT (Proxy :: Proxy (VU.Vector Float))
