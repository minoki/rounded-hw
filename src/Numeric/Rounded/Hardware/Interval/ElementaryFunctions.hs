{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Numeric.Rounded.Hardware.Interval.ElementaryFunctions where
import Numeric.Rounded.Hardware.Internal
import Numeric.Rounded.Hardware.Interval.Class

sqrtI :: (IsInterval i, RoundedSqrt (EndPoint i)) => i -> i
sqrtI = withEndPoints $ \x y -> case intervalSqrt x y of (u, v) -> makeInterval u v
{-# INLINE sqrtI #-}

expP :: forall i. (IsInterval i, Fractional i, Eq (EndPoint i), RealFloat (EndPoint i), RealFloatConstants (EndPoint i)) => EndPoint i -> i
expP x | isInfinite x = if x > 0
                        then makeInterval (Rounded maxFinite) (Rounded positiveInfinity)
                        else makeInterval (Rounded 0) (Rounded minPositive)
expP x = let a = round x
             b = x - fromInteger a -- -1/2 <= b && b <= 1/2
             b' = singleton b
             series :: Int -> i -> i
             series n acc | n == 0 = acc
                          | otherwise = series (n-1) $ 1 + acc * b' / fromIntegral n
         in if fromInteger a + b == x && abs b <= 0.5
            then (makeInterval exp1_down exp1_up)^^a * series 15 (makeInterval expm1_2_down exp1_2_up)
            else error "rounding error"

expI :: (IsInterval i, Fractional i, Eq (EndPoint i), RealFloat (EndPoint i), RealFloatConstants (EndPoint i)) => i -> i
expI = withEndPoints (\(Rounded x) (Rounded y) -> hull (expP x) (expP y)) -- increasing

expm1P :: forall i. (IsInterval i, Fractional i, Eq (EndPoint i), RealFloat (EndPoint i), RealFloatConstants (EndPoint i)) => EndPoint i -> i
expm1P x | -0.5 <= x && x <= 0.5 = let b' = singleton x
                                       series :: Int -> i -> i
                                       series n acc | n == 1 = acc * b'
                                                    | otherwise = series (n-1) $ 1 + acc * b' / fromIntegral n
                                   in series 15 (makeInterval expm1_2_down exp1_2_up)
         | otherwise = expP x - 1

expm1I :: (IsInterval i, Fractional i, Eq (EndPoint i), RealFloat (EndPoint i), RealFloatConstants (EndPoint i)) => i -> i
expm1I = withEndPoints (\(Rounded x) (Rounded y) -> hull (expm1P x) (expm1P y)) -- increasing

logP :: forall i. (IsInterval i, Fractional i, Eq (EndPoint i), RealFloat (EndPoint i), RealFloatConstants (EndPoint i)) => EndPoint i -> i
logP x
  | floatRadix (undefined :: EndPoint i) /= 2 = error "Unsupported float radix"
  | x < 0 = error "Negative logarithm"
  | x == 0 = makeInterval (Rounded negativeInfinity) (Rounded (-maxFinite))
  | isInfinite x = makeInterval (Rounded maxFinite) (Rounded positiveInfinity)
  | isNaN x = error "NaN"
  | otherwise = let m :: Integer
                    n :: Int
                    (m,n) = decodeFloat x
                    -- x = m * 2^n, 2^(d-1) <= m < 2^d
                    -- x = (m * 2^(-d)) * 2^(n+d)
                    -- x = 2^a * b, a \in {.. -1.5, -1, -0.5, 0, 0.5, 1, 1.5 ..}, 1/\sqrt{2} < b < \sqrt{2}
                    a0, b, bm1 :: i
                    a0 = fromIntegral (n + d) -- fromIntegral (exponent x)
                    x' :: EndPoint i
                    x' = encodeFloat m (- d) -- significand x
                    -- 0.5 <= x' < 1
                    (a,b) | 0.5 <= x' && x' <= getRounded two_minus_sqrt2_down = (a0 - 1, singleton x' * 2) -- 1/2 <= x <= 2 - sqrt 2 => 1 <= 2*x <= 4 - 2 * sqrt 2
                          | getRounded two_minus_sqrt2_up <= x' && x' <= 2 * getRounded sqrt2m1_down = (a0 - 0.5, singleton x' * sqrt2_iv) -- 2 - sqrt2 <= x <= 2 * sqrt 2 - 2, 2 * sqrt 2 - 2 <= sqrt 2 * x <= 4 - 2 * sqrt 2
                          | 2 * getRounded sqrt2m1_up <= x' && x' < 1 = (a0, singleton x') -- 2 * sqrt 2 - 2 <= x
                          | otherwise = error "interval log: internal error"
                    -- 2 * sqrt 2 - 2 <= b <= 4 - 2 * sqrt 2
                    -- 2 * sqrt 2 - 3 <= b-1 <= 3 - 2 * sqrt 2
                    bm1 = b - 1
                    series :: Int -> i -> i
                    series k acc | k == 0 = bm1 * acc
                                 | otherwise = series (k-1) $ recip (fromIntegral k) - bm1 * acc
                in a * log2_iv + series 21 (hull 1 b ^^ (-22 :: Int) * bm1 / fromInteger 22)
  where
    d = floatDigits (undefined :: EndPoint i)
    log2_iv :: i -- log_e 2
    log2_iv = makeInterval log2_down log2_up
    sqrt2_iv :: i -- sqrt 2
    sqrt2_iv = makeInterval sqrt2_down sqrt2_up

logI :: (IsInterval i, Fractional i, Eq (EndPoint i), RealFloat (EndPoint i), RealFloatConstants (EndPoint i)) => i -> i
logI = withEndPoints (\(Rounded x) (Rounded y) -> hull (logP x) (logP y)) -- increasing

log1pP :: forall i. (IsInterval i, Fractional i, Eq (EndPoint i), RealFloat (EndPoint i), RealFloatConstants (EndPoint i)) => EndPoint i -> i
log1pP x | - getRounded three_minus_2sqrt2_down <= x && x <= getRounded three_minus_2sqrt2_down =
             let x' :: i
                 x' = singleton x
                 series :: Int -> i -> i
                 series k acc | k == 0 = x' * acc
                              | otherwise = series (k-1) $ recip (fromIntegral k) - x' * acc
             in series 21 (hull 1 (x' + 1) ^^ (-22 :: Int) * x' / fromInteger 22)
         | otherwise = logI (singleton x + 1)

log1pI :: (IsInterval i, Fractional i, Eq (EndPoint i), RealFloat (EndPoint i), RealFloatConstants (EndPoint i)) => i -> i
log1pI = withEndPoints (\(Rounded x) (Rounded y) -> hull (log1pP x) (log1pP y)) -- increasing

-- abs x <= pi / 4
sin_small :: forall i. (IsInterval i, Fractional i, Eq (EndPoint i), RealFloat (EndPoint i), RoundedRing (EndPoint i), RealFloatConstants (EndPoint i)) => i -> i
sin_small x = let xx = x * x
                  series :: Int -> i -> i
                  series k acc | k == 0 = x * acc
                               | otherwise = series (k-2) $ 1 - xx * acc / fromIntegral (k * (k+1))
              in series 18 (makeInterval (-1) 1)

-- abs x <= pi / 4
cos_small :: forall i. (IsInterval i, Fractional i, Eq (EndPoint i), RealFloat (EndPoint i), RoundedRing (EndPoint i), RealFloatConstants (EndPoint i)) => i -> i
cos_small x = let xx = x * x
                  series :: Int -> i -> i
                  series k acc | k == 1 = acc
                               | otherwise = series (k-2) $ 1 - xx * acc / fromIntegral ((k-1) * (k-2))
              in series 17 (makeInterval (-1) 1)

-- -pi <= x <= pi
-- x should be a small interval
sinP :: forall i. (IsInterval i, Fractional i, Eq (EndPoint i), RealFloat (EndPoint i), RoundedRing (EndPoint i), RealFloatConstants (EndPoint i)) => i -> i
sinP x | x `weaklyLess` - three_pi_iv / 4 = - sin_small (x + pi_iv) -- -pi <= x <= -3/4*pi
       | x `weaklyLess` - pi_iv / 2 = - cos_small (- pi_iv / 2 - x) -- -3/4*pi <= x <= -pi/2
       | x `weaklyLess` - pi_iv / 4 = - cos_small (x + pi_iv / 2) -- -pi <= x <= -pi/4
       | x `weaklyLess` 0 = - sin_small (- x)
       | x `weaklyLess` pi_iv / 4 = sin_small x
       | x `weaklyLess` pi_iv / 2 = cos_small (pi_iv / 2 - x)
       | x `weaklyLess` three_pi_iv / 4 = cos_small (x - pi_iv / 2)
       | otherwise = sin_small (pi_iv - x)
  where
    pi_iv :: i
    pi_iv = makeInterval pi_down pi_up
    three_pi_iv :: i
    three_pi_iv = makeInterval three_pi_down three_pi_up
    -- TODO: Is `weaklyLess` okay?

-- -pi <= x <= pi
-- x should be a small interval
cosP :: forall i. (IsInterval i, Fractional i, Eq (EndPoint i), RealFloat (EndPoint i), RoundedRing (EndPoint i), RealFloatConstants (EndPoint i)) => i -> i
cosP x | x `weaklyLess` - three_pi_iv / 4 = - cos_small (x + pi_iv) -- -pi <= x <= -3/4*pi
       | x `weaklyLess` - pi_iv / 2 = - sin_small (- pi_iv / 2 - x) -- -3/4*pi <= x <= -pi/2
       | x `weaklyLess` - pi_iv / 4 = sin_small (x + pi_iv / 2) -- -pi <= x <= -pi/4
       | x `weaklyLess` 0 = cos_small (- x)
       | x `weaklyLess` pi_iv / 4 = cos_small x
       | x `weaklyLess` pi_iv / 2 = sin_small (pi_iv / 2 - x)
       | x `weaklyLess` three_pi_iv / 4 = - sin_small (x - pi_iv / 2)
       | otherwise = - cos_small (pi_iv - x)
  where
    pi_iv :: i
    pi_iv = makeInterval pi_down pi_up
    three_pi_iv :: i
    three_pi_iv = makeInterval three_pi_down three_pi_up
    -- TODO: Is `weaklyLess` okay?

sinI :: forall i. (IsInterval i, Fractional i, Eq (EndPoint i), RealFloat (EndPoint i), RoundedRing (EndPoint i), RealFloatConstants (EndPoint i)) => i -> i
sinI t = flip withEndPoints t $ \(Rounded x) (Rounded y) ->
  if isInfinite x || isInfinite y
  then wholeRange
  else flip withEndPoints (singleton x / (2 * pi_iv)) $ \(Rounded x0) (Rounded _) ->
    let n = round x0
        t' = t - 2 * pi_iv * fromInteger n
    in flip withEndPoints t' $ \(Rounded x') (Rounded y') ->
      if y' - x' >= getRounded (2 * pi_up)
      then wholeRange
      else -- -pi <= x' <= pi, x' <= y' <= 3 * pi
        let include_minus_1 = minus_half_pi_iv `subset` t' || three_pi_2_iv `subset` t'
            include_plus_1 = pi_iv / 2 `subset` t' || five_pi_2_iv `subset` t'
            u = hull (sinP $ singleton x') $ sinP (if y <= getRounded pi_down then singleton y' else singleton y' - 2 * pi_iv)
            v | include_minus_1 = hull (-1) u
              | otherwise = u
            w | include_plus_1 = hull 1 v
              | otherwise = v
        in intersection wholeRange w
  where
    pi_iv :: i
    pi_iv = makeInterval pi_down pi_up
    minus_half_pi_iv :: i
    minus_half_pi_iv = - pi_iv / 2
    three_pi_2_iv :: i
    three_pi_2_iv = makeInterval three_pi_down three_pi_up / 2
    five_pi_2_iv :: i
    five_pi_2_iv = makeInterval five_pi_down five_pi_up / 2
    wholeRange :: i
    wholeRange = makeInterval (-1) 1

cosI :: forall i. (IsInterval i, Fractional i, Eq (EndPoint i), RealFloat (EndPoint i), RoundedRing (EndPoint i), RealFloatConstants (EndPoint i)) => i -> i
cosI t = flip withEndPoints t $ \(Rounded x) (Rounded y) ->
  if isInfinite x || isInfinite y
  then wholeRange
  else flip withEndPoints (singleton x / (2 * pi_iv)) $ \(Rounded x0) (Rounded _) ->
    let n = round x0
        t' = t - 2 * pi_iv * fromInteger n
    in flip withEndPoints t' $ \(Rounded x') (Rounded y') ->
      if y' - x' >= getRounded (2 * pi_up)
      then wholeRange
      else -- -pi <= x' <= pi, x' <= y' <= 3 * pi
        let include_minus_1 = -pi_iv `subset` t' || pi_iv `subset` t' || three_pi_iv `subset` t'
            include_plus_1 = 0 `subset` t' || 2 * pi_iv `subset` t'
            u = hull (cosP $ singleton x') $ cosP (if y <= getRounded pi_down then singleton y' else singleton y' - 2 * pi_iv)
            v | include_minus_1 = hull (-1) u
              | otherwise = u
            w | include_plus_1 = hull 1 v
              | otherwise = v
        in intersection wholeRange w
  where
    pi_iv :: i
    pi_iv = makeInterval pi_down pi_up
    three_pi_iv :: i
    three_pi_iv = makeInterval three_pi_down three_pi_up
    wholeRange :: i
    wholeRange = makeInterval (-1) 1

tanI :: forall i. (IsInterval i, Fractional i, Eq (EndPoint i), RealFloat (EndPoint i), RoundedRing (EndPoint i), RealFloatConstants (EndPoint i)) => i -> i
tanI t = flip withEndPoints t $ \(Rounded x) (Rounded y) ->
  if isInfinite x || isInfinite y
  then wholeRange
  else flip withEndPoints (t / pi_iv) $ \(Rounded x0) (Rounded _) ->
    let n = round x0 -- abs (x - n) <= 1/2
        t' = t - pi_iv * fromInteger n
    in flip withEndPoints t' $ \(Rounded x') (Rounded y') ->
      -- -pi/2 < x' < pi/2
      if y' >= getRounded pi_up / 2
      then wholeRange
      else let lb = sinP (singleton x') / cosP (singleton x')
               ub = sinP (singleton y') / cosP (singleton y')
               -- lb <= ub
           in hull lb ub -- increasing in (-pi/2,pi/2)
  where
    pi_iv :: i
    pi_iv = makeInterval pi_down pi_up
    wholeRange :: i
    wholeRange = makeInterval (Rounded negativeInfinity) (Rounded positiveInfinity)

-- abs x <= 1 / (1 + sqrt 2) = sqrt 2 - 1
atan_small :: forall i. (IsInterval i, Fractional i, Eq (EndPoint i), RealFloat (EndPoint i), RoundedRing (EndPoint i), RealFloatConstants (EndPoint i)) => i -> i
atan_small x = let n = 39 -- odd
               in series n (makeInterval (-1) 1 / fromIntegral n)
  where
    xx = x * x
    series :: Int -> i -> i
    series k acc | k == 1 = x * acc
                 | otherwise = series (k-2) $ recip (fromIntegral (k-2)) - xx * acc

atanP :: forall i. (IsInterval i, Fractional i, Eq (EndPoint i), RealFloat (EndPoint i), RoundedRing (EndPoint i), RealFloatConstants (EndPoint i)) => EndPoint i -> i
atanP x |   getRounded (1 + sqrt2_up)   <= x =   pi_iv / 2 - atan_small (recip x')
        |   getRounded sqrt2m1_up       <= x =   pi_iv / 4 + atan_small ((x' - 1) / (x' + 1))
        | - getRounded sqrt2m1_down     <= x =               atan_small x'
        | - getRounded (sqrt2_down + 1) <= x = - pi_iv / 4 + atan_small ((1 + x') / (1 - x'))
        |   otherwise                        = - pi_iv / 2 - atan_small (recip x')
  where
    x' = singleton x
    pi_iv :: i
    pi_iv = makeInterval pi_down pi_up

atanI :: forall i. (IsInterval i, Fractional i, Eq (EndPoint i), RealFloat (EndPoint i), RoundedRing (EndPoint i), RealFloatConstants (EndPoint i)) => i -> i
atanI = withEndPoints (\(Rounded x) (Rounded y) -> hull (atanP x) (atanP y)) -- increasing

asinP :: forall i. (IsInterval i, Fractional i, Eq (EndPoint i), RealFloat (EndPoint i), RoundedRing (EndPoint i), RoundedSqrt (EndPoint i), RealFloatConstants (EndPoint i)) => EndPoint i -> i
asinP x | x < -1 || 1 < x = error "asin"
        | x == -1 = - pi_iv / 2
        | x == 1  =   pi_iv / 2
        | otherwise = atanI (x' / sqrtI (1 - x'*x')) -- TODO: Use sqrt ((1+x')*(1-x')) when |x| is near 1
  where
    x' = singleton x
    pi_iv :: i
    pi_iv = makeInterval pi_down pi_up

asinI :: forall i. (IsInterval i, Fractional i, Eq (EndPoint i), RealFloat (EndPoint i), RoundedRing (EndPoint i), RoundedSqrt (EndPoint i), RealFloatConstants (EndPoint i)) => i -> i
asinI = withEndPoints (\(Rounded x) (Rounded y) -> hull (asinP x) (asinP y)) -- increasing

acosP :: forall i. (IsInterval i, Fractional i, Eq (EndPoint i), RealFloat (EndPoint i), RoundedRing (EndPoint i), RoundedSqrt (EndPoint i), RealFloatConstants (EndPoint i)) => EndPoint i -> i
acosP x | x < -1 || 1 < x = error "asin"
        | x == -1 = pi_iv
        | x == 1  = 0
        | otherwise = case x' / sqrtI (1 - x'*x') of  -- TODO: Use sqrt ((1+x')*(1-x')) when |x| is near 1
                        y' |   one_plus_sqrt2  `weaklyLess` y' -> atan_small (recip y')
                           |   sqrt2_minus_one `weaklyLess` y' -> pi_iv / 4 - atan_small ((y' - 1) / (y' + 1))
                           | - sqrt2_minus_one `weaklyLess` y' -> pi_iv / 2 -  atan_small y'
                           | - one_plus_sqrt2  `weaklyLess` y' -> three_pi_iv / 4 - atan_small ((1 + y') / (1 - y'))
                           |   otherwise                       -> pi_iv + atan_small (recip y')
                      -- == pi_iv / 2 - atanI y'
  where
    x' = singleton x
    pi_iv :: i
    pi_iv = makeInterval pi_down pi_up
    three_pi_iv :: i
    three_pi_iv = makeInterval three_pi_down three_pi_up
    one_plus_sqrt2 :: i
    one_plus_sqrt2 = 1 + makeInterval sqrt2_down sqrt2_up
    sqrt2_minus_one :: i
    sqrt2_minus_one = makeInterval sqrt2m1_down sqrt2m1_up

acosI :: forall i. (IsInterval i, Fractional i, Eq (EndPoint i), RealFloat (EndPoint i), RoundedRing (EndPoint i), RoundedSqrt (EndPoint i), RealFloatConstants (EndPoint i)) => i -> i
acosI = withEndPoints (\(Rounded x) (Rounded y) -> hull (acosP x) (acosP y)) -- decreasing

sinhP :: (IsInterval i, Fractional i, Eq (EndPoint i), RealFloat (EndPoint i), RealFloatConstants (EndPoint i)) => EndPoint i -> i
sinhP x | x >= 0 = let y = expP x
                   in (y - recip y) / 2
        | otherwise = let y = expP (- x)
                      in (recip y - y) / 2
        -- TODO: precision when x ~ 0

sinhI :: (IsInterval i, Fractional i, Eq (EndPoint i), RealFloat (EndPoint i), RealFloatConstants (EndPoint i)) => i -> i
sinhI = withEndPoints (\(Rounded x) (Rounded y) -> hull (sinhP x) (sinhP y)) -- increasing

coshP :: (IsInterval i, Fractional i, Eq (EndPoint i), RealFloat (EndPoint i), RealFloatConstants (EndPoint i)) => EndPoint i -> i
coshP x | x >= 0 = let y = expP x
                   in (y + recip y) / 2
        | otherwise = let y = expP (- x)
                      in (recip y + y) / 2

coshI :: (IsInterval i, Fractional i, Eq (EndPoint i), RealFloat (EndPoint i), RealFloatConstants (EndPoint i)) => i -> i
coshI = withEndPoints $ \(Rounded x) (Rounded y) ->
                          let z = hull (coshP x) (coshP y)
                          in if x <= 0 && 0 <= y
                             then hull 0 z
                             else z

tanhP :: (IsInterval i, Fractional i, Eq (EndPoint i), RealFloat (EndPoint i), RealFloatConstants (EndPoint i)) => EndPoint i -> i
tanhP x | -0.5 <= x && x <= 0.5 = sinhP x / coshP x
        | 0 < x = 1 - 2 / (1 + expP (2 * x)) -- assuming 2*x is exact
        | otherwise = 2 / (1 + expP (- 2 * x)) - 1 -- assuming 2*x is exact

tanhI :: (IsInterval i, Fractional i, Eq (EndPoint i), RealFloat (EndPoint i), RealFloatConstants (EndPoint i)) => i -> i
tanhI = withEndPoints $ \(Rounded x) (Rounded y) -> hull (tanhP x) (tanhP y) -- increasing

asinhP :: (IsInterval i, Fractional i, Eq (EndPoint i), RealFloat (EndPoint i), RealFloatConstants (EndPoint i), RoundedSqrt (EndPoint i)) => EndPoint i -> i
asinhP x = let x' = singleton x
           in logI (x' + sqrtI (1 + x' ^ (2 :: Int)))
-- TODO: precision when x ~ 0

asinhI :: (IsInterval i, Fractional i, Eq (EndPoint i), RealFloat (EndPoint i), RealFloatConstants (EndPoint i), RoundedSqrt (EndPoint i)) => i -> i
asinhI = withEndPoints $ \(Rounded x) (Rounded y) -> hull (asinhP x) (asinhP y) -- increasing

acoshP :: (IsInterval i, Fractional i, Eq (EndPoint i), RealFloat (EndPoint i), RealFloatConstants (EndPoint i), RoundedSqrt (EndPoint i)) => EndPoint i -> i
acoshP x | x < 1 = error "acosh: domain"
         | otherwise = let x' = singleton x
                       in logI (x' + sqrtI (x' ^ (2 :: Int) - 1))
-- TODO: precision when x ~ 1

acoshI :: (IsInterval i, Fractional i, Eq (EndPoint i), RealFloat (EndPoint i), RealFloatConstants (EndPoint i), RoundedSqrt (EndPoint i)) => i -> i
acoshI = withEndPoints $ \(Rounded x) (Rounded y) -> hull (acoshP x) (acoshP y) -- increasing

atanhP :: (IsInterval i, Fractional i, Eq (EndPoint i), RealFloat (EndPoint i), RealFloatConstants (EndPoint i)) => EndPoint i -> i
atanhP x | x < -1 || 1 < x = error "atanh: domain"
         | x == -1 = - makeInterval (Rounded maxFinite) (Rounded positiveInfinity)
         | x == 1 = makeInterval (Rounded maxFinite) (Rounded positiveInfinity)
         | otherwise = let x' = singleton x
                       in logI ((1 + x') / (1 - x')) / 2

atanhI :: (IsInterval i, Fractional i, Eq (EndPoint i), RealFloat (EndPoint i), RealFloatConstants (EndPoint i)) => i -> i
atanhI = withEndPoints $ \(Rounded x) (Rounded y) -> hull (atanhP x) (atanhP y) -- increasing
