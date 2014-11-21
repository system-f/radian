{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Radian(
  Radian
, radians
, degrees
) where

import Control.Lens(Iso, iso, from)
import Data.Eq(Eq)
import Data.Ord(Ord)
import Prelude(Show, Num((*)), Fractional((/)), Floating, Real, RealFrac, RealFloat, pi)

-- $setup
-- >>> import Control.Lens((#))
-- >>> import Prelude(Double)

newtype Radian a =
  Radian a
  deriving (Eq, Ord, Show, Num, Fractional, Floating, Real, RealFrac, RealFloat)

-- | An isomorphism from radians to degrees.
--
-- >>> radians # (180 :: Double)
-- Radian 3.141592653589793
--
-- >> radians # (90 :: Double)
-- Radian 1.5707963267948966
--
-- >>> radians # (359 :: Double)
-- Radian 6.265732014659643
--
-- >>> radians # (360 :: Double)
-- Radian 6.283185307179586
--
-- >>> radians # (3600 :: Double)
-- Radian 62.83185307179586
--
-- >>> radians # (1 :: Double)
-- Radian 1.7453292519943295e-2
--
-- >>> radians # ((-180) :: Double)
-- Radian (-3.141592653589793)
radians ::
  (Floating a, Floating b) =>
  Iso (Radian a) (Radian b) a b
radians =
  iso
    (\(Radian a) -> a / pi * 180)
    (\a -> Radian (a / 180 * pi))

-- | An isomorphism from degrees to radians.
--
-- >>> degrees # (3.14 :: Radian Double)
-- 179.90874767107852
--
-- >>> degrees # (1 :: Radian Double)
-- 57.29577951308232
--
-- >>> degrees # (10 :: Radian Double)
-- 572.9577951308232
--
-- >>> degrees # (0 :: Radian Double)
-- 0.0
--
-- >>> degrees # (-1 :: Radian Double)
-- -57.29577951308232
--
-- >>> degrees # (-3 :: Radian Double)
-- -171.88733853924697
degrees ::
  (Floating a, Floating b) =>
  Iso a b (Radian a) (Radian b)
degrees =
  from radians
