{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Radian(
  toRadians
, fromRadians
) where

import Control.Lens(Iso, iso, from)
import Prelude(Num((*)), Fractional((/)), Floating, pi)

-- $setup
-- >>> import Control.Lens((#))
-- >>> import Prelude(Double)

-- | An isomorphism between radians and degrees.
--
-- >>> toRadians # (180 :: Double)
-- 3.141592653589793
--
-- >> toRadians # (90 :: Double)
-- 1.5707963267948966
--
-- >>> toRadians # (359 :: Double)
-- 6.265732014659643
--
-- >>> toRadians # (360 :: Double)
-- 6.283185307179586
--
-- >>> toRadians # (3600 :: Double)
-- 62.83185307179586
--
-- >>> toRadians # (1 :: Double)
-- 1.7453292519943295e-2
--
-- >>> toRadians # ((-180) :: Double)
-- -3.141592653589793
toRadians ::
  (Floating a, Floating b) =>
  Iso a b a b
toRadians =
  iso
    (\a -> a / pi * 180)
    (\a -> a / 180 * pi)   

-- | An isomorphism between degrees and radians.
--
-- >>> fromRadians # (0 :: Double)
-- 0.0
--
-- >>> fromRadians # (1 :: Double)
-- 57.29577951308232
--
-- >>> fromRadians # ((-1) :: Double)
-- -57.29577951308232
--
-- >>> fromRadians # (3 :: Double)
-- 171.88733853924697
fromRadians ::
  (Floating a, Floating b) =>
  Iso a b a b
fromRadians =
  from toRadians
