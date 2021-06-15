{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

{-|
Module      : Data.Radian
Copyright   : (C) CSIRO 2018
Copyright   : (C) Tony Morris 2020-2021
License     : BSD3
Maintainer  : Tony Morris <ʇǝu˙sıɹɹoɯʇ@ןןǝʞsɐɥ>
Stability   : experimental
Portability : portable

Isomorphisms between degrees and radians.
-}
module Data.Radian(
  toRadians
, fromRadians
) where

import Data.Functor(Functor(fmap))
import Data.Profunctor(Profunctor(dimap))
import Prelude(Num((*)), Fractional((/)), Floating, pi)

-- | An isomorphism between radians and degrees.
toRadians ::
  (Floating a, Floating b) =>
  Iso a b a b
toRadians =
  dimap
    to
    (fmap fr)

-- | An isomorphism between degrees and radians.
fromRadians ::
  (Floating a, Floating b) =>
  Iso a b a b
fromRadians =
  dimap
    fr
    (fmap to)

----

to ::
  Floating a =>
  a
  -> a
to a =
  a / pi * 180

fr ::
  Floating a =>
  a
  -> a
fr a =
  a / 180 * pi

type Iso s t a b =
  forall p f.
  (Profunctor p, Functor f) =>
  p a (f b) -> p s (f t)
