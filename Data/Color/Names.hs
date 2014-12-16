-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Color.Names
-- Copyright   :  (C) 2013 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
-- Entities
----------------------------------------------------------------------------
module Data.Color.Names where
import Data.Color.Class

white :: (HasRGB f, RealFrac a) => f a
white = fromRGB 1.0 1.0 1.0

gray :: (HasRGB f, RealFrac a) => f a
gray = fromRGB 0.5 0.5 0.5

black :: (HasRGB f, RealFrac a) => f a
black = fromRGB 0.0 0.0 0.0

red :: (HasRGB f, RealFrac a) => f a
red = fromRGB 1.0 0.0 0.0

green :: (HasRGB f, RealFrac a) => f a
green = fromRGB 0.0 1.0 0.0

blue :: (HasRGB f, RealFrac a) => f a
blue = fromRGB 0.0 0.0 1.0

yellow :: (HasRGB f, RealFrac a) => f a
yellow = fromRGB 1.0 1.0 0.0

cyan :: (HasRGB f, RealFrac a) => f a
cyan = fromRGB 0.0 1.0 1.0

magenta :: (HasRGB f, RealFrac a) => f a
magenta = fromRGB 1.0 0.0 1.0
