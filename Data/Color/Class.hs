{-# LANGUAGE Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Color.Class
-- Copyright   :  (C) 2013 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
-- Classes for colors
----------------------------------------------------------------------------
module Data.Color.Class where

import Data.Profunctor
import Data.Word
import Control.Lens

class HasRGB f where
    fromRGB :: RealFrac a => a -> a -> a -> f a
    _Red :: RealFrac a => Lens' (f a) a
    _Green :: RealFrac a => Lens' (f a) a
    _Blue :: RealFrac a => Lens' (f a) a

class HasHSB f where
    fromHSB :: RealFrac a => a -> a -> a -> f a
    _Hue :: RealFrac a => Lens' (f a) a
    _Saturation :: RealFrac a => Lens' (f a) a
    _Brightness :: RealFrac a => Lens' (f a) a

class HasAlpha f where
    _Alpha :: Lens' (f a) a

_8Bit :: RealFrac a => Iso' a Word8
_8Bit = dimap (floor.(*255)) (fmap ((/255) . fromIntegral))
