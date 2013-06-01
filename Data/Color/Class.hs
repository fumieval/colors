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

class HasRGB a where
    fromRGB :: Float -> Float -> Float -> a
    -- | @'_Red' :: Lens' 'a' 'Float'@
    _Red :: Functor f => (Float -> f Float) -> a -> f a
    -- | @'_Green' :: Lens' 'a' 'Float'@
    _Green :: Functor f => (Float -> f Float) -> a -> f a
    -- | @'_Blue' :: Lens' 'a' 'Float'@
    _Blue :: Functor f => (Float -> f Float) -> a -> f a

class HasHSB a where
    fromHSB :: Float -> Float -> Float -> a
    -- | @'_Hue' :: Lens' 'a' 'Float'@
    _Hue :: Functor f => (Float -> f Float) -> a -> f a
    -- | @'_Saturation' :: Lens' 'a' 'Float'@
    _Saturation :: Functor f => (Float -> f Float) -> a -> f a
    -- | @'_Brightness' :: Lens' 'a' 'Float'@
    _Brightness :: Functor f => (Float -> f Float) -> a -> f a

class HasAlpha a where
    -- | @'_Alpha' :: Lens' 'a' 'Float'@
    _Alpha :: Functor f => (Float -> f Float) -> a -> f a

-- | @'_8Bit' :: Iso' 'Float' 'Word8'@
_8Bit :: forall p f. (Profunctor p, Functor f) => p Word8 (f Word8) -> p Float (f Float)
_8Bit = dimap (floor.(*255)) (fmap ((/255) . fromIntegral))

{-
class HasCMYK a where
    _Cyan :: Functor f => (Float -> f Float) -> a -> f a
    _Magenta :: Functor f => (Float -> f Float) -> a -> f a
    _Yellow :: Functor f => (Float -> f Float) -> a -> f a
    _KeyPlate :: Functor f => (Float -> f Float) -> a -> f a
-}
