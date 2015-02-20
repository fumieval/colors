{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 707
{-# LANGUAGE PatternSynonyms #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.RGBA
-- Copyright   :  (C) 2013 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
-- RGBAs and its operations
----------------------------------------------------------------------------
module Data.Color (
    -- * The type
    RGBA(..)
    , Color
    -- * RGBA operations
    , blend
    , multRGBA
    , module Data.Color.Class
    ) where

import Data.String
import Data.Char
import Data.Color.Class
import Foreign.Storable
import Foreign.Ptr
import Control.Applicative
import Linear

type Color = RGBA

type RGBA = V4

#if __GLASGOW_HASKELL__ >= 707
pattern RGBA a b c d = V4 a b c d
#endif

instance HasRGB V4 where
    fromRGB r g b = V4 r g b 1.0
    _Red = _x
    _Green = _y
    _Blue = _z

instance HasHSB V4 where
    fromHSB h s v = hsv_rgb h s v (argb 1.0)
    _Hue f (V4 r g b a) = rgb_hsv r g b $ \h s v -> fmap (\h' -> hsv_rgb h' s v (argb a)) (f h)
    _Saturation f (V4 r g b a) = rgb_hsv r g b $ \h s v -> fmap (\s' -> hsv_rgb h s' v (argb a)) (f s)
    _Brightness f (V4 r g b a) = rgb_hsv r g b $ \h s v -> fmap (\v' -> hsv_rgb h s v' (argb a)) (f v)

instance HasAlpha V4 where
    _Alpha = _w

instance Fractional a => IsString (V4 a) where
    fromString xs@[r,g,b,a] | all isHexDigit xs = V4 (hf r) (hf g) (hf b) (hf a)
    fromString xs@[r,g,b] | all isHexDigit xs = V4 (hf r) (hf g) (hf b) 1
    fromString xs@[r1,r0,g1,g0,b1,b0,a1,a0] | all isHexDigit xs = V4 (hf' r1 r0) (hf' g1 g0) (hf' b1 b0) (hf' a1 a0)
    fromString xs@[r1,r0,g1,g0,b1,b0] | all isHexDigit xs = V4 (hf' r1 r0) (hf' g1 g0) (hf' b1 b0) 1
    fromString x = error $ "Invalid color representation: " ++ x

-- | Blend two colors.
blend :: (Functor f, Additive f, Num a) => a -> f a -> f a -> f a
blend t a b = (1 - t) *^ a ^+^ t *^ b

argb :: a -> a -> a -> a -> V4 a
argb a r g b = RGBA r g b a

rgb_hsv :: RealFrac a => a -> a -> a -> (a -> a -> a -> r) -> r
rgb_hsv r g b f = f h (s / maxC) maxC where
    maxC = r `max` g `max` b
    minC = r `min` g `min` b
    s = maxC - minC
    h | maxC == r = (g - b) / s * 60
      | maxC == g = (b - r) / s * 60 + 120
      | maxC == b = (r - g) / s * 60 + 240
      | otherwise = undefined

hsv_rgb :: RealFrac a => a -> a -> a -> (a -> a -> a -> r) -> r
hsv_rgb h s v r
    | h' == 0 = r v t p
    | h' == 1 = r q v p
    | h' == 2 = r p v t
    | h' == 3 = r p q v
    | h' == 4 = r t p v
    | h' == 5 = r v p q
    | otherwise = undefined
    where
        h' = floor (h / 60) `mod` 6 :: Int
        f = h / 60 - fromIntegral h'
        p = v * (1 - s)
        q = v * (1 - f * s)
        t = v * (1 - (1 - f) * s)

hf x = fromIntegral (digitToInt x) / 15

hf' x y = fromIntegral (digitToInt x * 16 + digitToInt y) / 255

{-# DEPRECATED multRGBA "Use (*) instead" #-}
multRGBA = (*)
