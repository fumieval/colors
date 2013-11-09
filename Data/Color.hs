-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Color
-- Copyright   :  (C) 2013 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
-- Colors and its operations
----------------------------------------------------------------------------
module Data.Color (
    -- * The type
    Color(..)
    -- * Color operations
    , blend
    , module Data.Color.Class
    ) where

import Data.String
import Data.Char
import Data.Color.Class

-- | A color that has red, green, blue, alpha as its components.
-- It is an instance of 'HasRGB' so there are some lenses to tweak individual components.
data Color = Color Float Float Float Float deriving (Show, Read, Eq, Ord)

instance HasRGB Color where
    fromRGB r g b = Color r g b 1.0
    _Red f (Color r g b a) = fmap (\r' -> Color r' g b a) (f r)
    _Green f (Color r g b a) = fmap (\g' -> Color r g' b a) (f g)
    _Blue f (Color r g b a) = fmap (\b' -> Color r g b' a) (f b)

instance HasHSB Color where
    fromHSB h s v = hsv_rgb h s v (argb 1.0)
    _Hue f (Color r g b a) = rgb_hsv r g b $ \h s v -> fmap (\h' -> hsv_rgb h' s v (argb a)) (f h)
    _Saturation f (Color r g b a) = rgb_hsv r g b $ \h s v -> fmap (\s' -> hsv_rgb h s' v (argb a)) (f s)
    _Brightness f (Color r g b a) = rgb_hsv r g b $ \h s v -> fmap (\v' -> hsv_rgb h s v' (argb a)) (f v)

instance HasAlpha Color where
    _Alpha f (Color r g b a) = fmap (\a' -> Color r g b a') (f a)

instance IsString Color where
    fromString xs@[r,g,b,a] | all isHexDigit xs = Color (hf r) (hf g) (hf b) (hf a)
    fromString xs@[r,g,b] | all isHexDigit xs = Color (hf r) (hf g) (hf b) 1
    fromString xs@[r1,r0,g1,g0,b1,b0,a1,a0] | all isHexDigit xs = Color (hf' r1 r0) (hf' g1 g0) (hf' b1 b0) (hf' a1 a0)
    fromString xs@[r1,r0,g1,g0,b1,b0] | all isHexDigit xs = Color (hf' r1 r0) (hf' g1 g0) (hf' b1 b0) 1
    fromString x = error $ "Invalid color representation: " ++ x

-- | Blend two colors.
blend :: Float -> Color -> Color -> Color
blend t (Color r0 g0 b0 a0) (Color r1 g1 b1 a1) = Color
    (r0 * (1 - t) + r1 * t)
    (g0 * (1 - t) + g1 * t)
    (b0 * (1 - t) + b1 * t)
    (a0 * (1 - t) + a1 * t)

argb :: Float -> Float -> Float -> Float -> Color
argb a r g b = Color r g b a

rgb_hsv :: Float -> Float -> Float -> (Float -> Float -> Float -> a) -> a
rgb_hsv r g b f = f h (s / maxC) maxC where
    maxC = r `max` g `max` b
    minC = r `min` g `min` b
    s = maxC - minC
    h | maxC == r = (g - b) / s * 60
      | maxC == g = (b - r) / s * 60 + 120
      | maxC == b = (r - g) / s * 60 + 240
      | otherwise = undefined

hsv_rgb :: Float -> Float -> Float -> (Float -> Float -> Float -> a) -> a
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

hf :: Char -> Float
hf x = fromIntegral (digitToInt x) / 15

hf' :: Char -> Char -> Float
hf' x y = fromIntegral (digitToInt x * 16 + digitToInt y) / 255
