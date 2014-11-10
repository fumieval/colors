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

type Color = RGBA

-- | A color that has red, green, blue, alpha as its components.
-- It is an instance of 'HasRGB' so there are some lenses to tweak individual components.
data RGBA = RGBA Float Float Float Float deriving (Show, Read, Eq, Ord)

instance Storable RGBA where
    sizeOf _ = sizeOf (0 :: Float) * 4
    alignment _ = 0
    peek ptr = RGBA
        <$> peekElemOff (castPtr ptr) 0 
        <*> peekElemOff (castPtr ptr) 1
        <*> peekElemOff (castPtr ptr) 2
        <*> peekElemOff (castPtr ptr) 3
    poke ptr (RGBA r g b a) = do
        pokeElemOff (castPtr ptr) 0 r
        pokeElemOff (castPtr ptr) 1 g
        pokeElemOff (castPtr ptr) 2 b
        pokeElemOff (castPtr ptr) 3 a

instance HasRGB RGBA where
    fromRGB r g b = RGBA r g b 1.0
    _Red f (RGBA r g b a) = fmap (\r' -> RGBA r' g b a) (f r)
    _Green f (RGBA r g b a) = fmap (\g' -> RGBA r g' b a) (f g)
    _Blue f (RGBA r g b a) = fmap (\b' -> RGBA r g b' a) (f b)

instance HasHSB RGBA where
    fromHSB h s v = hsv_rgb h s v (argb 1.0)
    _Hue f (RGBA r g b a) = rgb_hsv r g b $ \h s v -> fmap (\h' -> hsv_rgb h' s v (argb a)) (f h)
    _Saturation f (RGBA r g b a) = rgb_hsv r g b $ \h s v -> fmap (\s' -> hsv_rgb h s' v (argb a)) (f s)
    _Brightness f (RGBA r g b a) = rgb_hsv r g b $ \h s v -> fmap (\v' -> hsv_rgb h s v' (argb a)) (f v)

instance HasAlpha RGBA where
    _Alpha f (RGBA r g b a) = fmap (\a' -> RGBA r g b a') (f a)

instance IsString RGBA where
    fromString xs@[r,g,b,a] | all isHexDigit xs = RGBA (hf r) (hf g) (hf b) (hf a)
    fromString xs@[r,g,b] | all isHexDigit xs = RGBA (hf r) (hf g) (hf b) 1
    fromString xs@[r1,r0,g1,g0,b1,b0,a1,a0] | all isHexDigit xs = RGBA (hf' r1 r0) (hf' g1 g0) (hf' b1 b0) (hf' a1 a0)
    fromString xs@[r1,r0,g1,g0,b1,b0] | all isHexDigit xs = RGBA (hf' r1 r0) (hf' g1 g0) (hf' b1 b0) 1
    fromString x = error $ "Invalid color representation: " ++ x

-- | Blend two colors.
blend :: Num a => Float -> RGBA -> RGBA -> RGBA
blend t (RGBA r0 g0 b0 a0) (RGBA r1 g1 b1 a1) = RGBA
    (r0 * (1 - t) + r1 * t)
    (g0 * (1 - t) + g1 * t)
    (b0 * (1 - t) + b1 * t)
    (a0 * (1 - t) + a1 * t)

argb :: Float -> Float -> Float -> Float -> RGBA
argb a r g b = RGBA r g b a

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

multRGBA :: RGBA -> RGBA -> RGBA
multRGBA (RGBA r0 g0 b0 a0) (RGBA r1 g1 b1 a1) = RGBA (r0 * r1) (g0 * g1) (b0 * b1) (a0 * a1)
