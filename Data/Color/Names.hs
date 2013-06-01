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

white :: HasRGB a => a
white = fromRGB 1.0 1.0 1.0

black :: HasRGB a => a
black = fromRGB 0.0 0.0 0.0

red :: HasRGB a => a
red = fromRGB 1.0 0.0 0.0

green :: HasRGB a => a
green = fromRGB 0.0 1.0 0.0

blue :: HasRGB a => a
blue = fromRGB 0.0 0.0 1.0

yellow :: HasRGB a => a
yellow = fromRGB 1.0 1.0 0.0

cyan :: HasRGB a => a
cyan = fromRGB 0.0 1.0 1.0

magenta :: HasRGB a => a
magenta = fromRGB 1.0 0.0 1.0
