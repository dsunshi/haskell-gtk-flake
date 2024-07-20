
module Draw.Color (Color (..), color) where

import Data.Bits
import GI.Cairo.Render hiding (x, y, width, height)

data Color = RGB Double Double Double | Hex Int

color :: Color -> Render ()
color (RGB r g b) = setSourceRGB r g b
color (Hex h)     = setSourceRGB r g b
    where
        r = fromIntegral (shiftR h 16 .&. 0xFF) / 255.0
        g = fromIntegral (shiftR h 8 .&. 0xFF) / 255.0
        b = fromIntegral (h .&. 0xFF) / 255.0
