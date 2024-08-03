
module Draw.Color (Color (..), color) where

import Data.Bits
import GI.Cairo.Render hiding (x, y, width, height)
import System.Random

data Color = RGB Double Double Double | Hex Int | Gray Double

color :: Color -> Render ()
color (RGB r g b) = setSourceRGB r g b
color (Gray bw)   = setSourceRGB bw bw bw
color (Hex h)     = setSourceRGB r g b
    where
        r = fromIntegral (shiftR h 16 .&. 0xFF) / 255.0
        g = fromIntegral (shiftR h 8 .&. 0xFF) / 255.0
        b = fromIntegral (h .&. 0xFF) / 255.0

instance Random Color where
    random g = (RGB red green blue, bg)
        where
            (red, rg)   = randomR (0.0, 1.0) g
            (green, gg) = randomR (0.0, 1.0) rg
            (blue, bg)  = randomR (0.0, 1.0) gg
    randomR (_a, _b) = random
