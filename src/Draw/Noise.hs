module Draw.Noise (noise) where

import Numeric.Noise.Perlin

noise :: Double -> Double -> Double -> Double
noise x y z = noiseValue perlinNoise (x, y, z)
    where
       seed        = 1
       octaves     = 5
       scale       = 0.05
       persistance = 0.5
       perlinNoise = perlin seed octaves scale persistance
