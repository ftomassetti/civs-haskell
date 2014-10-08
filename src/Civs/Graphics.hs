{-# LANGUAGE DeriveDataTypeable #-}

module Civs.Graphics where

import Civs.Model
import Codec.Picture
import Data.Word

shadowFrom world e pos dx dy = let shadowOrigin = move world pos dx dy                                   
                               in case (shadowOrigin) of
                                  Just pos' -> let e' = getElevation world pos'
                                               in if (e' > e) then 0.5 else 0.0
                                  Nothing -> 0.0

shadow :: World -> Position -> Double
shadow world pos = let e = getElevation world pos                       
                   in let shadowTl   = shadowFrom world e pos  (-1) (-1)
                          shadowTl'  = shadowFrom world e pos  (-2) (-2)
                          shadowTl'' = shadowFrom world e pos  (-3) (-3)
                          shadowL    = shadowFrom world e pos  (-1)   0
                          shadowL'   = shadowFrom world e pos  (-2) (-1)
                          shadowL''  = shadowFrom world e pos  (-3) (-2)
                          shadowT    = shadowFrom world e pos    0  (-1)
                          shadowT'   = shadowFrom world e pos  (-1) (-2)
                          shadowT''  = shadowFrom world e pos  (-2) (-3)
                      in shadowTl + shadowTl' + shadowTl'' + (shadowL/2.0) + (shadowL'/3.0) + (shadowL''/4.0) + (shadowT/2.0) + (shadowT'/3.0) + (shadowT''/4.0)


altitudeColor :: Double -> PixelRGB8
altitudeColor elev = let f = elev / 20.0
                         comp = round( f*255 )
                     in PixelRGB8 comp comp comp

mix :: Word8 -> Word8 -> Double -> Double -> Word8
mix c1 c2 f1 f2 = let comp1 =  (fromIntegral c1) * f1
                      comp2 =  (fromIntegral c2) * f2
                  in round( comp1+comp2 )

mixColors :: PixelRGB8 -> PixelRGB8 ->  Double -> PixelRGB8
mixColors c1 c2 f = let PixelRGB8 r1 g1 b1 = c1
                        PixelRGB8 r2 g2 b2 = c2
                        fi = 1.0 - f
                        r = mix r1 r2 f fi
                        g = mix g1 g2 f fi                        
                        b = mix b1 b2 f fi
                    in PixelRGB8 r g b

biomeToColor :: Biome -> PixelRGB8
biomeToColor b = case b of
                  Ocean        -> PixelRGB8   0   0 255 
                  Tundra       -> PixelRGB8 142 145 102
                  Alpine       -> PixelRGB8  94  82  48
                  Glacier      -> PixelRGB8 186 227 222
                  Jungle       -> PixelRGB8  98 214  69
                  Forest       -> PixelRGB8  60  99  51
                  Iceland      -> PixelRGB8 186 227 222
                  Grassland    -> PixelRGB8  82 158  63
                  SandDesert   -> PixelRGB8 232 224 107
                  RockDesert   -> PixelRGB8 153 149 138
                  Savanna      -> PixelRGB8 199 152  44
                  _ -> error $ "Unknown: " ++ (show b)

generateMap :: World -> Image PixelRGB8
generateMap world = generateImage f w h
                    where w = getWidth world
                          h = getHeight world
                          f x y = mixColors (PixelRGB8 30 30 30) (mixColors base altColor 0.4) sh
                                  where b = getBiome world (Pos x y)
                                        base = biomeToColor b
                                        altColor = altitudeColor (getElevation world (Pos x y))
                                        sh = (shadow world (Pos x y)) / 30.0