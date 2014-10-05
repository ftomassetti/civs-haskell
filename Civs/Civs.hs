{-# LANGUAGE DeriveDataTypeable #-}

import Civs.Model
import Civs.Pickle
import Civs.Graphics

import Data.List
import Data.Word
import Data.Map
import Data.Char
import qualified Data.ByteString.Lazy as S
import Data.Typeable
import Codec.Picture

worldFileName = "worlds/seed_77.world"
worldBytes = S.readFile worldFileName


main :: IO ()
main = do putStrLn "Start"
          byteString <- S.readFile worldFileName :: IO S.ByteString
          world <- process (PickleStatus [] empty) byteString
          putStrLn $ " name = "++(show $ getName world)
          putStrLn $ " width = "++(show $ getWidth world)
          putStrLn $ " height = "++(show $ getHeight world)
          let img = generateMap world
          savePngImage "map.png" (ImageRGB8 img)
          putStrLn "Done"