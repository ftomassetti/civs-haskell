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

generateGame :: World -> Int -> Game
generateGame world seed = g0
                          where g0 = Game world []

main :: IO ()
main = do putStrLn "Start"
          byteString <- S.readFile worldFileName :: IO S.ByteString
          world' <- process (PickleStatus [] empty) byteString
          let world = World world'
          let g = generateGame world 1
          putStrLn $ "Game: " ++ (show g)
          putStrLn "Done"