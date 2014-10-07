{-# LANGUAGE DeriveDataTypeable #-}

import Civs.Model
import Civs.Pickle
import Civs.Graphics

import qualified Data.List as L
import Data.Word
import Data.Map
import Data.Char
import qualified Data.ByteString.Lazy as S
import Data.Typeable
import Codec.Picture
import System.Random

worldFileName = "worlds/seed_77.world"
worldBytes = S.readFile worldFileName

applyNTimes :: Int -> (a -> a) -> a -> a
applyNTimes n f val = L.foldl (\s e -> e s) val [f | x <- [1..n]]

generateGroup :: Game -> Int -> Game
generateGroup g seed = g' { gameGroups = ng : (gameGroups g)}
                       where ng = Group (gameNextId g) (Name "Lupozzi")
                             g' = g { gameNextId = 1 + gameNextId g}

generateGame :: World -> Int -> Int -> Game
generateGame world seed ngroups = helper g0 ss0 ngroups
                                  where g0 = Game 1 world []
                                        rg = mkStdGen seed
                                        ss0 = randoms rg :: [Int]
                                        helper g ss 0 = g
                                        helper g (s:ss) n = helper (generateGroup g s) ss (n-1)


main :: IO ()
main = do putStrLn "Start"
          byteString <- S.readFile worldFileName :: IO S.ByteString
          world' <- process (PickleStatus [] empty) byteString
          let world = World world'
          let g = generateGame world 1 3
          putStrLn $ "Game: " ++ (show g)
          putStrLn "Done"