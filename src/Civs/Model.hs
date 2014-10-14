{-# LANGUAGE DeriveDataTypeable #-}

module Civs.Model where

import Civs.Pickle
import Data.List
import Data.Word
import Data.Map
import Data.Char
import System.Random

-------------------------------------------------
-- Model
-------------------------------------------------

type Id = Int
data World = World PickleElement

instance Show World where
  show w = "World " ++ getName w

class WithId el where
  getInGame :: Game -> Id -> el

data Name = Name String | Unnamed
            deriving Show

data Position = Pos { posx :: Int, posy :: Int } 
                deriving (Show, Eq)

data Group = Group { groupId :: Id, groupName :: Name, groupPos :: Position }
             deriving Show

data Game = Game { gameNextId :: Int, gameWorld :: World, gameGroups :: [Group] }
            deriving Show

data Biome = Ocean
             | RockDesert
             | SandDesert
             | Forest
             | Grassland
             | Tundra
             | Alpine
             | Glacier
             | Iceland
             | Jungle
             | Savanna
             deriving (Show, Eq)

getWorld :: World -> Map PickleElement PickleElement
getWorld (World (PickleSetState _ (PickleDict m))) = m

getWorldEntry w k = let d = getWorld w
                    in getMaybe (Data.Map.lookup (PickleString k) d)

getName w = toString $ getWorldEntry w "name"

getWidth :: World -> Int
getWidth w = toInt $ getWorldEntry w "width"

getHeight :: World -> Int
getHeight w = toInt $ getWorldEntry w "height"

toBiome :: String -> Biome
toBiome s = case s of
            "ocean" -> Ocean
            "tundra" -> Tundra
            "alpine" -> Alpine
            "glacier" -> Glacier
            "jungle" -> Jungle
            "forest" -> Forest
            "iceland" -> Iceland
            "grassland" -> Grassland
            "sand desert" -> SandDesert
            "rock desert" -> RockDesert
            "savanna" -> Savanna
            _ -> error $ "Unknown: " ++ s

getBiomeStrict :: World -> Position -> Biome
getBiomeStrict w pos = let biomeMatrix = getWorldEntry w "biome"
                           biomeMatrix' = Civs.Pickle.toList biomeMatrix
                           x = posx pos
                           y = posy pos
                           row  = Civs.Pickle.toList $ biomeMatrix' !! y
                           cell = row !! x
                           cell' = toString cell
                           cell'' = toBiome cell'
                       in cell''


isValidPos :: World -> Position -> Bool
isValidPos w (Pos x y) = x>=0 && y>=0 && x<(getWidth w) && y<(getHeight w)

getBiome world pos = if isValidPos world pos then getBiomeStrict world pos else Ocean

isLand w pos = getBiome w pos /= Ocean

getElevation :: World -> Position -> Double
getElevation w pos = let matrix = getWorldEntry w "elevation"
                         x = posx pos
                         y = posy pos
                         dat = Civs.Pickle.toDict matrix
                         dat' = getMaybe $ Data.Map.lookup "data" dat :: PickleElement
                         dat'' = Civs.Pickle.toList dat'
                         row  = Civs.Pickle.toList $ dat'' !! y
                         cell = row !! x
                         cell' = toDouble cell                          
                     in cell'

move :: World -> Position -> Int -> Int -> Maybe Position
move world pos dx dy = let Pos x y = pos                           
                           w = getWidth world
                           h = getHeight world
                           nx = x+dx
                           ny = y+dy
                       in if (nx>0) && (ny>0) && (nx<w) && (ny<h) then Just (Pos nx ny) else Nothing

nextSeed :: Int -> Int
nextSeed seed = head ss
                where rg = mkStdGen seed
                      ss = randoms rg :: [Int]

randomPos world seed = Pos x y
                       where rg = mkStdGen seed
                             ss0 = randoms rg :: [Int]
                             x = (ss0 !! 0) `mod` (getWidth world)
                             y = (ss0 !! 1) `mod` (getHeight world)

randomLandPos world seed = if isLand world pos then pos else randomPos world (nextSeed seed)
                           where pos = randomPos world seed

generateGroup :: Game -> Int -> (Group, Game)
generateGroup g seed = (ng, g' { gameGroups = ng : (gameGroups g)})
                       where pos = randomLandPos (gameWorld g) seed
                             ng = Group (gameNextId g) (Name "Lupozzi") pos
                             g' = g { gameNextId = 1 + gameNextId g}