{-# LANGUAGE DeriveDataTypeable #-}

module Civs.Model where

import Civs.Pickle
import Data.List
import Data.Word
import Data.Map
import Data.Char

-------------------------------------------------
-- Basic
-------------------------------------------------

hasRepetitions :: (Eq a) => [a] -> Bool
hasRepetitions xs = nub xs /= xs

assert false msg _ = error ("Assertion failed: " ++ msg)
assert true  msg x = x

-------------------------------------------------
-- Model
-------------------------------------------------

type Id = Integer
data World = World PickleElement

instance Show World where
  show w = "World " ++ getName w

class WithId el where
  getInGame :: Game -> Id -> el

data Name = Name String | Unnamed
            deriving Show

data Position = Pos { posx :: Int, posy :: Int } 
                deriving Show

data Group = Group { id :: Id, name :: Name }
             deriving Show

data Game = Game { gameWorld :: World, gameGroups :: [Group] }
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

getBiome :: World -> Position -> Biome
getBiome w pos = let biomeMatrix = getWorldEntry w "biome"
                     biomeMatrix' = Civs.Pickle.toList biomeMatrix
                     x = posx pos
                     y = posy pos
                     row  = Civs.Pickle.toList $ biomeMatrix' !! y
                     cell = row !! x
                     cell' = toString cell
                     cell'' = toBiome cell'
                 in cell''

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
