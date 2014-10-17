{-# LANGUAGE DeriveDataTypeable #-}

module Civs.Model where

import Civs.Pickle
import Data.Maybe
import qualified Data.Map.Strict as M
import System.Random
import Namegen

-------------------------------------------------
-- General
-------------------------------------------------

type Id = Int

class WithId el where
  getId :: el -> Id

data Name = Name String | Unnamed
            deriving Show

data Position = Pos { posx :: Int, posy :: Int }
                deriving (Show, Eq)

nextSeed :: Int -> Int
nextSeed seed = head ss
                where rg = mkStdGen seed
                      ss = randoms rg :: [Int]

takeRandom :: Int -> [a] -> Int -> [a]
takeRandom seed [] n = error "List is empty"
takeRandom seed list n = helper ss0 list n []
                         where rg = mkStdGen seed
                               ss0 = randoms rg :: [Int]
                               helper :: [Int] -> [a] -> Int -> [a] -> [a]
                               helper ss list 0 taken = taken
                               helper ss list n taken = let index = (head ss) `mod` (length list)
                                                            extracted = list !! index
                                                        in helper (tail ss) list (n-1) (extracted:taken)

-------------------------------------------------
-- Language
-------------------------------------------------

type LanguageSamples = [[String]]

generateLanguage :: [String] -> Int -> Language
generateLanguage samples seed = fromSamples samples

extractRandomSample :: LanguageSamples -> Int -> [String]
extractRandomSample allSamples seed = samplesFromA ++ samplesFromB
                                      where rg = mkStdGen seed
                                            ss0 = randoms rg :: [Int]
                                            indexA = (ss0 !! 0) `mod` (length allSamples)
                                            indexB = (ss0 !! 1) `mod` (length allSamples)
                                            howManyFromA = (ss0 !! 2) `mod` 250
                                            howManyFromB = 250 - howManyFromA
                                            samplesFromA = takeRandom (ss0 !! 3) (allSamples !! indexA) howManyFromA
                                            samplesFromB = takeRandom (ss0 !! 4) (allSamples !! indexB) howManyFromB

-------------------------------------------------
-- World
-------------------------------------------------

data World = World PickleElement

instance Show World where
  show w = "World " ++ getName w

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

getWorld :: World -> M.Map PickleElement PickleElement
getWorld (World (PickleSetState _ (PickleDict m))) = m

getWorldEntry w k = let d = getWorld w
                    in getMaybe (M.lookup (PickleString k) d)

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
                         dat' = getMaybe $ M.lookup "data" dat :: PickleElement
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

randomPos world seed = Pos x y
                       where rg = mkStdGen seed
                             ss0 = randoms rg :: [Int]
                             x = (ss0 !! 0) `mod` (getWidth world)
                             y = (ss0 !! 1) `mod` (getHeight world)

randomLandPos world seed = if isLand world pos then pos else randomPos world (nextSeed seed)
                           where pos = randomPos world seed


-------------------------------------------------
-- Group
-------------------------------------------------

data Group = Group { groupId :: Id, groupName :: Name, groupPos :: Position, groupLanguage :: Language }

instance Show Group where
   show g = "Group {id="++(show $ groupId g) ++", name="++(show $ groupName g)++"}"

-------------------------------------------------
-- Settlement
-------------------------------------------------

data Settlement = Settlement { settlId :: Int, settlOwner :: Int, settlPos :: Position, settlName :: Name }

-------------------------------------------------
-- Game
-------------------------------------------------

data Game = Game {
    gameNextId :: Int,
    gameWorld :: World,
    gameGroups :: M.Map Id Group,
    gameLanguageSamples :: LanguageSamples,
    gameSettlements :: M.Map Int Settlement
}

instance Show Game where
   show g = "Game {groups="++(show $ gameGroups g) ++"}"

nextId :: Game -> (Int, Game)
nextId game = (gameNextId game, game { gameNextId = 1 + gameNextId game})

getGroup :: Game -> Id -> Group
getGroup game id = fromJust $ M.lookup id (gameGroups game)

addSettlement :: Game -> Int -> Int -> Position -> (Game, Int)
addSettlement game seed owner pos = let (settlId, game') = nextId game
                                        group = getGroup game' owner
                                        language = groupLanguage group
                                        name = generateName language seed
                                        settlement = Settlement settlId owner pos (Name name)
                                        game'' = game' { gameSettlements = M.insert settlId settlement (gameSettlements game') }
                                     in (game'',settlId)

generateGroup :: Game -> Int -> (Group, Game)
generateGroup g seed = (ng, g' { gameGroups = M.insert (groupId ng) ng (gameGroups g)})
                       where pos = randomLandPos (gameWorld g) seed
                             allSamples = gameLanguageSamples g
                             samples = extractRandomSample allSamples seed
                             language = generateLanguage samples seed
                             name = generateName language seed
                             ng = Group (gameNextId g) (Name name) pos language
                             g' = g { gameNextId = 1 + gameNextId g}