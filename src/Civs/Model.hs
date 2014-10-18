{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Civs.Model where

import Civs.Pickle
import Data.Maybe
import qualified Data.Map.Strict as M
import System.Random
import Namegen
import Data.Map.Strict as M
import qualified Data.ByteString.Lazy as S

-------------------------------------------------
-- General
-------------------------------------------------

type Id = Int

class WithId el where
  getId :: el -> Id

data Name = Name String | Unnamed
            deriving (Show, Eq)

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

loadAllSamples :: IO LanguageSamples
loadAllSamples = helper ["languages/citynames_ch.txt", "languages/citynames_eg.txt",
                                        "languages/citynames_fr.txt", "languages/citynames_jp.txt",
                                        "languages/citynames_de.txt", "languages/citynames_es.txt",
                                        "languages/citynames_it.txt", "languages/citynames_pl.txt"] []
                 where helper :: [String] -> [[String]] -> IO [[String]]
                       helper [] loaded = return loaded
                       helper fileNames loaded = do newlyLoaded :: [String] <- loadSamples (head fileNames)
                                                    helper (tail fileNames) (newlyLoaded:loaded)

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

data Group = Group {
    groupName :: Name,
    groupPos :: Position,
    groupLanguage :: Language,
    groupId :: Id
}

instance Show Group where
   show g = "Group {id="++(show $ groupId g) ++", name="++(show $ groupName g)++"}"

-------------------------------------------------
-- Settlement
-------------------------------------------------

data Settlement = Settlement {
    settlOwner :: Id,
    settlName :: Name,
    settlPos :: Position,
    settlId :: Int
} deriving Eq
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

numberOfGroups = M.size . gameGroups

numberOfSettlements = M.size . gameSettlements

getSettlement :: Game -> Id -> Settlement
getSettlement game id = fromJust $ M.lookup id (gameSettlements game)

addSettlement :: Game -> Int -> Int -> Position -> (Game, Int)
addSettlement game seed owner pos = let (settlId, game') = nextId game
                                        group = getGroup game' owner
                                        language = groupLanguage group
                                        name = generateName language seed
                                        settlement = Settlement owner (Name name) pos settlId 
                                        game'' = game' { gameSettlements = M.insert settlId settlement (gameSettlements game') }
                                     in (game'',settlId)

insertGroup :: Game -> (Id -> Group) -> (Game, Group)
insertGroup game grNoId = let grId = (gameNextId game)
                              gr = grNoId grId
                              game' = game { gameNextId = grId + 1}
                              gg = gameGroups game'
                              gg' = M.insert grId gr gg
                              game'' = game { gameGroups = gg' }
                          in (game'', gr)

insertSettlement :: Game -> (Id -> Settlement) -> (Game, Settlement)
insertSettlement game settlNoId = let settlId = (gameNextId game)
                                      settl = settlNoId settlId
                                      game' = game { gameNextId = settlId + 1}
                                      gs = gameSettlements game'
                                      gs' = M.insert settlId settl gs
                                      game'' = game { gameSettlements = gs' }
                                  in (game'', settl)

generateGroup :: Game -> Int -> (Game, Group)
generateGroup g seed = insertGroup g ng
                       where pos = randomLandPos (gameWorld g) seed
                             allSamples = gameLanguageSamples g
                             samples = extractRandomSample allSamples seed
                             language = generateLanguage samples seed
                             name = generateName language seed
                             ng = Group (Name name) pos language

containVillage :: Game -> Position -> Bool
containVillage game pos = helper $ M.elems (gameSettlements game)
                          where helper [] = False
                                helper (s:ss) = if (settlPos s)==pos then True else helper ss

generateGame :: World -> LanguageSamples -> Game
generateGame world languageSamples = Game 1 world M.empty languageSamples M.empty

initialGame worldFileName languageSamples = do
    byteString <- S.readFile worldFileName :: IO S.ByteString
    world' <- process (PickleStatus [] empty) byteString
    let world = World world'
    return $ generateGame world languageSamples
