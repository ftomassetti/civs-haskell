{-# LANGUAGE DeriveDataTypeable #-}

import Civs.Pickle

import Data.List
import Data.Word
import Data.Map
import Data.Char
import qualified Data.ByteString.Lazy as S
import Data.Typeable
import Codec.Picture

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

class WithId el where
  getInGame :: Game -> Id -> el

data Name = Name String | Unnamed
            deriving Show

data Position = Pos { posx :: Int, posy :: Int } 
                deriving Show

data Group = Group { id :: Id, name :: Name }
             deriving Show

data Game = Game { groups :: [Group] }
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

worldFileName = "worlds/seed_77.world"
worldBytes = S.readFile worldFileName


printWorld :: PickleElement -> IO ()
printWorld (PickleSetState _ (PickleDict m)) = do putStrLn $ "Getting " ++ (show $ keys m)
                                                      

getWorld :: PickleElement -> Map PickleElement PickleElement
getWorld (PickleSetState _ (PickleDict m)) = m

getWorldEntry w k = let d = getWorld w
                    in getMaybe (Data.Map.lookup (PickleString k) d)

getName w = toString $ getWorldEntry w "name"

getWidth :: PickleElement -> Int
getWidth w = toInt $ getWorldEntry w "width"

getHeight :: PickleElement -> Int
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

getBiome :: PickleElement -> Position -> Biome
getBiome w pos = let biomeMatrix = getWorldEntry w "biome"
                     biomeMatrix' = Civs.Pickle.toList biomeMatrix
                     x = posx pos
                     y = posy pos
                     row  = Civs.Pickle.toList $ biomeMatrix' !! y
                     cell = row !! x
                     cell' = toString cell
                     cell'' = toBiome cell'
                 in cell''

printCell w pos = putStrLn $ " biome " ++ (show (posx pos)) ++ " " ++ (show (posy pos)) ++ " = "++(show $ getBiome w pos)
printCells w (pos:rest) = do printCell w pos
                             printCells w rest
printCells w [] = putStrLn "Done"

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

generateMap :: PickleElement -> Image PixelRGB8
generateMap world = generateImage f w h
                    where w = getWidth world
                          h = getHeight world
                          f x y = biomeToColor b
                                  where b = getBiome world (Pos x y)                          

main :: IO ()
main = do putStrLn "Start"
          byteString <- S.readFile worldFileName :: IO S.ByteString
          --let res = unpickle byteString
          --let bytes = S.unpack byteString :: [Word8]      
          world <- process (PickleStatus [] empty) byteString
          --case res of
          --     Left err -> putStrLn $ "Can't unpickle .\nUnpickling error:\n " ++ err
          --     Right v -> putStrLn "Well done!"
          printWorld world
          putStrLn $ "Biome " ++ (show $ printPickle $ getWorldEntry world "biome")
          putStrLn $ " name = "++(show $ getName world)
          putStrLn $ " width = "++(show $ getWidth world)
          putStrLn $ " height = "++(show $ getHeight world)
          let img = generateMap world
          savePngImage "map.png" (ImageRGB8 img)
          putStrLn "Done"