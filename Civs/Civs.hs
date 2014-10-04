{-# LANGUAGE DeriveDataTypeable #-}

import Civs.Pickle

import Data.List
import Data.Word
import Data.Map
import Data.Char
import qualified Data.ByteString.Lazy as S
import Data.Typeable

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

data Position = Pos { x :: Int, y :: Int } 
                deriving Show

data Group = Group { id :: Id, name :: Name }
             deriving Show

data Game = Game { groups :: [Group] }
            deriving Show

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
          putStrLn "Done"