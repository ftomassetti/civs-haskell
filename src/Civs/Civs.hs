{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import System.Console.ANSI
import System.IO
import Civs.ConsoleExplorer
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

worldFileName = "worlds/seed_77.world"
worldBytes = S.readFile worldFileName

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

generateGroup :: Game -> Int -> Game
generateGroup g seed = g' { gameGroups = ng : (gameGroups g)}
                       where pos = randomLandPos (gameWorld g) seed
                             ng = Group (gameNextId g) (Name "Lupozzi") pos
                             g' = g { gameNextId = 1 + gameNextId g}

generateGame :: World -> Int -> Int -> Game
generateGame world seed ngroups = helper g0 ss0 ngroups
                                  where g0 = Game 1 world []
                                        rg = mkStdGen seed
                                        ss0 = randoms rg :: [Int]
                                        helper g ss 0 = g
                                        helper g (s:ss) n = helper (generateGroup g s) ss (n-1)

simulation :: (TVar Game) -> (MVar ()) -> IO ()
simulation syncGame syncScreen = do
    rg <- newStdGen
    let ss = randoms rg :: [Int]
    simLoop syncGame syncScreen ss

type RandomIntSeq = [Int]

data Event = NoEvent
             | NewGroup
             | NewSettlement Int
             deriving (Eq, Show)

simEvent :: RandomIntSeq -> (TVar Game) -> IO (Event, RandomIntSeq)
simEvent randomSeq syncGame = do
    game <- atomRead syncGame
    return $ case randomValue' of
        0 -> (NoEvent, tail randomSeq)
        1 -> (NewGroup, tail randomSeq)
        2 -> let groups = gameGroups game
             in (NewSettlement $ groupId (head groups), tail randomSeq)
    where randomValue = head randomSeq
          randomValue' = randomValue `mod` 3

simLoop :: (TVar Game) -> (MVar ()) -> RandomIntSeq -> IO ()
simLoop syncGame syncScreen randomInts = do
    takeMVar syncScreen
    (event,randomInts') <- simEvent randomInts syncGame
    drawNews $ " "++ (show event)
    putMVar syncScreen ()
    threadDelay 1000000
    simLoop syncGame syncScreen randomInts'

initialGame worldFileName = do
    byteString <- S.readFile worldFileName :: IO S.ByteString
    world' <- process (PickleStatus [] empty) byteString
    let world = World world'
    return $ generateGame world 1 3

main :: IO ()
main = do syncScreen <- newMVar ()
          g <- initialGame worldFileName
          syncGame :: (TVar Game)  <- atomically $ newTVar $ g
          let e = initialExplorer syncScreen
          initScreen
          forkIO $ simulation syncGame syncScreen
          gameLoop syncGame e