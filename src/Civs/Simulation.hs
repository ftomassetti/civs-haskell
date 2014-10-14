module Civs.Simulation where

import Civs.Model

import System.Random
import Civs.ConsoleExplorer
import Control.Concurrent
import Control.Concurrent.STM

startSimulation syncGame syncScreen = forkIO $ simulation syncGame syncScreen

simulation :: (TVar Game) -> (MVar ()) -> IO ()
simulation syncGame syncScreen = do
    rg <- newStdGen
    let ss = randoms rg :: [Int]
    simLoop syncGame syncScreen ss

type RandomIntSeq = [Int]

data Event = NoEvent
             | NewGroup
             | NewSettlement Int
             deriving Eq

instance Show Event where
    show NoEvent = "<nothing>"
    show NewGroup = "Creating new group"
    show (NewSettlement id) = "Creating new settlement for group " ++ (show id)

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

executeEvent syncGame NoEvent = return ()

executeEvent syncGame NewGroup = return ()

executeEvent syncGame (NewSettlement groupId) = return ()

simLoop :: (TVar Game) -> (MVar ()) -> RandomIntSeq -> IO ()
simLoop syncGame syncScreen randomInts = do
    takeMVar syncScreen
    (event,randomInts') <- simEvent randomInts syncGame
    executeEvent syncGame event
    drawNews $ " "++ (show event)
    putMVar syncScreen ()
    threadDelay 1000000
    simLoop syncGame syncScreen randomInts'

