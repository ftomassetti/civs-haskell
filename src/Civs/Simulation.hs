module Civs.Simulation where

import Civs.Model
import Civs.Base

import System.Random
import Civs.ConsoleExplorer
import Control.Concurrent
import Control.Concurrent.STM

-- |Start all the separate threads for the simulation
startSimulation syncGame syncScreen = do
    forkIO $ simulation syncGame syncScreen
    forkIO $ groupBalancer syncGame syncScreen

-- |This thread creates new group when there are too few
groupBalancer :: (TVar Game) -> (MVar ()) -> IO ()
groupBalancer syncGame syncScreen = do
        rg <- newStdGen
        let ss = randoms rg :: [Int]
        loop syncGame syncScreen ss
    where loop syncGame syncScreen ri = do  takeMVar syncScreen
                                            game <- atomRead syncGame
                                            let groups = gameGroups game
                                            if (length groups) < 5
                                                then do let (gr, game') = generateGroup game (head ri)
                                                        atomWrite syncGame game'
                                                        let Name sName = groupName gr
                                                        drawNews $ "Balancer: creating group "++ sName
                                                else drawNews $ "Balancer: enough groups ("++(show $ length groups)++")"
                                            putMVar syncScreen ()
                                            threadDelay 4500000
                                            loop syncGame syncScreen (tail ri)

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
        1 -> let groups = gameGroups game
             in if length groups > 0
                then (NewSettlement $ groupId (head groups), tail randomSeq)
                else (NoEvent, tail randomSeq)
    where randomValue = head randomSeq
          randomValue' = randomValue `mod` 2

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

