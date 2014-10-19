{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Civs.Simulation where

import Civs.Model
import Civs.Base

import System.Random
import Civs.ConsoleExplorer
import Control.Concurrent
import Control.Concurrent.STM

import qualified Data.Map.Strict as M

-- |Start all the separate threads for the simulation
startSimulation :: (TVar Game) -> (TVar UI) -> IO ()
startSimulation syncGame sUI = do
    forkIO $ simulation syncGame sUI
    forkIO $ groupBalancer syncGame sUI
    return ()

-- |This thread creates new group when there are too few
groupBalancer :: (TVar Game) -> (TVar UI) -> IO ()
groupBalancer syncGame sUI = do
        rg <- newStdGen
        let ss = randoms rg :: [Int]
        loop syncGame sUI ss
    where loop syncGame sUI ri = do         lockScreen sUI
                                            game <- atomRead syncGame
                                            let groupIds = M.keys $ gameGroups game
                                            if (length groupIds) < 5
                                                then do (game', gr) <- atomUpdateT syncGame (generateGroup (head ri))
                                                        let Name sName = groupName gr
                                                        drawNews $ "Balancer: creating group "++ sName
                                                else return () -- drawNews $ "Balancer: enough groups ("++(show $ length groupIds)++")"
                                            releaseScreen sUI
                                            threadDelay 4500000
                                            loop syncGame sUI (tail ri)

simulation :: (TVar Game) -> (TVar UI) -> IO ()
simulation syncGame sUI = do
    rg <- newStdGen
    let ss = randoms rg :: [Int]
    simLoop syncGame sUI ss

type RandomIntSeq = [Int]

data Event = NoEvent
             | NewGroup
             | NewSettlement Int Settlement
             deriving Eq

generateSettlement :: Int -> Int -> Game -> (Game, Int)
generateSettlement grId seed game = addSettlement game seed grId pos
                                    where world = gameWorld game
                                          pos = randomLandPos world seed

instance Show Event where
    show NoEvent = "<nothing>"
    show NewGroup = "Creating new group"
    show (NewSettlement grId sett) = let pos = settlPos sett
                                     in "Creating new settlement for group " ++ (show grId) ++ " at " ++ (show pos)

simEvent :: RandomIntSeq -> (TVar Game) -> IO (Event, RandomIntSeq)
simEvent randomSeq syncGame = do
    game <- atomRead syncGame
    case randomValue' of
        0 -> return (NoEvent, tail randomSeq)
        1 -> let groupIds = M.keys $ gameGroups game
             in if length groupIds > 0
                then let grId = head groupIds
                     in do let f :: Game -> (Game, Id) = generateSettlement grId randomValue''
                           (game',settlId) :: (Game, Id) <- atomUpdateT syncGame f
                           let settl = getSettlement game' settlId
                           let e :: Event = NewSettlement grId settl
                           return (e, tail randomSeq)
                else return (NoEvent, tail randomSeq)
    where randomValue = head randomSeq
          randomValue' = randomValue `mod` 2
          randomValue'' = head $ tail randomSeq

executeEvent syncGame NoEvent = return ()

executeEvent syncGame NewGroup = return ()

executeEvent syncGame (NewSettlement groupId settl) = return ()

simLoop :: (TVar Game) -> (TVar UI) -> RandomIntSeq -> IO ()
simLoop syncGame sUI randomInts = do
    lockScreen sUI
    (event,randomInts') <- simEvent randomInts syncGame
    executeEvent syncGame event
    if event==NoEvent then return() else drawNews $ (show event)
    releaseScreen sUI
    threadDelay 1000000
    simLoop syncGame sUI randomInts'

