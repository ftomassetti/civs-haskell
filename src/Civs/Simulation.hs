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
                                            let groupIds = M.keys $ gameGroups game
                                            if (length groupIds) < 5
                                                then do (game', gr) <- atomUpdateT syncGame (generateGroup (head ri))
                                                        let Name sName = groupName gr
                                                        drawNews $ "Balancer: creating group "++ sName
                                                else return () -- drawNews $ "Balancer: enough groups ("++(show $ length groupIds)++")"
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

simLoop :: (TVar Game) -> (MVar ()) -> RandomIntSeq -> IO ()
simLoop syncGame syncScreen randomInts = do
    takeMVar syncScreen
    (event,randomInts') <- simEvent randomInts syncGame
    executeEvent syncGame event
    if event==NoEvent then return() else drawNews $ (show event)
    putMVar syncScreen ()
    threadDelay 1000000
    simLoop syncGame syncScreen randomInts'

