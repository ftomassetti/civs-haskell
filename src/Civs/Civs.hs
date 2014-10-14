{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Civs.Model
import Civs.Pickle
import Civs.Simulation

import Data.Map
import qualified Data.ByteString.Lazy as S
import System.Random
import Civs.ConsoleExplorer
import Control.Concurrent
import Control.Concurrent.STM

worldFileName = "worlds/seed_77.world"
worldBytes = S.readFile worldFileName


generateGame :: World -> Game
generateGame world = Game 1 world []

initialGame worldFileName = do
    byteString <- S.readFile worldFileName :: IO S.ByteString
    world' <- process (PickleStatus [] empty) byteString
    let world = World world'
    return $ generateGame world

main :: IO ()
main = do syncScreen <- newMVar ()
          g <- initialGame worldFileName
          syncGame :: (TVar Game)  <- atomically $ newTVar $ g
          let e = initialExplorer syncScreen
          initScreen
          startSimulation syncGame syncScreen
          gameLoop syncGame e