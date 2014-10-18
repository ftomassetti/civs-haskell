{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Civs.Base
import Civs.Model
import Civs.Pickle
import Civs.Simulation

import Data.Map.Strict as M
import qualified Data.ByteString.Lazy as S
import System.Random
import Civs.ConsoleExplorer
import Control.Concurrent
import Control.Concurrent.STM
import Namegen

worldFileName = "worlds/seed_77.world"
worldBytes = S.readFile worldFileName

main :: IO ()
main = do syncScreen <- newMVar ()
          namesSamples <- loadAllSamples
          g <- initialGame worldFileName namesSamples
          syncGame :: (TVar Game)  <- atomically $ newTVar $ g
          let e = initialExplorer syncScreen
          initScreen
          startSimulation syncGame syncScreen
          gameLoop syncGame e
          finalG <- atomRead syncGame
          putStrLn $ "Game: " ++ (show finalG)