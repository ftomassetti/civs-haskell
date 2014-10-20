{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Civs.Base
import Civs.Model
import Civs.Simulation

import qualified Data.ByteString.Lazy as S
import Civs.ConsoleExplorer
import Control.Concurrent
import Control.Concurrent.STM

worldFileName = "worlds/seed_77.world"

main :: IO ()
main = do namesSamples <- loadAllSamples
          g <- initialGame worldFileName namesSamples
          syncGame :: (TVar Game)  <- atomically $ newTVar $ g
          ui :: UI <- initialUI
          sUI :: (TVar UI) <- atomically $ newTVar $ ui
          initScreen
          drawBorders
          startSimulation syncGame sUI
          gameLoop syncGame sUI
          finalG <- atomRead syncGame
          putStrLn $ "Game: " ++ (show finalG)