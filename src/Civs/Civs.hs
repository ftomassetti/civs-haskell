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


generateGame :: World -> Int -> Int -> Game
generateGame world seed ngroups = helper g0 ss0 ngroups
                                  where g0 = Game 1 world []
                                        rg = mkStdGen seed
                                        ss0 = randoms rg :: [Int]
                                        helper g ss 0 = g
                                        helper g (s:ss) n = helper (generateGroup g s) ss (n-1)

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
          startSimulation syncGame syncScreen
          gameLoop syncGame e