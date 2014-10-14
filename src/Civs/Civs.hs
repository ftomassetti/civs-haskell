{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Civs.Model
import Civs.Pickle
import Civs.Simulation

import Data.Map as M
import qualified Data.ByteString.Lazy as S
import System.Random
import Civs.ConsoleExplorer
import Control.Concurrent
import Control.Concurrent.STM
import Namegen

worldFileName = "worlds/seed_77.world"
worldBytes = S.readFile worldFileName

generateGame :: World -> LanguageSamples -> Game
generateGame world languageSamples = Game 1 world [] languageSamples

initialGame worldFileName languageSamples = do
    byteString <- S.readFile worldFileName :: IO S.ByteString
    world' <- process (PickleStatus [] empty) byteString
    let world = World world'
    return $ generateGame world languageSamples

loadAllSamples :: IO LanguageSamples
loadAllSamples = helper ["languages/citynames_ch.txt", "languages/citynames_eg.txt",
                                        "languages/citynames_fr.txt", "languages/citynames_jp.txt",
                                        "languages/citynames_de.txt", "languages/citynames_es.txt",
                                        "languages/citynames_it.txt", "languages/citynames_pl.txt"] []
                 where helper :: [String] -> [[String]] -> IO [[String]]
                       helper [] loaded = return loaded
                       helper fileNames loaded = do newlyLoaded :: [String] <- loadSamples (head fileNames)
                                                    helper (tail fileNames) (newlyLoaded:loaded)

main :: IO ()
main = do syncScreen <- newMVar ()
          namesSamples <- loadAllSamples
          g <- initialGame worldFileName namesSamples
          syncGame :: (TVar Game)  <- atomically $ newTVar $ g
          let e = initialExplorer syncScreen
          initScreen
          startSimulation syncGame syncScreen
          gameLoop syncGame e