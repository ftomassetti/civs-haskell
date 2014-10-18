{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Civs.ModelTest where

import Test.Framework
import Namegen
import Civs.Model

game0 :: IO Game
game0 = do samples <- loadAllSamples
           initialGame "worlds/seed_77.world" samples

genLang :: Int -> IO Language
genLang seed = do allSamples <- loadAllSamples
                  let sample = extractRandomSample allSamples seed
                  return $ generateLanguage sample seed

test_numberOfGroupsEmpty = do
    g <- game0
    assertEqual 0 (numberOfGroups g)

test_numberOfGroupsNonEmpty = do
    g <- game0
    l <- genLang 123
    let (g',_) = insertGroup g (Group (Name "aName") (Pos 123 123) l)
    assertEqual 1 (numberOfGroups g')

test_numberOfSettlementsEmpty = do
    g <- game0
    assertEqual 0 (numberOfSettlements g)

test_numberOfSettlementsNonEmpty = do
    g <- game0
    let (g',_) = insertSettlement g (Settlement 123 (Name "aName") (Pos 123 123))
    assertEqual 1 (numberOfSettlements g')

