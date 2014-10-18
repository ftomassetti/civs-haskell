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
                  return $ generateLanguageFromSamples sample seed

test_nextId = do
    g <- game0
    let (id1,g') = nextId g
    assertEqual 1 id1
    let (id2,g'') = nextId g'
    assertEqual 2 id2

test_numberOfGroupsEmpty = do
    g <- game0
    assertEqual 0 (numberOfGroups g)

test_numberOfGroupsNonEmpty = do
    g <- game0
    l <- genLang 123
    let (g',_) = insertGroup g (Group (Name "aName") (Pos 123 123) l)
    assertEqual 1 (numberOfGroups g')

test_insertGroupMultipleCalls = do
    g <- game0
    l <- genLang 123
    let (g',_)   = insertGroup g   (Group (Name "aName") (Pos 123 123) l)
    let (g'',_)  = insertGroup g'  (Group (Name "aName") (Pos 123 123) l)
    let (g''',_) = insertGroup g'' (Group (Name "aName") (Pos 123 123) l)
    assertEqual 0 (numberOfGroups g)
    assertEqual 1 (numberOfGroups g')
    assertEqual 2 (numberOfGroups g'')
    assertEqual 3 (numberOfGroups g''')

test_numberOfSettlementsEmpty = do
    g <- game0
    assertEqual 0 (numberOfSettlements g)

test_numberOfSettlementsNonEmpty = do
    g <- game0
    let (g',_) = insertSettlement g (Settlement 123 (Name "aName") (Pos 123 123))
    assertEqual 1 (numberOfSettlements g')

test_insertSettlementMultipleCalls = do
    g <- game0
    let (g',_) = insertSettlement g (Settlement 123 (Name "aName") (Pos 123 123))
    let (g'',_) = insertSettlement g' (Settlement 123 (Name "aName") (Pos 123 123))
    let (g''',_) = insertSettlement g'' (Settlement 123 (Name "aName") (Pos 123 123))
    assertEqual 0 (numberOfSettlements g)
    assertEqual 1 (numberOfSettlements g')
    assertEqual 2 (numberOfSettlements g'')
    assertEqual 3 (numberOfSettlements g''')

