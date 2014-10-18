{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Civs.ModelTest where

import Test.Framework
import Civs.Model

game0 :: IO Game
game0 = do samples <- loadAllSamples
           initialGame "worlds/seed_77.world" samples

test_numberOfGroupsEmpty = do g <- game0
                              assertEqual 0 (numberOfGroups g)

