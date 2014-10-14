module Civs.Base where

import Data.List
import Control.Concurrent.STM

-------------------------------------------------
-- Basic
-------------------------------------------------

atomRead = atomically . readTVar
atomWrite value = atomically . writeTVar value

hasRepetitions :: (Eq a) => [a] -> Bool
hasRepetitions xs = nub xs /= xs

assert false msg _ = error ("Assertion failed: " ++ msg)
assert true  msg x = x

