{-# LANGUAGE ScopedTypeVariables #-}

module Civs.Base where

import Data.List
import Control.Concurrent.STM

-------------------------------------------------
-- Basic
-------------------------------------------------

hasRepetitions :: (Eq a) => [a] -> Bool
hasRepetitions xs = nub xs /= xs

assert false msg _ = error ("Assertion failed: " ++ msg)
assert true  msg x = x

atomRead = atomically . readTVar

atomUpdate :: TVar a -> (a -> a) -> IO a
atomUpdate var f = atomically (do value :: a <- readTVar var
                                  let value' :: a = (f value)
                                  writeTVar var value'
                                  return value')

atomUpdateT :: TVar a -> (a -> (a,b)) -> IO (a,b)
atomUpdateT var f = atomically (do value :: a <- readTVar var
                                   let (value',res) = (f value)
                                   writeTVar var value'
                                   return (value',res))

