module Civs.ConsoleExplorer where

import System.Console.ANSI
import System.IO
import Civs.Model

data Input = Up
           | Down
           | Left
           | Right
           | Exit
           deriving (Eq)

data Explorer = Explorer { explorerPos :: Position }

gameLoop :: Game -> Explorer -> IO()
gameLoop game explorer = do
  let hero = explorerPos explorer
  drawHero hero
  input <- getInput
  case input of
    Exit -> handleExit
    _    -> handleDir game explorer input

drawHero (Pos heroX heroY) = do
  clearScreen
  setCursorPosition heroY heroX
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Blue ]
  putStr "@"

-- when the user wants to exit we give them a thank you
-- message and then reshow the cursor
handleExit = do
  clearScreen
  setCursorPosition 0 0
  showCursor
  putStrLn "Thank you for playing!"

-- receive a character and return our Input data structure,
-- recursing on invalid input
getInput = do
  char <- getChar
  case char of
    'q' -> return Exit
    'w' -> return Up
    's' -> return Down
    'a' -> return Civs.ConsoleExplorer.Left
    'd' -> return Civs.ConsoleExplorer.Right
    _ -> getInput

-- given a world and a direction, 'adjust' the hero's position, and loop
-- with our updated hero
handleDir :: Game -> Explorer -> Input -> IO()
handleDir game explorer input = gameLoop game (explorer { explorerPos = newCoord })
  where  Pos heroX heroY = explorerPos explorer
         newCoord = case input of
                    Up    -> Pos heroX (heroY - 1)
                    Down  -> Pos heroX (heroY + 1)
                    Civs.ConsoleExplorer.Left  -> Pos (heroX - 1) heroY
                    Civs.ConsoleExplorer.Right -> Pos (heroX + 1) heroY