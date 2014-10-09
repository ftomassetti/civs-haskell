module Civs.ConsoleExplorer where

import System.Console.ANSI
import System.IO
import Civs.Model
import Data.Matrix

data Input = Up
           | Down
           | Left
           | Right
           | Exit
           deriving (Eq)

data Cell = CellBiome Biome
            | CellPlayer
            | EmptyCell
            deriving (Eq)

-- All the info represented on the screen
type Screen = Matrix Cell

data Explorer = Explorer { explorerPos :: Position, explorerScreen :: Screen }

gameLoop :: Game -> Explorer -> IO()
gameLoop game explorer = do
  let hero = explorerPos explorer
  clearScreen
  explorer'  <- drawWorld game explorer
  explorer'' <- drawHero  hero explorer
  input <- getInput
  case input of
    Exit -> handleExit
    _    -> handleDir game explorer' input

data ScreenPos = ScreenPos { spRow :: Int, spCol :: Int}

setScreenCell screen screenPos cell = setElem cell (r,c) screen
                                      where ScreenPos r c = screenPos

getScreenCell screen screenPos = getElem r c initialScreen
                                 where ScreenPos r c = screenPos

drawBiome :: Biome -> IO ()
drawBiome Ocean =       do  setSGR [ SetConsoleIntensity BoldIntensity
                                     , SetColor Foreground Vivid Blue ]
                            putStr "~"

drawBiome Glacier = do setSGR [ SetConsoleIntensity BoldIntensity
                           , SetColor Foreground Vivid White ]
                       putStr "*"

drawBiome Iceland = do setSGR [ SetConsoleIntensity BoldIntensity
                           , SetColor Foreground Vivid White ]
                       putStr "*"

drawBiome Tundra = do  setSGR [ SetConsoleIntensity BoldIntensity
                           , SetColor Foreground Vivid Black ]
                       putStr "T"

drawBiome Forest = do  setSGR [ SetConsoleIntensity BoldIntensity
                           , SetColor Foreground Vivid Green ]
                       putStr "F"

drawBiome Jungle = do  setSGR [ SetConsoleIntensity BoldIntensity
                           , SetColor Foreground Vivid Green ]
                       putStr "J"


drawBiome _ = return ()

drawCells :: Game -> Explorer -> [(Int,Int)] -> IO Explorer
drawCells game explorer [] = return explorer
drawCells game explorer ((x,y):cells) =    do setCursorPosition y x
                                              let Pos heroX heroY = explorerPos explorer
                                              let screen = explorerScreen explorer
                                              let w = gameWorld game
                                              let biome = getBiome w (Pos (x+heroX) (y+heroY))
                                              let currCellOnScreen = getScreenCell screen (ScreenPos y x)
                                              explorer' <- if currCellOnScreen /= CellBiome biome
                                                           then do drawBiome biome
                                                                   let screen' = setScreenCell screen (ScreenPos y x) (CellBiome biome)
                                                                   return explorer { explorerScreen = screen' }
                                                           else return explorer
                                              drawCells game explorer' cells

drawWorld :: Game -> Explorer -> IO Explorer
drawWorld game explorer = do
  let cells = [(x,y) | x <- [1..80], y <- [1..40]]
  drawCells game explorer cells

drawHero (Pos heroX heroY) explorer = do
  setCursorPosition 20 20
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Blue ]
  putStr "@"
  return explorer { explorerScreen = setScreenCell (explorerScreen explorer) (ScreenPos 20 20) CellPlayer }

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

screenWidth  = 80
screenHeight = 40

initialScreen :: Screen
initialScreen = matrix screenHeight screenWidth (\_ -> EmptyCell)

initialExplorer :: Explorer
initialExplorer      = Explorer pos initialScreen
                       where pos = Pos 100 100