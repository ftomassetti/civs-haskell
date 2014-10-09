module Civs.ConsoleExplorer where

import System.Console.ANSI
import System.IO
import Civs.Model
import qualified Data.Sequence as S

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
type Screen = S.Seq (S.Seq Cell)

data Explorer = Explorer { explorerPos :: Position, explorerScreen :: Screen }

gameLoop :: Game -> Explorer -> IO()
gameLoop game explorer = do
  let hero = explorerPos explorer
  explorer'  <- drawWorld game explorer
  explorer'' <- drawHero  hero explorer
  input <- getInput
  case input of
    Exit -> handleExit
    _    -> handleDir game explorer' input

data ScreenPos = ScreenPos { spRow :: Int, spCol :: Int}

setScreenCell screen screenPos cell = S.update r row' screen
                                      where ScreenPos r c = screenPos
                                            row = S.index screen r
                                            row' = S.update c cell row

getScreenCell screen screenPos = S.index row c
                                 where ScreenPos r c = screenPos
                                       row = S.index screen r

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
                           , SetColor Foreground Dull Green ]
                       putStr "F"

drawBiome Jungle = do  setSGR [ SetConsoleIntensity BoldIntensity
                           , SetColor Foreground Vivid Green ]
                       putStr "J"

drawBiome Alpine = do  setSGR [ SetConsoleIntensity BoldIntensity
                           , SetColor Foreground Dull Magenta ]
                       putStr "A"

drawBiome Grassland = do  setSGR [ SetConsoleIntensity BoldIntensity
                           , SetColor Foreground Dull Green ]
                          putStr "_"

drawBiome SandDesert = do  setSGR [ SetConsoleIntensity BoldIntensity
                            , SetColor Foreground Dull Yellow ]
                           putStr "_"

drawBiome Savanna = do     setSGR [ SetConsoleIntensity BoldIntensity
                            , SetColor Foreground Dull Yellow ]
                           putStr "T"

drawBiome RockDesert = do  setSGR [ SetConsoleIntensity BoldIntensity
                            , SetColor Foreground Dull Cyan ]
                           putStr "_"

drawBiome _ = do  setSGR [ SetConsoleIntensity BoldIntensity
                          , SetColor Foreground Dull Red ]
                  putStr "?"

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
  let cells = [(x,y) | x <- [0..(screenWidth-1)], y <- [0..(screenHeight-1)]]
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
initialScreen = S.replicate screenHeight row
                where row = S.replicate screenWidth EmptyCell

initialExplorer :: Explorer
initialExplorer      = Explorer pos initialScreen
                       where pos = Pos 100 100