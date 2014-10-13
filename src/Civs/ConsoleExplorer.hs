module Civs.ConsoleExplorer where

import System.Console.ANSI
import System.IO
import Civs.Model
import Data.Char
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Sequence as S
import qualified System.Console.Terminal.Size as TS
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

atomRead = atomically . readTVar

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

data Explorer = Explorer { explorerPos :: Position, explorerScreen :: Screen, explorerSyncScreen :: MVar () }

drawStatus (Pos heroX heroY) game explorer = do
  setCursorPosition screenHeight 0
  setSGR [ SetConsoleIntensity BoldIntensity
       , SetColor Foreground Vivid Blue ]
  let w = gameWorld game
  let biome = getBiome w (Pos heroX heroY)
  putStr $ "[" ++ show(heroX) ++ ", " ++ show(heroY) ++ "] "++(show biome)++ "       "

drawNews msg = do setCursorPosition (screenHeight+1) 0
                  setSGR [ SetConsoleIntensity BoldIntensity
                         , SetColor Foreground Vivid Black ]
                  putStr $ "News: " ++ msg ++ "                 "

gameLoop :: (TVar Game) -> Explorer -> IO()
gameLoop syncGame explorer = do
  let syncScreen = explorerSyncScreen explorer
  let hero = explorerPos explorer
  takeMVar syncScreen
  game <- atomRead syncGame
  explorer'  <- drawWorld game explorer
  explorer'' <- drawHero  hero explorer'
  drawStatus hero game explorer''
  putMVar syncScreen ()
  input <- getInput
  case input of
    Exit -> handleExit
    _    -> handleDir syncGame explorer'' input

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
                       putStr [chr 9651]

drawBiome Iceland = do setSGR [ SetConsoleIntensity BoldIntensity
                           , SetColor Foreground Vivid White ]
                       putStr "*" -- [chr 9728]

drawBiome Tundra = do  setSGR [ SetConsoleIntensity BoldIntensity
                           , SetColor Foreground Vivid Black ]
                       putStr [chr 937]

drawBiome Forest = do  setSGR [ SetConsoleIntensity BoldIntensity
                           , SetColor Foreground Dull Green ]
                       putStr [chr 937] -- 	003A9

drawBiome Jungle = do  setSGR [ SetConsoleIntensity BoldIntensity
                           , SetColor Foreground Vivid Green ]
                       putStr [chr 9641]

drawBiome Alpine = do  setSGR [ SetConsoleIntensity BoldIntensity
                           , SetColor Foreground Dull Black ]
                       putStr [chr 9650] --[chr 9968]

drawBiome Grassland = do  setSGR [ SetConsoleIntensity BoldIntensity
                           , SetColor Foreground Dull Green ]
                          putStr [chr 9644]-- "_"

drawBiome SandDesert = do  setSGR [ SetConsoleIntensity BoldIntensity
                            , SetColor Foreground Dull Yellow ]
                           putStr [chr 9926]

drawBiome Savanna = do     setSGR [ SetConsoleIntensity BoldIntensity
                            , SetColor Foreground Dull Yellow ]
                           putStr [chr 9641]

drawBiome RockDesert = do  setSGR [ SetConsoleIntensity BoldIntensity
                            , SetColor Foreground Dull Black ]
                           putStr [chr 9926]

drawBiome _ = do  setSGR [ SetConsoleIntensity BoldIntensity
                          , SetColor Foreground Dull Red ]
                  putStr "?"

drawCells :: Game -> Explorer -> [(Int,Int)] -> IO Explorer
drawCells game explorer [] = return explorer
drawCells game explorer ((x,y):cells) =    do setCursorPosition y x
                                              let Pos heroX heroY = explorerPos explorer
                                              let screen = explorerScreen explorer
                                              let w = gameWorld game
                                              let (Pos baseX baseY) = heroPosOnScreen (Pos heroX heroY) explorer
                                              let biome = getBiome w (Pos (x+heroX-baseX) (y+heroY-baseY))
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

heroPosOnScreen (Pos heroX heroY) explorer =
  let hw = screenWidth `div` 2
      hh = screenHeight `div` 2
  in Pos hw hh

drawHero pos@(Pos heroX heroY) explorer = do
  let (Pos heroOnScreenX heroOnScreenY) = heroPosOnScreen pos explorer
  setCursorPosition heroOnScreenY heroOnScreenX
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Red ]
  putStr "@"
  return explorer { explorerScreen = setScreenCell (explorerScreen explorer) (ScreenPos heroOnScreenX heroOnScreenY) CellPlayer }

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

newCoord input heroX heroY world = case input of
    Up    -> Pos heroX (max (heroY - 1) 0)
    Down  -> Pos heroX (min (heroY + 1) ((getHeight world) - 1))
    Civs.ConsoleExplorer.Left  -> Pos (max (heroX - 1) 0) heroY
    Civs.ConsoleExplorer.Right -> Pos (min (heroX + 1) ((getWidth world) - 1)) heroY

-- given a world and a direction, 'adjust' the hero's position, and loop
-- with our updated hero
handleDir :: (TVar Game) -> Explorer -> Input -> IO()
handleDir syncGame explorer input = do
    game <- atomRead syncGame
    let w = gameWorld game
    let Pos heroX heroY = explorerPos explorer
    let nc = newCoord input heroX heroY w
    gameLoop syncGame (explorer { explorerPos = nc })

dynScreenWidth  = do res <- TS.size
                     case res of
                       Just (TS.Window h w) -> return w
                       Nothing -> return 80

dynScreenHeight = do res <- TS.size
                     case res of
                       Just (TS.Window h w) -> return h
                       Nothing -> return 30

initScreen = do
    hSetEcho stdin False
    hSetBuffering stdin  NoBuffering
    hSetBuffering stdout NoBuffering
    hideCursor
    setTitle "Civs"
    clearScreen

screenWidth = 80
screenHeight = 30

initialScreen :: Screen
initialScreen = S.replicate screenHeight row
                where row = S.replicate screenWidth EmptyCell

initialExplorer :: MVar () -> Explorer
initialExplorer syncScreen = Explorer pos initialScreen syncScreen
                             where pos = Pos 100 100