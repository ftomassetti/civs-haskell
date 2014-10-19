{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Civs.ConsoleExplorer where

import System.Console.ANSI
import System.IO
import Civs.Base
import Civs.Model
import Data.Char
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Sequence as S
import qualified System.Console.Terminal.Size as TS
import Civs.UI.Screen
import UI.NCurses

data Input = Up
           | Down
           | Left
           | Right
           | Exit
           | NoInput
           deriving (Eq)

data UI = UI { explorerPos :: Position, explorerScreen :: Screen, explorerSyncScreen :: MVar () }

drawStatus (Pos heroX heroY) game explorer = do
  setCursorPosition (screenHeight+2) 2
  setSGR [ SetConsoleIntensity BoldIntensity
       , SetColor Foreground Vivid Blue ]
  let w = gameWorld game
  let pos = Pos heroX heroY
  let biome = getBiome w pos
  let posMsg = case getSettlementAt game pos of
                    Nothing -> (show biome)
                    Just s -> "in " ++ (show $ settlName s)
  putStr $ "[" ++ show(heroX) ++ ", " ++ show(heroY) ++ "] "++ posMsg ++ "       "

drawNews msg = do setSGR [ SetConsoleIntensity BoldIntensity
                         , SetColor Foreground Vivid Black ]
                  helper msg 3
               where helper [] row = return ()
                     helper str row = do setCursorPosition row (infoAreaLeft + 1)
                                         putStr $ (take len str)
                                         helper (drop len str) (row+1)
                                      where len = (infoAreaRight-infoAreaLeft-2)


gameLoop :: (TVar Game) -> UI -> IO()
gameLoop syncGame explorer = do
  let syncScreen = explorerSyncScreen explorer
  let hero = explorerPos explorer
  takeMVar syncScreen
  game <- atomRead syncGame
  explorer'  <- drawWorld game explorer
  explorer'' <- drawHero  hero explorer'
  drawStatus hero game explorer''
  putMVar syncScreen ()
  input :: Input <- getInput
  case input of
    Exit -> handleExit
    _    -> handleDir syncGame explorer'' input

drawBorderCells :: [ScreenPos] -> IO ()
drawBorderCells [] = return ()
drawBorderCells (ScreenPos r c : cells) = do setCursorPosition r c
                                             putStr [(chr 9553)]
                                             drawBorderCells cells

infoAreaLeft   =  82
infoAreaRight  = 115
infoAreaTop    =   1
infoAreaBottom =  80

newsAreaBottom = screenHeight+10
newsAreaRight  = infoAreaRight

-- See http://www.kreativekorp.com/software/fonts/samples/petme-0ascii.png
drawBorders = do
  setSGR [ SetConsoleIntensity BoldIntensity
           , SetColor Foreground Dull White ]

  -- Around map
  setCursorPosition 0 1
  putStr $ (replicate screenWidth (chr 9552))
  setCursorPosition (screenHeight+1) 1
  putStr $ (replicate screenWidth (chr 9552))
  let bordersLeft :: [ScreenPos] = map (\y -> ScreenPos (fromInteger y) 0) [1..(fromIntegral $ screenHeight)]
  let bordersRight :: [ScreenPos] = map (\y -> ScreenPos (fromInteger y) (fromIntegral $ screenWidth+1)) [1..(fromIntegral $ screenHeight)]
  drawBorderCells bordersLeft
  drawBorderCells bordersRight
  setCursorPosition 0 0
  putStr [(chr 9556)]
  setCursorPosition 0 (screenWidth+1)
  putStr [(chr 9574)]
  setCursorPosition (screenHeight+1) 0
  putStr [(chr 9562)]
  setCursorPosition (screenHeight+1) (screenWidth+1)
  putStr [(chr 9577)]

  -- Around info
  setCursorPosition 1 (screenWidth+3)
  putStr "News"
  setCursorPosition 0 (screenWidth+2)
  putStr $ (replicate (infoAreaRight-infoAreaLeft) (chr 9552))
  setCursorPosition (screenHeight+1) (screenWidth+2)
  putStr $ (replicate (infoAreaRight-infoAreaLeft) (chr 9552))

  setCursorPosition (screenHeight+1) 1
  putStr $ (replicate screenWidth (chr 9552))
  let bordersInfoRight :: [ScreenPos] = map (\y -> ScreenPos (fromInteger y) infoAreaRight) [1..(fromIntegral $ screenHeight)]
  drawBorderCells bordersInfoRight
  setCursorPosition 0 infoAreaRight
  putStr [(chr 9559)]
  setCursorPosition (screenHeight+1) infoAreaRight
  putStr [(chr 9565)]

  -- line below "news"
  setCursorPosition 2 (screenWidth+2)
  putStr $ (replicate (infoAreaRight-infoAreaLeft) (chr 9472))
  -- connections with line below nes
  setCursorPosition 2 (screenWidth+1)
  putStr [(chr 9567)]
  setCursorPosition 2 (infoAreaRight)
  putStr [(chr 9570)]

  --
  -- Around news
  --

  -- news: bottom line
  setCursorPosition newsAreaBottom 1
  putStr $ (replicate (newsAreaRight-1) (chr 9552))

  -- news: left and right lines
  let bordersNewsLeft :: [ScreenPos]  = map (\y -> ScreenPos (fromInteger y) 0) [(fromIntegral $ screenHeight)..(fromIntegral $ newsAreaBottom)]
  let bordersNewsRight :: [ScreenPos] = map (\y -> ScreenPos (fromInteger y) (fromIntegral $ newsAreaRight))  [(fromIntegral $ screenHeight)..(fromIntegral $ newsAreaBottom)]
  drawBorderCells bordersNewsLeft
  drawBorderCells bordersNewsRight

  -- news: bottom corners
  setCursorPosition (newsAreaBottom) 0
  putStr [(chr 9562)]
  setCursorPosition (newsAreaBottom) (newsAreaRight)
  putStr [(chr 9565)]

  -- news: top corners
  setCursorPosition (screenHeight+1) 0
  putStr [(chr 9568)]
  setCursorPosition (screenHeight+1) (newsAreaRight)
  putStr [(chr 9571)]

  --newsAreaBottom = screenHeight+10
  --newsAreaRight

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

drawCell (CellBiome biome) = drawBiome biome
drawCell CellVillage = do setSGR [ SetConsoleIntensity BoldIntensity
                              , SetColor Foreground Vivid Red ]
                          putStr "#"

drawCells :: Game -> UI -> [(Int,Int)] -> IO UI
drawCells game explorer [] = return explorer
drawCells game explorer ((x,y):cells) =    do setCursorPosition (y+1) (x+1)
                                              let Pos heroX heroY = explorerPos explorer
                                              let screen = explorerScreen explorer
                                              let w = gameWorld game
                                              let (Pos baseX baseY) = heroPosOnScreen (Pos heroX heroY) explorer
                                              let pos = (Pos (x+heroX-baseX) (y+heroY-baseY))
                                              let biome = getBiome w pos
                                              let hasVillage = containVillage game pos
                                              let toDraw = if hasVillage then CellVillage else CellBiome biome
                                              let currCellOnScreen = getScreenCell screen (ScreenPos y x)
                                              explorer' <- if currCellOnScreen /= toDraw
                                                           then do drawCell toDraw
                                                                   let screen' = setScreenCell screen (ScreenPos y x) toDraw
                                                                   return explorer { explorerScreen = screen' }
                                                           else return explorer
                                              drawCells game explorer' cells

drawWorld :: Game -> UI -> IO UI
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

getInputChar char = case char of
                        'q' -> return Exit
                        'w' -> return Up
                        's' -> return Down
                        'a' -> return Civs.ConsoleExplorer.Left
                        'd' -> return Civs.ConsoleExplorer.Right
                        _ -> getInput

-- receive a character and return our Input data structure,
-- recursing on invalid input
getInput = do
  char <- getChar
  getInputChar char

newCoord input heroX heroY world = case input of
    Up    -> Pos heroX (max (heroY - 1) 0)
    Down  -> Pos heroX (min (heroY + 1) ((getHeight world) - 1))
    Civs.ConsoleExplorer.Left  -> Pos (max (heroX - 1) 0) heroY
    Civs.ConsoleExplorer.Right -> Pos (min (heroX + 1) ((getWidth world) - 1)) heroY

-- given a world and a direction, 'adjust' the hero's position, and loop
-- with our updated hero
handleDir :: (TVar Game) -> UI -> Input -> IO()
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

initialExplorer :: MVar () -> UI
initialExplorer syncScreen = UI pos initialScreen syncScreen
                             where pos = Pos 100 100