module Civs.UI.Screen where

import qualified Data.Sequence as S
import Civs.Model

data Cell = CellBiome Biome
            | CellPlayer
            | EmptyCell
            | CellVillage
            deriving (Eq)

-- | All the info represented on the screen
data Screen = Screen { screenCells :: S.Seq (S.Seq Cell), screenTooSmall :: Bool }

screenWidth = 80
screenHeight = 30

initialScreen :: Screen
initialScreen = Screen cells False
                where row = S.replicate screenWidth EmptyCell
                      cells = S.replicate screenHeight row

data ScreenPos = ScreenPos { spRow :: Int, spCol :: Int}

setScreenCell screen screenPos cell = screen'
                                      where ScreenPos r c = screenPos
                                            cells = screenCells screen
                                            row = S.index cells r
                                            row' = S.update c cell row
                                            cells' = S.update r row' cells
                                            screen' = screen { screenCells = cells' }

getScreenCell screen screenPos = S.index row c
                                 where ScreenPos r c = screenPos
                                       cells = screenCells screen
                                       row = S.index cells r