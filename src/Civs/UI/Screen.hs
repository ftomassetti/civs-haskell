module Civs.UI.Screen where

import qualified Data.Sequence as S
import Civs.Model

data Cell = CellBiome Biome
            | CellPlayer
            | EmptyCell
            | CellVillage
            deriving (Eq)

type ScreenInfo = [String]

-- | All the info represented on the screen.
-- | The screen is divided in three ares:
-- | Map (made of Cells)
-- | Info
data Screen = Screen {
    screenCells :: S.Seq (S.Seq Cell),
    screenInfo :: ScreenInfo
}

screenWidth  = 80
screenHeight = 30

initialScreen :: Screen
initialScreen = Screen cells []
                where row = S.replicate screenWidth EmptyCell
                      cells = S.replicate screenHeight row

data ScreenPos = ScreenPos { spRow :: Int, spCol :: Int}
     deriving (Eq, Show)

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