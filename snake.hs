module Main where

import Brick.AttrMap
import Brick.BChan
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core
import Control.Concurrent
import Control.Monad
import Graphics.Vty
import qualified Graphics.Vty as V

data SPosition = SPosition
  { sRow :: Integer,
    sColumn :: Integer
  }
  deriving (Show, Eq)

data SDirection = SUp | SRight | SDown | SLeft
  deriving (Show, Eq)

data SSnake = SSnake
  { sBody :: [SPosition],
    sDirection :: SDirection
  }
  deriving (Show, Eq)

data SStatus = SStarted | SInProgress | SOver deriving (Show, Eq)

data SState = SState
  { sSnake :: SSnake,
    sStatus :: SStatus
  }
  deriving (Show, Eq)

data STimeUnitEvent = STimeUnitEvent

lowerGridBound :: Integer
lowerGridBound = 1

upperGridBound :: Integer
upperGridBound = 20

snakeMatrix :: SSnake -> [String]
snakeMatrix snake = [line n | n <- [lowerGridBound .. upperGridBound]]
  where
    cell r c = if SPosition {sRow = r, sColumn = c} `elem` (sBody snake) then "()" else "  "
    line n = concat [cell n c | c <- [lowerGridBound .. upperGridBound]]

drawUi :: SState -> [T.Widget ()]
drawUi state = [C.center $ B.border $ vBox (fmap str (snakeMatrix $ sSnake state))]

updateDirection :: SSnake -> SDirection -> SSnake
updateDirection snake direction = SSnake {sBody = sBody snake, sDirection = newDirection}
  where
    newDirection = case (sDirection snake, direction) of
      (SUp, SLeft) -> SLeft
      (SUp, SRight) -> SRight
      (SRight, SUp) -> SUp
      (SRight, SDown) -> SDown
      (SDown, SLeft) -> SLeft
      (SDown, SRight) -> SRight
      (SLeft, SUp) -> SUp
      (SLeft, SDown) -> SDown
      (dir, _) -> dir

appEvent :: SState -> T.BrickEvent () STimeUnitEvent -> T.EventM () (T.Next SState)
appEvent state (T.VtyEvent (V.EvKey keyAction [])) = case keyAction of
  V.KUp -> action SUp
  V.KRight -> action SRight
  V.KDown -> action SDown
  V.KLeft -> action SLeft
  V.KEsc -> M.halt state
  _ -> M.continue state
  where
    action newDirection = M.continue SState {sStatus = sStatus state, sSnake = updateDirection (sSnake state) newDirection}
appEvent state (T.AppEvent STimeUnitEvent) = case sDirection $ sSnake state of
  SUp -> M.continue $ updated (row -1) col
  SRight -> M.continue $ updated row (col + 1)
  SDown -> M.continue $ updated (row + 1) col
  SLeft -> M.continue $ updated row (col -1)
  where
    body = sBody $ sSnake state
    row = sRow $ head body
    col = sColumn $ head body
    updated r c =
      SState
        { sStatus = sStatus state,
          sSnake =
            SSnake
              { sBody = SPosition {sRow = r, sColumn = c} : (reverse $ tail $ reverse body),
                sDirection = sDirection $ sSnake state
              }
        }
appEvent state _ = M.continue state

app :: M.App SState STimeUnitEvent ()
app =
  M.App
    { M.appDraw = drawUi,
      M.appStartEvent = return,
      M.appHandleEvent = appEvent,
      M.appAttrMap = const $ attrMap V.defAttr [],
      M.appChooseCursor = M.neverShowCursor
    }

pingSnake :: Brick.BChan.BChan STimeUnitEvent -> IO b
pingSnake chan = forever $ do
  _ <- Brick.BChan.writeBChanNonBlocking chan STimeUnitEvent
  threadDelay 100000
  return ()

main :: IO ()
main = do
  eventChan <- Brick.BChan.newBChan 1
  let buildVty = Graphics.Vty.mkVty Graphics.Vty.defaultConfig
  initialVty <- buildVty
  let initialState = SState {sSnake = SSnake {sBody = [SPosition {sRow = i, sColumn = 10} | i <- [9, 10 .. 18]], sDirection = SUp}, sStatus = SStarted}
  _ <- forkIO $ pingSnake eventChan
  _ <- M.customMain initialVty buildVty (Just eventChan) app initialState
  return ()