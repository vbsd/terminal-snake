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

data SPosition = SPosition Integer Integer deriving (Show, Eq)

data SDirection = SUp | SRight | SDown | SLeft deriving (Show, Eq)

data SSnake = SSnake [SPosition] SDirection deriving (Show, Eq)

data SStatus = SStarted | SInProgress | SOver deriving (Show, Eq)

data SState = SState SSnake SStatus deriving (Show, Eq)

data STimeUnitEvent = STimeUnitEvent

lowerGridBound :: Integer
lowerGridBound = 1

upperGridBound :: Integer
upperGridBound = 20

snakeMatrix :: SSnake -> [String]
snakeMatrix (SSnake body _) = [line y | y <- [upperGridBound, upperGridBound - 1 .. lowerGridBound]]
  where
    cell x y = if SPosition x y `elem` body then "()" else "  "
    line y = concat [cell x y | x <- [lowerGridBound .. upperGridBound]]

drawUi :: SState -> [T.Widget ()]
drawUi (SState snake _) = [C.center $ B.border $ vBox (fmap str (snakeMatrix snake))]

updateDirection :: SSnake -> SDirection -> SSnake
updateDirection (SSnake body oldDirection) direction = SSnake body newDirection
  where
    newDirection = case (oldDirection, direction) of
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
appEvent state@(SState snake status) (T.VtyEvent (V.EvKey keyAction [])) = case keyAction of
  V.KUp -> action SUp
  V.KRight -> action SRight
  V.KDown -> action SDown
  V.KLeft -> action SLeft
  V.KEsc -> M.halt state
  _ -> M.continue state
  where
    action newDirection = M.continue $ SState (updateDirection snake newDirection) status
appEvent (SState (SSnake body direction) status) (T.AppEvent STimeUnitEvent) = case direction of
  SUp -> M.continue $ updated x (y+1)
  SRight -> M.continue $ updated (x+1) y
  SDown -> M.continue $ updated x (y-1)
  SLeft -> M.continue $ updated (x-1) y
  where
    x = case body of (SPosition x' _) : _ -> x'; _ -> error "can't be"
    y = case body of SPosition _ y' : _ -> y'; _ -> error "can't be"
    updated newX newY = SState (SSnake ((SPosition newX newY) : (reverse $ tail $ reverse body)) direction) status
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
  let initialState = SState (SSnake [SPosition x 10 | x <- [13, 12 .. 2]] SRight) SStarted
  _ <- forkIO $ pingSnake eventChan
  _ <- M.customMain initialVty buildVty (Just eventChan) app initialState
  return ()