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
import System.Random

data SPosition = SPosition Int Int deriving (Show, Eq)

data SDirection = SUp | SRight | SDown | SLeft deriving (Show, Eq)

data SSnake = SSnake [SPosition] SDirection deriving (Show, Eq)

data SStatus = SStarted | SInProgress | SOver deriving (Show, Eq)

data SState = SState SSnake SStatus [SPosition] StdGen deriving (Show)

data STimeUnitEvent = STimeUnitEvent

lowerGridBound :: Int
lowerGridBound = 1

upperGridBound :: Int
upperGridBound = 21

snakeMatrix :: SState -> [String]
snakeMatrix (SState (SSnake body _) status food _) = case status of
  SStarted -> grid
  SInProgress -> grid
  SOver -> mergedGrid
  where
    mergedGrid = zipWith mergedLine loserGrid grid
    mergedLine loserL snakeL = zipWith (\a b -> if a == ' ' then b else a) loserL snakeL
    loserGrid = reverse [loserLine y | y <- [lowerGridBound .. upperGridBound]]
    grid = reverse [line y | y <- [lowerGridBound .. upperGridBound]]
    cell x y = if SPosition x y `elem` body then "()" else if SPosition x y `elem` food then "[]" else "  "
    line y = concat [cell x y | x <- [lowerGridBound .. upperGridBound]]
    loserLine y =
      if y `mod` 2 == 0
        then take lineLength $ repeat ' '
        else take lineLength $ drop y $ concat $ repeat "YOU LOSE  "
    lineLength = (upperGridBound - lowerGridBound + 1) * 2

drawUi :: SState -> [T.Widget ()]
drawUi state = [C.center $ B.border $ vBox (fmap str (snakeMatrix state))]

updateDirection :: SSnake -> SDirection -> SSnake
updateDirection (SSnake body oldDirection) direction
  | (direction == SLeft || direction == SRight) && vertical = SSnake body direction
  | (direction == SUp || direction == SDown) && horizontal = SSnake body direction
  | otherwise = SSnake body oldDirection
  where
    horizontal = case body of
      SPosition _ y : SPosition _ y' : _ -> y == y'
      _ -> True
    vertical = case body of
      SPosition x _ : SPosition x' _ : _ -> x == x'
      _ -> True

randomFood :: StdGen -> [SPosition] -> ([SPosition], StdGen)
randomFood rng body =
  if emptyCells == []
    then ([], rng)
    else case randomR (0 :: Int, length emptyCells - 1) rng of
      (idx, rng') -> ([emptyCells !! idx], rng')
  where
    emptyCells =
      [ SPosition x y
        | x <- [lowerGridBound .. upperGridBound],
          y <- [lowerGridBound .. upperGridBound],
          not $ SPosition x y `elem` body
      ]

moveSnake :: SState -> SState
moveSnake (SState (SSnake body direction) status food rng) =
  SState (SSnake newBody direction) newStatus newFood newRng
  where
    SPosition currentHeadX currentHeadY = head body
    newHead = case direction of
      SUp -> SPosition currentHeadX (currentHeadY + 1)
      SRight -> SPosition (currentHeadX + 1) currentHeadY
      SDown -> SPosition currentHeadX (currentHeadY - 1)
      SLeft -> SPosition (currentHeadX - 1) currentHeadY
    willBeOutOfBounds = case newHead of
      SPosition newX newY ->
        min newX newY < lowerGridBound
          || max newX newY > upperGridBound
          || newHead `elem` tail newBody
    willEatFood = newHead `elem` food
    newStatus = if willBeOutOfBounds then SOver else status
    newBody =
      if status == SOver
        then body
        else
          if willEatFood
            then newHead : body
            else newHead : (reverse $ tail $ reverse body)
    (newFood, newRng) = if willEatFood then randomFood rng body else (food, rng)

appEvent :: SState -> T.BrickEvent () STimeUnitEvent -> T.EventM () (T.Next SState)
appEvent state@(SState snake status food rng) (T.VtyEvent (V.EvKey keyAction [])) = case keyAction of
  V.KUp -> action SUp
  V.KRight -> action SRight
  V.KDown -> action SDown
  V.KLeft -> action SLeft
  V.KEsc -> M.halt state
  _ -> M.continue state
  where
    action newDirection = M.continue $ SState (updateDirection snake newDirection) status food rng
appEvent state (T.AppEvent STimeUnitEvent) = M.continue $ moveSnake state
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
  seed <- randomIO :: IO Int
  let rng = mkStdGen seed
      body = [SPosition x 10 | x <- [12, 11 .. 10]]
      (food, _) = randomFood rng body
      initialState = SState (SSnake body SRight) SStarted food rng
  _ <- forkIO $ pingSnake eventChan
  _ <- M.customMain initialVty buildVty (Just eventChan) app initialState
  return ()