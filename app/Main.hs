{-# LANGUAGE TemplateHaskell #-}

import           Brick.AttrMap
import           Brick.BChan
import qualified Brick.Main                    as M
import qualified Brick.Types                   as T
import qualified Brick.Widgets.Border          as B
import qualified Brick.Widgets.Center          as C
import           Brick.Widgets.Core
import           Control.Concurrent
import           Control.Monad
import           Graphics.Vty
import           Graphics.Vty.CrossPlatform
import qualified Graphics.Vty                  as V
import           System.Random
import           Prelude                 hiding ( Left
                                                , Right
                                                )
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Mtl (use, (.=), (%=), zoom)
import qualified Brick.Widgets.Edit as M

data Position = Position
  { _x :: Int
  , _y :: Int
  } deriving (Show, Eq)

data Direction = Up | Right | Down | Left deriving (Show, Eq)

data Snake = Snake
  { _body :: [Position]
  , _direction :: Direction
  } deriving (Show, Eq)

data Status = Started | InProgress | GameOver deriving (Show, Eq)

data State = State
  { _snake :: Snake
  , _status :: Status
  , _food :: [Position]
  , _rng :: StdGen
  } deriving (Show)

makeLenses ''State
makeLenses ''Snake
makeLenses ''Position

data TimeUnitEvent = TimeUnitEvent

lowerGridBound :: Int
lowerGridBound = 1

upperGridBound :: Int
upperGridBound = 21

snakeMatrix :: State -> [String]
snakeMatrix state = case _status state of
  Started    -> grid
  InProgress -> grid
  GameOver   -> mergedGrid
 where
  mergedGrid = zipWith mergedLine loserGrid grid
  loserGrid = reverse [ loserLine y | y <- [lowerGridBound .. upperGridBound] ]
  grid      = reverse [ line y | y <- [lowerGridBound .. upperGridBound] ]
  cell x y
    | Position x y `elem` (_body . _snake $ state) = "()"
    | Position x y `elem` _food state = "[]"
    | otherwise = "  "
  line y = concat [ cell x y | x <- [lowerGridBound .. upperGridBound] ]
  loserLine y = if even y
    then replicate lineLength ' '
    else take lineLength $ drop y $ cycle "YOU LOSE  "
  lineLength = (upperGridBound - lowerGridBound + 1) * 2
  mergedLine = zipWith (\a b -> if a == ' ' then b else a)

drawUi :: State -> [T.Widget ()]
drawUi state = [C.center $ B.border $ vBox (fmap str (snakeMatrix state))]

updateDirection :: Direction -> Snake -> Snake
updateDirection direction (Snake body oldDirection)
  | (direction == Left || direction == Right) && vertical = Snake body direction
  | (direction == Up || direction == Down) && horizontal = Snake body direction
  | otherwise = Snake body oldDirection
 where
  horizontal = case body of
    Position _ y : Position _ y' : _ -> y == y'
    _ -> True
  vertical = case body of
    Position x _ : Position x' _ : _ -> x == x'
    _ -> True

randomFood :: StdGen -> [Position] -> ([Position], StdGen)
randomFood rng body = if null emptyCells
  then ([], rng)
  else case randomR (0 :: Int, length emptyCells - 1) rng of
    (idx, rng') -> ([emptyCells !! idx], rng')
 where
  emptyCells =
    [ Position x y
    | x <- [lowerGridBound .. upperGridBound]
    , y <- [lowerGridBound .. upperGridBound]
    , Position x y `notElem` body
    ]

moveSnake :: State -> State
moveSnake (State (Snake body direction) status food rng) = State
  (Snake newBody direction)
  newStatus
  newFood
  newRng
 where
  Position currentHeadX currentHeadY = head body
  newHead                            = case direction of
    Up    -> Position currentHeadX (currentHeadY + 1)
    Right -> Position (currentHeadX + 1) currentHeadY
    Down  -> Position currentHeadX (currentHeadY - 1)
    Left  -> Position (currentHeadX - 1) currentHeadY
  willBeOutOfBounds = case newHead of
    Position newX newY ->
      min newX newY
        <      lowerGridBound
        ||     max newX newY
        >      upperGridBound
        ||     newHead
        `elem` tail newBody
  willEatFood = newHead `elem` food
  newStatus   = if willBeOutOfBounds then GameOver else status
  newBody
    | status == GameOver = body
    | willEatFood = newHead : body
    | otherwise = newHead : init body
  (newFood, newRng) =
    if willEatFood then randomFood rng newBody else (food, rng)

appEvent :: T.BrickEvent () TimeUnitEvent -> T.EventM () State ()
appEvent e = case e of
  T.AppEvent TimeUnitEvent -> T.modify moveSnake
  T.VtyEvent (V.EvKey keyAction _) -> case keyAction of
      V.KUp -> snake %= updateDirection Up
      V.KDown -> snake %= updateDirection Down
      V.KLeft -> snake %= updateDirection Left
      V.KRight -> snake %= updateDirection Right
      V.KEsc   -> M.halt
      _        -> return ()
  _ -> return ()

app :: M.App State TimeUnitEvent ()
app = M.App { M.appDraw         = drawUi
            , M.appStartEvent   = return ()
            , M.appHandleEvent  = appEvent
            , M.appAttrMap      = const $ attrMap V.defAttr []
            , M.appChooseCursor = M.neverShowCursor
            }

pingSnake :: Brick.BChan.BChan TimeUnitEvent -> IO b
pingSnake chan = forever $ do
  _ <- Brick.BChan.writeBChanNonBlocking chan TimeUnitEvent
  threadDelay 100000
  return ()

main :: IO ()
main = do
  eventChan <- Brick.BChan.newBChan 1
  let buildVty = Graphics.Vty.CrossPlatform.mkVty Graphics.Vty.defaultConfig
  initialVty <- buildVty
  seed       <- randomIO :: IO Int
  let rng          = mkStdGen seed
      body         = [ Position x 10 | x <- [12, 11 .. 10] ]
      (food, _)    = randomFood rng body
      initialState = State (Snake body Right) Started food rng
  _ <- forkIO $ pingSnake eventChan
  _ <- M.customMain initialVty buildVty (Just eventChan) app initialState
  return ()
