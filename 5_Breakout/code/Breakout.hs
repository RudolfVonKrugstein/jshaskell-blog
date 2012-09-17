{-# LANGUAGE Arrows #-}

module Main where

import JavaScript hiding (Event)
import Coroutine
import Data.IORef
import Control.Arrow
import Data.VectorSpace
import Data.List

-- technical data
data Input = KeyUp Int | KeyDown Int deriving (Eq)

type Vector2D = (Double, Double)

canvasName = "canvas3"

-- entry point
main = setOnLoad initilize

initilize = do
  state <- newIORef mainCoroutine
  input <- newIORef ([] :: [Input])
  setOnKeyDown canvasName (onKeyDown input)
  setOnKeyUp   canvasName (onKeyUp input)
  setInterval 20.0 (update state input)

-- input
onKeyDown :: IORef [Input] -> Int-> IO ()
onKeyDown input keyCode = do
  i <- readIORef input
  let i' = i ++ [KeyDown keyCode]
  writeIORef input i'

onKeyUp :: IORef [Input] -> Int-> IO ()
onKeyUp input keyCode = do
  i <- readIORef input
  let i' = i ++ [KeyUp keyCode]
  writeIORef input i'
  

-- draw a gamestate
draw :: GameState -> IO ()
draw gs = do
  ctx <- getContext2d canvasName
  clear ctx
  -- draw player
  setFillColor ctx playerColor
  let pRect = playerRect . player $ gs
  fillRect ctx (x pRect) (y pRect) (width pRect) (height pRect)
  --draw blocks
  mapM_ (drawBlock ctx) $ blocks gs
  --draw ball
  setFillColor ctx ballColor
  let (x,y) = ballPos . ball $ gs
  fillCircle ctx x y ballRadius

drawBlock :: Context2D -> BlockState -> IO ()
drawBlock ctx bs = do
  setFillColor ctx blockColor
  let bRect = blockRect bs
  fillRect ctx (x bRect) (y bRect) (width bRect) (height bRect)

-- update function
update :: IORef MainCoroutineType -> IORef (Event Input) -> IO ()
update state input = do
  co <- readIORef state
  i <- readIORef input
  writeIORef input ([] :: [Input])
  let (gs, co') = runC co i
  draw gs
  writeIORef state co'

-- Game data
type Vector = (Double, Double)

data PlayerState = PlayerState {xPos :: Double}
data BallState = BallState {ballPos :: Vector2D}
data BlockState = BlockState {blockPos :: Vector2D, fade :: Double}

data GameState = GameState {player :: PlayerState,
                            ball :: BallState,
                            blocks :: [BlockState]}

data BallCollision = LeftBounce | RightBounce | UpBounce | DownBounce
data BlockCollision = BlockCollision
data Rect = Rect { x::Double, y::Double, width ::Double, height::Double}
-- game values
screenWidth = 600.0
screenHeight = 400.0
playerColor = "black"

ballColor = "red"
playerYPos = screenHeight - playerHeight
playerHeight = 15.0
playerWidth = 40.0
ballRadius = 5.0

blockWidth = 60.0
blockHeight = 20.0
blockColor = "blue"

initBallState = BallState ((screenWidth / 2.0), (screenHeight - 50.0))
initBallSpeed = (3.0, -3.0)

initPlayerState = PlayerState ((screenWidth - playerWidth) / 2.0)

initBlockStates = [BlockState (x,y) 1.0 | x <- [20.0, 140.0, 240.0, 340.0, 440.0, 520.0], y <- [60.0, 100.0, 140.0, 180.0, 220.0, 260.0]]

playerSpeed = 5.0 --the speed with which the player moves

leftKeyCode = 37
rightKeyCode = 39

-- Game logic
type MainCoroutineType = Coroutine (Event Input) GameState

mainCoroutine :: MainCoroutineType
mainCoroutine = proc inEvents -> do
  plState <- playerState -< inEvents
  rec
    let (ballColls', blockColls) = ballBlocksCollisions oldBlState oldBlocksStates
    let ballColls = (ballWallCollisions oldBlState) ++ (ballPlayerCollisions plState oldBlState) ++ ballColls'
    blState        <- ballState             -< ballColls
    blocksStates   <- blocksStatesA          -< blockColls
    oldBlState     <- delay initBallState   -< blState
    oldBlocksStates<- delay initBlockStates -< blocksStates
  returnA -< GameState plState blState blocksStates

ballBlocksCollisions :: BallState -> [BlockState] -> (Event BallCollision, [Event BlockCollision])
ballBlocksCollisions ballState blocksStates =
  let ballR = ballRect ballState
      foldStep (ballC, blockC) blockState =
        if rectOverlap ballR (blockRect blockState) then
          (ballRectCollisions ballState (blockRect blockState) ++ ballC,blockC ++ [[BlockCollision]])
        else
          (ballC, blockC ++ [[]])
   in foldl' foldStep ([],[]) blocksStates

blockState :: BlockState -> Coroutine (Event BlockCollision) (Maybe BlockState)
blockState initState = scanE (\_ _ -> Nothing) (Just initState)

blocksStatesA :: Coroutine ([Event BlockCollision]) ([BlockState])
blocksStatesA = manager $ map blockState initBlockStates

playerState :: Coroutine (Event Input) PlayerState
playerState = proc inEvents -> do
  vel <- playerVelocity -< inEvents
  xPos <- boundedIntegrate (0.0,screenWidth-playerWidth) (xPos initPlayerState)  -< vel
  returnA -< PlayerState xPos

playerVelocity :: Coroutine (Event Input) Double
playerVelocity = proc inEvents -> do
  leftDown <- keyDown leftKeyCode -< inEvents
  rightDown <- keyDown rightKeyCode -< inEvents
  returnA -< if leftDown then -playerSpeed else (if rightDown then playerSpeed else 0.0)

ballWallCollisions :: BallState -> (Event BallCollision)
ballWallCollisions (BallState (bx,by)) =
  map snd . filter fst $ [(bx < ballRadius,                LeftBounce),
                          (bx > screenWidth - ballRadius,  RightBounce),
                          (by < ballRadius, UpBounce)]

ballRectCollisions :: BallState -> Rect -> (Event BallCollision)
ballRectCollisions (BallState (bx, by)) (Rect rx ry rw rh) =
  map snd . filter fst $ [(bx <= rx,       RightBounce),
                          (bx >= rx + rw, LeftBounce),
                          (by <= ry,       DownBounce),
                          (by >= ry + rh, UpBounce)]

ballPlayerCollisions :: PlayerState -> BallState -> (Event BallCollision)
ballPlayerCollisions playerState ballState =
  if rectOverlap (playerRect playerState) (ballRect ballState)
  then ballRectCollisions ballState (playerRect playerState)
  else []

ballState :: Coroutine (Event BallCollision) BallState
ballState = proc collEvents -> do
  vel <- ballVelocity -< collEvents
  pos <- scan (^+^) (ballPos initBallState) -< vel
  returnA -< BallState pos

ballVelocity :: Coroutine (Event BallCollision) Vector2D
ballVelocity = scanE bounce initBallSpeed
  where
    bounce :: Vector2D -> BallCollision -> Vector2D
    bounce (vx,vy) coll = case coll of
      LeftBounce -> (abs(vx), vy)
      RightBounce -> (-abs(vx), vy)
      UpBounce -> (vx, abs(vy))
      DownBounce -> (vx, -abs(vy))
  
  	
  
-- helper functions
keyDown :: Int -> Coroutine (Event Input) Bool
keyDown code = scanE step False 
  where
  step old input
    | input == KeyUp code   = False
    | input == KeyDown code = True
    | otherwise             = old

rectOverlap :: Rect -> Rect -> Bool
rectOverlap r1 r2
  | x r1 >= x r2 + width r2 = False
  | x r2 >= x r1 + width r1 = False
  | y r1 >= y r2 + height r2 = False
  | y r2 >= y r1 + height r1 = False
  | otherwise                = True

playerRect :: PlayerState -> Rect
playerRect (PlayerState px) = Rect px playerYPos playerWidth playerHeight

ballRect :: BallState -> Rect
ballRect (BallState (bx,by)) = Rect (bx - ballRadius) (by - ballRadius) (2.0 * ballRadius) (2.0 * ballRadius)

blockRect :: BlockState -> Rect
blockRect (BlockState (bx,by) _) = Rect bx by blockWidth blockHeight
