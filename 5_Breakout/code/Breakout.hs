{-# LANGUAGE Arrows #-}

module Main where

import JavaScript
import Coroutine
import Data.IORef
import Control.Arrow
import Data.List

import Data.VectorSpace

-- input data
data Input = KeyUp Int | KeyDown Int deriving (Eq)

-- Game data
type Vector = (Double, Double)

data PlayerState = PlayerState {xPos :: Double}
data BallState = BallState {ballPos :: Vector}
data BlockState = BlockState {blockPos :: Vector, blockLives :: Int}

data GameState = GameState {player :: PlayerState,
                            ball :: BallState,
                            blocks :: [BlockState]}
                 | StartScreen

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
blockColor1live = "blue"
blockColor2live = "darkblue"

initBallState = BallState ((screenWidth / 2.0), (screenHeight - 50.0))
initBallSpeed = (3.0, -3.0)

initPlayerState = PlayerState ((screenWidth - playerWidth) / 2.0)

initBlockStates = [BlockState (x,y) lives | x <- [20.0, 140.0, 240.0, 340.0, 440.0, 520.0], (y, lives) <- [(60.0,2), (100.0,1), (140.0,2), (180.0,1), (220.0,1), (260.0,1)]]

playerSpeed = 5.0

-- technical values
leftKeyCode = 37
rightKeyCode = 39
restartKeyCode = 32
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
draw StartScreen = do
  ctx <- getContext2d canvasName
  clear ctx
  -- draw the text
  setFillColor ctx "black"
  fillText ctx "Press Space to start" (screenWidth/2.0) (screenHeight/2.0)

draw (GameState playerState ballState blockStates) = do
  ctx <- getContext2d canvasName
  clear ctx
  -- draw player
  setFillColor ctx playerColor
  let pRect = playerRect playerState
  fillRect ctx (x pRect) (y pRect) (width pRect) (height pRect)
  --draw blocks
  mapM_ (drawBlock ctx) $ blockStates
  --draw ball
  setFillColor ctx ballColor
  let (x,y) = ballPos ballState
  fillCircle ctx x y ballRadius

drawBlock :: Context2D -> BlockState -> IO ()
drawBlock ctx bs = do
  setFillColor ctx (if blockLives bs == 1 then blockColor1live else blockColor2live)
  let r = blockRect bs
  fillRect ctx (x r) (y r) (width r) (height r)

-- update function
update :: IORef MainCoroutineType -> IORef (Event Input) -> IO ()
update state input = do
  co <- readIORef state
  i <- readIORef input
  writeIORef input ([] :: [Input])
  let (gs, co') = runC co i
  draw gs
  writeIORef state co'

-- helper functions
gameOver :: GameState -> Bool
gameOver (GameState _ (BallState (_, by)) _) = by > screenHeight
gameOver _ = False

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

-- Game logic
type MainCoroutineType = Coroutine (Event Input) GameState

mainCoroutine :: MainCoroutineType
mainCoroutine = proc inEvents -> do
  rec
    let startEvent = filter (\ke -> ke == KeyUp restartKeyCode) inEvents <$ mainGameCoroutine
        stopEvent  = if gameOver oldState then [mainStartScreenCoroutine] else []
    state <- switch mainStartScreenCoroutine -< (startEvent ++ stopEvent, inEvents)
    oldState <- delay StartScreen -< state
  returnA -< state

mainStartScreenCoroutine :: MainCoroutineType
mainStartScreenCoroutine = arr $ const StartScreen

mainGameCoroutine :: MainCoroutineType
mainGameCoroutine = proc inEvents -> do
  plState <- playerState -< inEvents
  rec
    let (ballBlockColls, blockColls) = ballBlocksCollisions oldBallState oldBlockStates
    let colls = (ballWallCollisions oldBallState) ++ (ballPlayerCollisions plState oldBallState) ++ ballBlockColls
    currBallState   <- ballState            -< colls --long names ...
    currBlockStates <- blockStates          -< blockColls
    oldBallState    <- delay initBallState  -< currBallState
    oldBlockStates  <- delay initBlockStates-< currBlockStates
  returnA -< GameState plState currBallState currBlockStates

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

ballBlocksCollisions :: BallState -> [BlockState] -> (Event BallCollision, [Event BlockCollision])
ballBlocksCollisions ballState blockStates =
  let ballR = ballRect ballState
      foldStep (ballC, blockC) blockState = 
        if rectOverlap ballR (blockRect blockState) then
          (ballRectCollisions ballState (blockRect blockState) ++ ballC, blockC ++ [[BlockCollision]])
        else
          (ballC, blockC ++ [[]])
  in foldl' foldStep ([],[]) blockStates

ballState :: Coroutine (Event BallCollision) BallState
ballState = proc collEvents -> do
  vel <- ballVelocity -< collEvents
  pos <- scan (^+^) (ballPos initBallState) -< vel
  returnA -< BallState pos

ballVelocity :: Coroutine (Event BallCollision) Vector
ballVelocity = scanE bounce initBallSpeed
  where
    bounce :: Vector -> BallCollision -> Vector
    bounce (vx,vy) coll = case coll of
      LeftBounce -> (abs(vx), vy)
      RightBounce -> (-abs(vx), vy)
      UpBounce -> (vx, abs(vy))
      DownBounce -> (vx, -abs(vy))
  
blockState :: BlockState -> Coroutine (Event BlockCollision) (Maybe BlockState)
blockState initState = scanE update (Just initState)	
  where
  update :: Maybe BlockState -> BlockCollision -> Maybe BlockState
  update Nothing   _ = Nothing
  update (Just bs) _ = if (blockLives bs == 1) then Nothing else Just $ bs{blockLives=1}
  
blockStates :: Coroutine ([Event BlockCollision]) ([BlockState])
blockStates = manager $ map blockState initBlockStates

