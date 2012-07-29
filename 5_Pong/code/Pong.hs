{-# LANGUAGE Arrows #-}

module Main where

import JavaScript hiding (Event)
import Coroutine
import Data.IORef
import Control.Arrow

-- technical data
data Input = KeyUp Double | KeyDown Double

-- entry point
main = setOnLoad initilize

initilize = do
  state <- newIORef mainCoroutine
  input <- newIORef ([] :: [Input])
  setOnKeyDown (onKeyDown input)
  setOnKeyUp   (onKeyUp input)
  setInterval 20.0 (update state input)

-- input
onKeyDown :: IORef [Input] -> Double-> IO ()
onKeyDown input keyCode = do
  i <- readIORef input
  let i' = input ++ [KeyDown keyCode]
  writeIORef input i'

onKeyUp :: IORef [Input] -> Double-> IO ()
onKeyUp input keyCode = do
  i <- readIORef input
  let i' = input ++ [KeyUp keyCode]
  writeIORef input i'
  

-- draw a gamestate
draw :: GameState -> IO ()
draw gs = do
  ctx <- getContext2d
  clear ctx
  -- draw player
  setFillColor ctx playerColor
  fillRect ctx (xPos . player gs) playerYPos playerWidth playerHeight
  --draw ball
  setFillColor ctx ballColor
  let (x,y) = pos . ball gs
  fillCircle ctx x y ballRadius

-- update function
update :: IORef MainCoroutineType -> IORef (Event Input) -> IO ()
update state input = do
  co <- readIORef state
  input <- readIORef input
  writeIORef input []
  let (co', gs) = runC co input
  draw gs
  writeIORef co'

-- Game data
type Vector = (Double, Double)

data PlayerState = PlayerState {xPos :: Double}
data BallState = BallState {pos :: Vector}

data GameState = GameState {player :: PlayerState,
                            ball :: BallState}
-- game values
screenWidth = 600.0
screenHeight = 400.0
playerColor = "black"
ballColor = "red"
playerYPos = screenWidth - playerHeight
playerHeight = 15.0
playerWidth = 40.0
ballRadius = 5.0

initBallState = BallState (screenWidth / 2.0) (screenHeight - 50.0)
initBallSpeed = (1.0, -1.0)

initPlayerState = PlayerState ((screenWidth - playerWidth) / 2.0)

playerSpeed = 1.0 --the speed with which the player moves

-- Game logic
type MainCoroutineType = Coroutine (Event Input) GameState

mainCoroutine :: MainCoroutineType
mainCoroutine = proc inEvents -> do
  plState <- playerState -< inEvents
  rec
    blWlColls <- ballWallCollisions -< oldBlState
    blPlColls <- ballPlayerCollisions -< (plState, oldBlState)
    blState <- ballState -< (blPlColls ++ blWlColls)
    oldBlState <- delay initBallState -< blState
  returnA -< GameState plState blState

playerState :: Coroutine (Event Input) PlayerState
playerState = proc inEvents -> do
  vel <- playerVelocity -< inEvents
  xPos <- integrate (xPos initPlayerXPos)  -< vel
  returnA -< PlayerState xPos

playerVelocity :: Coroutine (Event Input) Double
playerVelocity = proc inEvents -> do
  leftDown <- keyDown leftKeyCode -< inEvents
  rightDown <- keyDown rightKeyCode -< inEvents
  returnA -< if leftDown then -playerSpeed else (if rightDown then playerSpeed else 0.0)

ballWallCollisions :: BallState -> (Event BallCollisions)
ballWallCollisions (bx,by) =
  nap snd . filter fst . [(by < ballRadius,                LeftBounce),
                          (by > screenWidth - ballRadius,  RightBounce),
                          (bx > screenHeight - ballRadius, UpBounce)]

ballPlayerCollisions :: PlayerState -> BallState -> (Event BallCollision)
ballPlayerCollisions playerState ballState =
  if rectOverlap (playerRect playerState) (ballRect blallState)
  then ballRectCollisions ballState (playerRect playerState)
  else []

ballState :: Coroutine (Event BallCollisions) BallState
ballState = proc collEvents -> do
  

