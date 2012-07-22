module Main where

-- draw a gamestate
draw :: GameState -> IO ()
draw gs = do
  ctx <- getContext2d
  clear ctx
  -- draw player
  setFillColor ctx playerColor
  fillRect ctx (xPos . playerState gs) playerYPos playerWidth playerHeight
  --draw ball
  setFillColor ctx ballColor
  let (x,y) = pos . ballState gs
  fillCircle ctx x y ballRadius

-- update function
update :: IO ()
update = do
  co <- loadGlobalObject "gameCoroutine" :: MainCoroutineType
  input <- loadInputEvents
  let (co', gs) = runC co input
  draw gs
  saveGlobalState "gameCoroutine"

-- Game data
type Vector = (Double, Double)

data PlayerState = PlayerState {xPos :: Double}
data BallState = BallState {pos :: Vector}

data GameState = GameState {playerState :: PlayerState,
                            ballState :: BallState}
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
    olBlState <- delay initBallState -< blState
  returnA -< GameState plState blState

playerState :: Coroutine (Event Input) PlayerState
playerState  proc inEvents -> do
  vel <- playerVelocity -< inEvents
  xPos <- integrate (xPos initPlayerXPos)  -< vel
  returnA -< PlayerState xPos

playerVelocity :: Coroutine (Event Input) Double
playerVelocity = proc inEvents -> do
  leftDown <- keyDown leftKeyCode -< inEvents
  rightDown <- keyDown rightKeyCode -< inEvents
  return -< if leftDown then -playerSpeed else (if rightDown playerSpeed else 0.0)

ballWallCollisions :: Coroutine BallState (Event BallCollisions)
ballWallCollisions = proc (BallState (bx,by)) -> do
  leftColl  <- watch (< ballRadius) >>> constE LeftBounce                -< by
  rightColl <- watch (> screenWidth - ballRadius) >>> constE RightBounce -< by
  upColl   <- watch (> ballRadius) >>> constE UpBounce                   -< bx
  returnA -< leftColl ++ rightColl ++ upColl
  

ballPlayerCollisions :: Coroutine (PlayerState, BallState) (Event BallCollision)
ballPlayerCollisions = proc (plState, blState) -> do
  returnA -< if rectOverlap (playerRect plState) (ballRect blState) then

