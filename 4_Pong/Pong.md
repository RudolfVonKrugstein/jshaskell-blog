In the [last Post](http://jshaskell.blogspot.de/2012/07/first-interactive-application.html) we wrote the first interactive application where a blog on the bottom of the canvas could be moved via keyboard input.

In this next step we will add ball (a moving circle) that can bounce of the paddle (the moving block) and the walls.

Here is a preview (again, click on the canvas to get input focus).

<script src="" type="text/javascript"></script>
<canvas height="400" id="canvas2" style="background-color: white;" width="600" tabindex="1"></canvas>

But first we will need some perquisites. I will utilize Functional Reactive Programming (FRP) using the functions defined here: [Purely Functional, Declarative Game Logic Using Reactive Programming](https://github.com/leonidas/codeblog/blob/master/2012/2012-01-17-declarative-game-logic-afrp.md).
So make sure you read and understand that blog entry. The resulting code can be found here: [Coroutine.hs]()

So, let us get started!

# Imports and definitions

I follow the source file [Pong.hs]() and therefor start with the imports and some definitions used later in the game.

```haskell
{-# LANGUAGE Arrows #-}

module Main where

import JavaScript
import Coroutine
import Data.IORef
import Control.Arrow

import Data.VectorSpace

-- input data
data Input = KeyUp Int | KeyDown Int deriving (Eq)

-- Game data
type Vector = (Double, Double)

data PlayerState = PlayerState {xPos :: Double}
data BallState = BallState {pos :: Vector2D}

data GameState = GameState {player :: PlayerState,
                            ball :: BallState}

data BallCollision = LeftBounce | RightBounce | UpBounce | DownBounce
data Rect = Rect { x::Double, y::Double, width ::Double, height::Double}
```

We will use Arrow Syntax and tell the compiler that we do. Actually UHC does not support Arrow Syntax (yet?), but more about that later.

We import Data.VectorSpace allowing us to use some basic vector operation with tuples of Doubles. Here we only need addition, but I if we need more VectorSpace is handy.

The input data will be a series of Keyboard up and down events with corresponding key codes. BallCollision describes a collision of the ball with the wall or the paddle in a certain direction.

The rest is types we need in the game and should be self explaining.

Next we will declare some values defining subtleties of the game.

```haskell
-- game values
screenWidth = 600.0
screenHeight = 400.0
playerColor = "black"
ballColor = "red"
playerYPos = screenHeight - playerHeight
playerHeight = 15.0
playerWidth = 40.0
ballRadius = 5.0

initBallState = BallState ((screenWidth / 2.0), (screenHeight - 50.0))
initBallSpeed = (3.0, -3.0)

initPlayerState = PlayerState ((screenWidth - playerWidth) / 2.0)

playerSpeed = 5.0

-- technical values
leftKeyCode = 37
rightKeyCode = 39
canvasName = "canvas2"

```

Again, these should be relatively self explaining. Keycode 37 and 39 correspond to the arrow keys. canvas2 is the name of the canvas defined in the html code of this blog.

# Entry point on callbacks

In difference to the last callback we will not use a javascript function to save and store global objects. Instead the objects will be stored in IORefs which are passed to the callbacks.

```haskell
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
```

So main sets the initilize function to be called then the window is loaded. initilize creates 2 IORefs, one for the main coroutine (which will be defined later) and one for the input stream, which is a list of input events. onKeyDown and onKeyUp are called when a key is pressed or released and expand the input stream.

update is set to be called every 20 milliseconds with the state and input IORefs passed to it.

#Updating and drawing the game sate
```haskell
-- draw a gamestate
draw :: GameState -> IO ()
draw gs = do
  ctx <- getContext2d canvasName
  clear ctx
  -- draw player
  setFillColor ctx playerColor
  let pRect = playerRect . player $ gs
  fillRect ctx (x pRect) (y pRect) (width pRect) (height pRect)
  --draw ball
  setFillColor ctx ballColor
  let (x,y) = pos . ball $ gs
  fillCircle ctx x y ballRadius

-- update function
update :: IORef MainCoroutineType -> IORef (Event Input) -> IO ()
update state input = do
  co <- readIORef state
  i <- readIORef input
  writeIORef input ([] :: [Input])
  let (gs, co') = runC co i
  draw gs
  writeIORef state co'
```

The draw function should be self explaining. If not, read my last blog articles. Some javascript functions have been added, but they all follow the same principle as in the last blog article.

The update function reads the current main coroutine and input stream. The coroutine is updated and the new game state is obtained by calling the coroutine with the current input stream. Finally the game state is drawn and the new coroutine is saved.

# Some helper functions

Before the main game logic starts a few helper functions are defined.

```haskell
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
```

keyDown takes a keycode and outputes a coroutine indicating at all times if the given key is down given the input stream (The Event type comes from [Coroutine.hs]()).

rectOverlap tests two rectangles if they overlap (used for collision detection). playerRect and ballRect return the rectangle occupied by the paddle and ball respectively.

# The main Coroutine

```haskell
-- Game logic
type MainCoroutineType = Coroutine (Event Input) GameState

mainCoroutine :: MainCoroutineType
mainCoroutine = proc inEvents -> do
  plState <- playerState -< inEvents
  rec
    let colls = (ballWallCollisions oldBlState) ++ (ballPlayerCollisions plState oldBlState)
    blState <- ballState -< colls
    oldBlState <- delay initBallState -< blState
  returnA -< GameState plState blState
```

# The Player

```haskell
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
````

# The Ball state

## Collisions
```haskell
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
```

## Updating the ball state

```haskell
ballState :: Coroutine (Event BallCollision) BallState
ballState = proc collEvents -> do
  vel <- ballVelocity -< collEvents
  pos <- scan (^+^) (pos initBallState) -< vel
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
```

# Compiling with haste

# Compiling with UHC

# Conclusion

I have little experience with FRP (this blog article is my first attempt to write a FRP application). I would have liked to use [Reactive Banana]() for this, but at present I am unable to compile Reactive Banana with UHC or haste.

According to [this]() Reactive Banana has been compiled with UHC, but ...
haste failed to compile Reactive Banana because of missing PrimOps. According to the maintainer of haste, that is a solvable problem and will be fixed in the future.
