In the [last Post](http://jshaskell.blogspot.de/2012/07/first-interactive-application.html) we wrote the first interactive application where a blog on the bottom of the canvas could be moved via keyboard input.

In this next step we will add ball (a moving circle) that can bounce of the paddle (the moving block) and the walls.

Here is a preview (again, click on the canvas to get input focus).

<script src="" type="text/javascript"></script>
<canvas height="400" id="canvas2" style="background-color: white;" width="600" tabindex="1"></canvas>

But first we will need some perquisites. I will utilize Functional Reactive Programming (FRP) using the functions defined here: [Purely Functional, Declarative Game Logic Using Reactive Programming](https://github.com/leonidas/codeblog/blob/master/2012/2012-01-17-declarative-game-logic-afrp.md).
So make sure you read and understand that blog entry. The resulting code can be found here: [Coroutine.hs]()

So, let's get started!

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
