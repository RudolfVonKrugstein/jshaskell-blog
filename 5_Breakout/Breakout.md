Hi, welcome to the 5th article of this blog.

I am very exited! In the [last Post][last] we wrote a Pong like ... well ... let's call it javascript application. In this post I have expanded it by the following properties:

* The game does not start until you hit press
* There are blocks that can be hit by the ball and disappear.
* There are even blocks with 2 lives (dark blue) that turn into normal blocks on the first hit.
* The game stops when the ball leaves the canvas downward.

This means, there is a goal and there is a game over situation. So in this point one could actually call it a game. And it even has a start screen ... did I mention that I am exitied?

But, now, as always, here is a preview. As always you have to click the canvas to get input focus. If you are not viewing this blog article on blogspot and the application does not work, try the original [article page][this].

** Note: ** This currently only works if you are viewing this article only (not in the flow of the complete blog). I am working on the problem ...

<script src="https://raw.github.com/RudolfVonKrugstein/jshaskell-blog/master/5_Breakout/code/compiled/Breakout.js" type="text/javascript"></script>
<canvas height="400" id="canvas3" style="background-color: white;" width="600" tabindex="1"></canvas>

But let me tell you how I did it :).

By the way, this post assumes that you have read the [last Post][last].

# More Coroutine helpers

There are two aspects of this game (yes, game!)

* The blocks are a dynamic set of objects, that disappear as the game progresses
* There are different "game states" (the start screen and the actual game)

So this has been added to [Coroutine.hs][Coroutine.hs]

```haskell
-- manages a set of coroutines which are deletet when returning Nothing
manager :: [Coroutine a (Maybe b)] -> Coroutine [a] [b]
manager cos = Coroutine $ \is ->
  let res  = map (\(co, i) -> runC co i) $ zip cos is
      res' = filter (isJust . fst) res
      (result, cos') = unzip res'
  in (catMaybes result, manager cos')

-- switcher, starts with a specific coroutine and switches whenever a new coroutine is send via an event
switch :: Coroutine a b -> Coroutine (Event (Coroutine a b), a) b
switch init = Coroutine $ \(e,i) ->
  let init' = last $ init : e --the last coroutine sent through
      (o, init'') = runC init' i
  in  (o, switch init'')

-- replace the contents of an event
(<$) :: Event a -> b -> Event b      
(<$) events content = map (\_ -> content) events
```

## manager
The manger is for managing the blocks. Every blocks state is described as a coroutine, and in the beginning there is a set of blocks in the game (first parameter to manager).

Every block Coroutine returns "Nothing" when the block is destroyed, the manager than removes the block from the set.

Note that at present there is no way of inserting new blocks in the manager, it is not needed in this game.

## switch
Switch allows us to switch between different game states, which all are described by coroutines with the same type.

Initially switch behaves as the init Coroutine (its first parameter) with an extra parameter holding events with other Coroutines. Whenever one of these events occurs, switch switches to the coroutine carried in the event.

## <$

This is a operator, when applied to an event replaces the contents of the event with the second parameter. We need this to replace the content of the KeyDown event with the main Coroutine when the start key is pressed. You will see!

# From Pong to Breakout 

All very exiting, by the real excitement start now. The main source file [Breakout.hs][Breakout.hs] is based on the [last posts][last] [Pong.hs][Pong.hs]. Here I will go over the differences.

## Definitions

The game state needs to reflect the blocks and the start screen. It has changed to:

```haskell
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
```

The BlockState has been added, which contains the block position and the number of lives (1 or 2) of the block.
The GameState has been expanded by a list of BlockStates AND can be just the start screen (when the game has not started).

BlockCollision is a type for creating Events where the block collides with ball. A type synonym to () would also work, but I choose this more verbose way.

```haskell
blockWidth = 60.0
blockHeight = 20.0
blockColor1live = "blue"
blockColor2live = "darkblue"

initBlockStates = [BlockState (x,y) lives | x <- [20.0, 140.0, 240.0, 340.0, 440.0, 520.0], (y, lives) <- [(60.0,2), (100.0,1), (140.0,2), (180.0,1), (220.0,1), (260.0,1)]]

restartKeyCode = 32
canvasName = "canvas3"
```

The color of the blocks depend if they have 1 or 2 lives. initBlockStates describes the blocks as the game starts. They are evenly spaced 6 in x and 6 in y directions. 2 of the y rows have 2 lives, the rest 1.

The restartKeyCode is the key code of the space bar and the canvasName is the name of the canvas in the html code of [this][this] blog.

## Drawing

```haskell
draw :: GameState -> IO ()
draw StartScreen = do
  ctx <- getContext2d canvasName
  clear ctx
  -- draw the text
  setFillColor ctx "black"
  fillText ctx "Press Space to start --- (click the canvas for input focus)" (screenWidth/2.0 - 100.0) (screenHeight/2.0)

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
```

draw now pattern matches its argument, to test if it is the start screen. If so, a short message telling the player to press space is displayed (see [fillText](http://www.w3schools.com/html/html5_canvas.asp) in some javascript documentation).

## helpers

```haskell
gameOver :: GameState -> Bool
gameOver (GameState _ (BallState (_, by)) _) = by > screenHeight
gameOver _ = False

blockRect :: BlockState -> Rect
blockRect (BlockState (bx,by) _) = Rect bx by blockWidth blockHeight
```

gameOver is a little helper function to test if the ball has left the canvas. It returns False on the start screen.

blockRect returns the rectangle occupied a block.

## Main coroutine

```haskell
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
```

The original main coroutine has been renamed to mainGameCoroutine. There is a new "main coroutine" mainStartScreenCoroutine which is used while in the start screen. The new mainCoroutine switches between these two coroutine when the player pressed space, or the game is over.

Remember, the ... operator replaces the contents of an event with its second parameter (here the mainGameCoroutine) and switch receives events containing coroutines to which it switches.

mainGameCoroutine has been extended by the blocks. ballBlocksCollisions, as we will see later, returns a tuple with the ballCollisions events due to the blocks and a list of BlockCollision events.
This list has the same length as the list of blocks (in oldBlockStates). The n-th element of this list are the collisions of the n-th block.

The block collisions are than passed to the blockState arrow while the ballCollisions are added to the collisions passed to ballState.

I dislike the long names like "currBallState" here. I would have called it ballState, but there is already an arrow with the same name. I wonder if there is a less clumsy way of doing this ...

## Ball-Block collisions

```haskell
ballBlocksCollisions :: BallState -> [BlockState] -> (Event BallCollision, [Event BlockCollision])
ballBlocksCollisions ballState blockStates =
  let ballR = ballRect ballState
      foldStep (ballC, blockC) blockState =
        if rectOverlap ballR (blockRect blockState) then
          (ballRectCollisions ballState (blockRect blockState) ++ ballC, blockC ++ [[BlockCollision]])
        else
          (ballC, blockC ++ [[]])
  in foldl' foldStep ([],[]) blockStates
```

In my eyes, this is the most complicated function. It takes the ball state and the block states (as a list) and produces ball collisions events, and a list of block collision events, which has the same length as the input block state list.

The foldStep function takes the next block, tests it for collision and updates the list of ball and block collisions. Here the ball collision events are only expanded when a collision happens. The wlist of block collision events is always expanded. By an empty event (empty list) when no collision happens, and by a BlockCollision event in case of collision. This is because the position in this event reflects the block that will receive it.

## Updating the block state

```haskell
blockState :: BlockState -> Coroutine (Event BlockCollision) (Maybe BlockState)
blockState initState = scanE update (Just initState)
  where
  update :: Maybe BlockState -> BlockCollision -> Maybe BlockState
  update Nothing   _ = Nothing
  update (Just bs) _ = if (blockLives bs == 1) then Nothing else Just $ bs{blockLives=1}

blockStates :: Coroutine ([Event BlockCollision]) ([BlockState])
blockStates = manager $ map blockState initBlockStates
```

Every block has its own coroutine, which receives block collision events. In case of such an event, the number of lives is reduced or the block is removed (if there are no lives left).
The coroutines return a Maybe data type, because they are inserted into the manager. Nothing is returned if the block should be deleted.

blockStats uses the manager to manage all "living" blocks.

# Compiling

The compilation is the same as for Pong int the [last post][last].

## Haste

For haste make sure the [newest version](https://github.com/valderman/haste-compiler) is installed. Because we use vector-space we need to install it for haste.

vector space is needed, see the [last post][last].

Now put [Breakout.hs][Breakout.hs], [Coroutine.hs][Coroutine.hs], the haste version of [JavaScript.hs][JavaScriptHaste.hs] and the javascript helper functions [helpers.js][helpersHaste.js] in a directory and compile with

```bash
hastec Breakout.hs --start=asap --with-js=helpers.js
```

You should receive a file "Breakout.js" which can be included in a html file, like this one: [haste html][indexHaste]

## UHC

With UHC it is a little bit more work. UHC does not support arrow syntax, so we must translate the haskell file with arrowp:

```bash
cabal install arrowp
arrowp Breakout.hs > BreakoutNA.hs
```

I choose the name PongNA.hs for "Pong no arrows". For some reason I also can not get vector space to compile with UHC. Luckily we have not used much of vector space, only the ^+^ operator.
So edit PongNA.hs and replace the line

```haskell
import Data.VectorSpace
```

with

```haskell
(^+^) :: Num a => (a,a) -> (a,a) -> (a,a)
(^+^) (a1,a2) (b1,b2) = (a1+b1, a2+b2)
```

Now copy [Coroutine.hs][Coroutine.hs] and [JavaScript.hs][JavaScriptUHC.hs] (the UHC version) into the directory and compile with

```bash
uhc -tjs BreakoutNA.hs -iuhc 
```

The canvas needs to be added to the generated html file, so add

```html
<canvas height="400" id="canvas3" style="background-color: white;" width="600" tabindex="1"></canvas>
```

Since we do not need any additional javascript functions, the generated html page should work!

# Conclusion

Well that is it. At places I find it a bit clumpsy and I wonder if another FRP library like [Reactive Banana][ReactiveBanana] or [elerea][elerea] would help. I will look into these!
