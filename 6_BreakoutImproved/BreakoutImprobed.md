Hi, welcome to the 6th article of this blog.

In this blog post, the breakout example from the [last Post][last] has been improved, giving it more features:

* The Paddle and Blocks have rounded edges. The ball bounces of them being depending on the surface normal where it hits.
* The blocks are fading out when destroyed.
* The paddle can shoot to destroy blocks. In the beginning one shot is available, shots can be gained by destroying green blocks.
* The game is aware when to player lost or won and displays this information when the game ends.

But that is not all! Instead of using the simple Coroutines we are now using a full blown FRP library called [netwire][netwire]. But more about that later, here is the preview. As alwyas you have to click the canvas to get input focus. If you are not viewing this blog article on blogspot and the application does not work, try the original [aricle page][this].

<script src="https://raw.github.com/RudolfVonKrugstein/jshaskell-blog/master/6_BreakoutImproved/code/compiled/BreakoutImproved.js" type="text/javascript"></script>
<canvas height="400" id="canvas4" style="background-color: white;" width="600" tabindex="1"></canvas>

I had a lot of help over the [haskell beginners mailing list][haskellbeginners]. I will try to add links to the specific topics whenever I am writing something I had help with.

As a final note before I start: Being a haskell beginner, I might not do everything here the best way. I encourage you to comment if you think something could be done better! I of course also encourage you to comment if you have any questions.

# About Netwire

[Netwire][netwire] is a arrowized functional reactive programming (AFRP) library for haskell and the version 4 of the library is recently be released on [hackage][netwire]. Since it uses Arrows, some we the things we did with Coroutines can be done the same way with netwires, but it has tons of new features. [Here][netwireTutorial] is a short introduction to netwire, but I will try to explain all the features when I use them.

Also I will explain some of [netwires][netwire] usage here, this is by no means a complete tutorial to [netwire][netwire]. One obvious reason for this is, that I myself do not (yet?) understand all the features and Ideas of netwire (remember, I am still a haskell beginner doing this for my own education) but maybe some of this will be useful for someone wanting to start with netwire.

But first lets see how collision detection differs from the [last][last] post.

# New Javascript functions

To Draw a rounded rectangle a new function "fillRoundedRect" is defined in [JavaScript.hs][JavaScript.hs].
Also a new type for Colors has been added:

```haskell
data Color = Color {red :: Double, green :: Double, blue :: Double, alpha :: Double}
```

"jsFillColor" now takes this as argument instead of a string.

# Collision detection

See [here][Collision.hs] for the complete code. We want to represent our objects as Circles (the ball, bullets) and rounded rectangles, so that define data structures for this:

```haskell
-- Information about collision
type Vector = (Double, Double) -- thanks to vector-space we can do ^+^ and similar

data Collision   = Collision { normal :: Vector } deriving (Show)

type Radius = Double
data Circle = Circle { circlePos :: Vector, circleRadius :: Radius}
data Rectangle   = Rectangle Vector Vector
data RoundedRect = RoundedRect { rectMin :: Vector, rectMax :: Vector, rectRadius :: Radius}
```

The Collision information now contains the normal of the collision. This is needed to correctly bounce the ball.

For convenience in the usage of the collision functions, we will define type classes for the objects having the shape of a circle or a rounded rectangle:

```haskell
class CircleShaped a where
  circle      :: a -> Maybe Circle
class RoundedRectShaped a where
  roundedRect :: a -> Maybe RoundedRect

instance CircleShaped Circle where
  circle c = Just c

instance RoundedRectShaped RoundedRect where
  roundedRect r = Just r
```

This allows us to apply the collision functions directly to our game objects (when they are instances of the corresponding class) without always explicitly extracting the shape. In other words, instead of writing:

``` haskell
circleRectCollision (ballCircle ball) (blockRect block)
```

we can write

```haskell
circleRectCollision ball block
```

The type classes return maybe types, because some objects might become "shapeless" and should not collide (for example a block that is fading out).

Ok, the first thing we need is a collision between circles

```haskell
import Control.Monad

circleCollision :: (CircleShaped a, CircleShaped b) => a -> b -> Maybe Collision
circleCollision a b = do
  (Circle p1 r1) <- circle a
  (Circle p2 r2) <- circle b
  let centerDiff = p2 ^-^ p1
  guard (centerDiff <.> centerDiff <= (r1 + r2) * (r1 + r2))
  return $ Collision $ normalized centerDiff
```

It returns a "Maybe Collision" because a collision might not take.
Notice the "do" notation. We are in the "Maybe" monad, which causes the function to automatically return Nothing if one of out circle shapes return Nothing (if you do not understand, see [here][maybeMonad]).
So we are getting the vector between the center positions and testing its square against the square of the sums of the radian of the circles.
The guard function (from Control.Monad) causes the monad to return with "Nothing" if the circles are not close enough.
Then we return the normalized vector as the normal. Notice that the normal always points from the first circle to the second.

As a helper function, we test if a point is inside a rectangle:

```haskell
pointInRectangle    :: Vector -> Rectangle -> Bool
pointInRectangle (px,py) (Rectangle (minX,minY) (maxX,maxY))
  | px > maxX = False
  | px < minX = False
  | py > maxY = False
  | py < minY = False
  | otherwise = True
```

That should be clear.

So how do we test a circle against a rounded rectangle?
A rounded rectangle is rectangle where the corners have been replaced by quarter circles. We have do test against these circles or the "inner" rectangle depending on where the colliding circle is, see this picture:

![Areas of rounded rectangle](reoundedRect.svg)

When the center of the colliding circle is in one of the red areas, collision testing is done with the corresponding corner circles. Otherwise collision is done against the "unrounded" rectangle (which is the same as rounded rectangle when we not in one of the red areas). The normal is then determined by the normal of the closest rectangle side. Here is the code:

```haskell
circleRoundedRectCollision :: (CircleShaped a, RoundedRectShaped b) => a -> b -> Maybe Collision
circleRoundedRectCollision c r = do
  circle <- circle c
  rect   <- roundedRect r
  circleRoundedRectCollision' circle rect
  where
    circleRoundedRectCollision' circle@(Circle (cx,cy) cr) (RoundedRect (minX,minY) (maxX,maxY) rr)
      --test the corners
      | cx <= innerMinX && cy <= innerMinY = circleCollision (Circle (innerMinX, innerMinY) rr) circle
      | cx >= innerMaxX && cy <= innerMinY = circleCollision (Circle (innerMaxX, innerMinY) rr) circle
      | cx >= innerMaxX && cy >= innerMaxY = circleCollision (Circle (innerMaxX, innerMaxY) rr) circle
      | cx <= innerMinX && cy <= innerMinY = circleCollision (Circle (innerMinX, innerMaxY) rr) circle
      -- test if collision with rectangle occured
      | not $ pointInRectangle (cx,cy) (Rectangle ((minX-cr), (minY-cr)) ((maxX+cr), (maxY+cr))) = Nothing
      -- collision definitly occured, find correct normal
      | otherwise = Just $ fst $ minimumBy (\(_,a) (_,b) -> compare a b)
                          [
                          (Collision (-1.0,0.0), cx - minX),
                          (Collision (1.0, 0.0), maxX - cx),
                          (Collision (0.0,-1.0), cy - minY),
                          (Collision (0.0, 1.0), maxY - cy)
                          ]
      where
        innerMinX = minX + rr
        innerMinY = minY + rr
        innerMaxX = maxX - rr
        innerMaxY = maxY - rr

```

I am a bit unhappy that I have to define the inner function "circleRoundedRectCollision'", but I do not know how else I could use this nice pattern guards.

# Wire helpers

To handle bullets and blocks we need some way to manage a set of objects where objects can be removed.
For this I got a lot of help [here][haskellbeginnersShrinking] and [here][haskellbeginnersDynamicSet]. The code is [here][WireUtils.hs].
Let's look at the type of a wire:

```haskell
data Wire e m a b
```

The m parameter is the underlying monad. We will set it to Identity and be fine with it.
"a" is the input type. Quoting from [here][netwireTutorial]: From these inputs it

* either produces an output value of type "b" or inhibits with a value of type "e",
* produces a new wire of type Wire e m a b.

## dynamicSet

When a wire inhibits, there are several combinators which allows to switch to other wires (permanently or just for one instance).
We will use this option to remove wires that inhibit. To create new wires we will use a creator function and an additional input.

```haskell
dynamicSet :: (Monad m) => (c -> Wire e m a b) -> [Wire e m a b] ->  Wire e m (a, [c]) [b]
dynamicSet creator ws' = mkGen $ \dt (i,new) -> do
            res <- mapM (\w -> stepWire w dt i) ws'
            let filt (Right a, b) = Just (a,b)
                filt _            = Nothing
                resx = mapMaybe filt res
            return (Right $ (fmap fst resx), dynamicSet creator $ (fmap snd resx) ++ (map creator new))
```

mkGen is passed a function that is turned into a wire. The parameters for this function are the time delta (dt) and the input (i,new) of the wire. We use the do notation because we are in the inner Monad "m" (of which we know nothing but that it is a monad).
After we stepped all wires ("stepWire" steps a wire ,see [netwire tutorial][netwireTutorial]) we filter those that produced (by returning a right value) and rerun there outputs as list. The new wire is again a dynamics set with the ramaing wires and the newly created ones using the creator function.

## dynamicSetMap

To use dynamic set in the breakout game, we assign each wire in the set a unique key (Int) and change the input to a Map that maps from the key to the input values of the inuvidual wires. Since a map lookup may fail, the input of the wires will be Maybes.

To archive this we define a wire that takes a list as inputs and pairs it with a given (infinite) list (which will be our keys):

```haskell
-- queue for the objects in the list given as parameter
-- The Int argument says how many objects should be returned
staticQueue :: (Monad m) => [a] -> Wire e m Int [a]
staticQueue set = unfold give set
  where
  give s n = (take n s, drop n s)

-- Pairs the input list with the given list, which is assumed to be infinite
pairListsWith :: (Monad m) => [p] -> Wire e m [a] [(p,a)]
pairListsWith pairs = proc as -> do
  p <- staticQueue pairs  -< length as
  returnA -< zip p as
```

using these wires we define dynamicSetMap:

```haskell
dynamicSetMap :: (Monad m) => (c -> Wire e m (Maybe a) b) -> [Wire e m (Maybe a) b] -> Wire e m (M.Map Int a, [c]) [(Int,b)]
dynamicSetMap creator ws = dynamicSet creator' ws' . (second $ pairListsWith restKeys)
  where
  wireWithLookupAndKey :: (Monad m) => Int -> Wire e m (Maybe a) b -> Wire e m (M.Map Int a) (Int,b)
  wireWithLookupAndKey i w = (pure i) &&& (w . (arr (M.lookup i)))
  keys           = [0,1..]
  restKeys       = drop (length ws) keys
  ws'            = map (uncurry wireWithLookupAndKey) $ zip keys ws
  creator' (i,c) = wireWithLookupAndKey i (creator c)
```

## shrinking and shrinkingMap

Since blocks can not be created, only destroyed, we define a simplified version of dynamicSet and dynamicSetMap where no new wires can be created.

```haskell
-- same as dynamicSet, only that it can not grow
shrinking :: (Monad m) => [Wire e m a b] -> Wire e m a [b]
shrinking ws = dynamicSet undefined ws <<< arr (\a -> (a,[]))

-- same as dynamicSetMap, only that it can not grow
shrinkingMap :: (Monad m) => [Wire e m (Maybe a) b] -> Wire e m (M.Map Int a) [(Int,b)]
shrinkingMap ws = dynamicSetMap undefined ws <<< arr (\a -> (a,[]))
```

To conclude these helper wires, I am not sure if these are the best choices but they work for now.


[last]:
[this]:
[netwire]:
[netwireTutorial]:
[hasellbeginners]:
[maybeMonad]: http://en.wikipedia.org/wiki/Monad_(functional_programming)#The_Maybe_monad
[haskellbeginnersDynamicSet]:
[haskellbeginnersShrinking]:

# The Game

Finally we are ready to define the game itself! The code is [here][BreakoutImproved.hs].

## Input

In difference to the [last][last] post, we will step the wire on every input event.
An input event will be a keyboard event or an "Update" event which causes the main wire to update all game objects.

```haskell
data InputEvent = KeyUp Int | KeyDown Int | Update
  deriving (Eq)
```

## Data objects

Here are the data objects defining the state of the game:

```haskell
-- state of game objects
data Paddle     = Paddle { xPos    :: Double }
data Gun        = Gun    { ammo    :: Int    }
data Ball       = Ball   { ballPos   :: Vector,
                           ballSpeed :: Vector}
data Block      = Block  { blockType :: BlockType, blockPos :: Vector, blockState :: BlockState}
data BlockState = Alive Int | Dying Double
data BlockType  = NormalBlock | PowerBlock deriving (Eq)

data Bullet     = Bullet { bulletPos :: Vector }
data GameState  = GameState {
                     paddle  :: Paddle,
                     gun     :: Gun,
                     ball    :: Ball,
                     blocks  :: [Block],
                     bullets :: [Bullet]}
                  | StartScreen String
```

The StartScreen constructor of the GameState is to show a message when the game is not running (in the beginning, when the player won or lost).
We gave the ball the ballSpeed property (which is not necessary for viewing the game state) because it will be needed outside the balls own wires later. You will see.

## constants

A lot of constants follow which define the properties of the game

```haskell
-- constants
screenWidth      = 600.0
screenHeight     = 400.0

paddleColor      = Color 0.0 0.0 0.0 1.0
paddleYPos       = screenHeight - paddleHeight
paddleHeight     = 15.0
paddleWidth      = 50.0
paddleRadius     = 7.0
paddleSpeed      = 7.0
initPaddleXPos   = (screenWidth - paddleWidth) / 2.0
initPaddle       = Paddle initPaddleXPos

initGun          = Gun 1

ballColor        = Color 1.0 0.0 0.0 1.0
ballRadius       = 5.0
initBallSpeed    = (3.0, -3.0)
initBallPos      = (screenWidth / 2.0, screenHeight - 50.0)
initBall         = Ball initBallPos initBallSpeed

blockWidth       = 60.0
blockHeight      = 20.0
blockRadius      = 5.0
normalBlockColor = [Color 0.0 0.0 1.0 1.0, Color 0.0 0.0 0.5 1.0]
powerBlockColor  = [Color 0.0 0.5 0.0 1.0]
initBlocks       = [Block t (x,y) (Alive l) | x <- [20.0, 140.0, 240.0, 340.0, 440.0, 520.0], (y,t,l) <- [(60.0, PowerBlock, 1), (100.0, NormalBlock,2), (140.0,NormalBlock,1), (180.0,NormalBlock,2), (260.0,NormalBlock,2)]]

bulletRadius     = 3.0
bulletSpeed      = (0.0, -10.0)
bulletColor      = Color 0.0 0.5 0.0 1.0

-- technical constants
leftKeyCode  = 37
rightKeyCode = 39
startKeyCode = 13
fireKeyCode  = 32
canvasName   = "canvas4"
```

The canvas name is the same name as defined in the outer html where the canvas is located.
