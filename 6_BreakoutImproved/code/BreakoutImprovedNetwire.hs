{-# LANGUAGE Arrows #-}

module Main where

import JavaScript
import Data.IORef
import Control.Wire
import Prelude hiding ((.), id)
import Data.VectorSpace
import qualified Control.Monad as CM
import qualified Data.Function as F
import Data.Maybe

-- Game Types
-- Input events
data InputEvent = KeyUp Int | KeyDown Int | Update
  deriving (Eq)

-- GameData
type Vector = (Double, Double) -- thanks to vector-space we can do ^+^ and similar

-- state of game objects
data Paddle     = Paddle { xPos    :: Double }
data Ball       = Ball   { ballPos :: Vector }
data Block      = Block  { blockType :: BlockType, blockPos :: Vector, blockLives :: Int}
                | DyingBlock {blockFade :: Double}
data BlockType  = NormalBlock | PowerBlock deriving (Eq)
data Bullet     = Bullet { bulletPos :: Vector }
data GameState  = GameState {
                     paddle  :: Paddle,
                     ball    :: Ball,
                     blocks  :: [Block],
                     bullets :: [Bullet]}
                  | StartScreen

-- Information about collision
data Collision   = Collision { normal :: Vector }

type Radius = Double
data Circle = Circle { circlePos :: Vector, circleRadius :: Radius}
data Rectangle   = Rectangle Vector Vector
data RoundedRect = RoundedRect { rectMin :: Vector, rectMax :: Vector, rectRadius :: Radius}

-- constants
screenWidth      = 600.0
screenHeight     = 400.0

paddleColor      = "black"
paddleYPos       = screenHeight - paddleHeight
paddleHeight     = 15.0
paddleWidth      = 50.0
paddleRadius     = 5.0
paddleSpeed      = 7.0
initPaddleXPos   = (screenWidth - paddleWidth) / 2.0
initPaddle       = Paddle initPaddleXPos

ballColor        = "red"
ballRadius       = 5.0
initBallSpeed    = (3.0, -3.0)
initBallPos      = (screenWidth / 2.0, screenHeight - 50.0)
initBall         = Ball initBallPos

blockWidth       = 60.0
blockHeight      = 20.0
blockRadius      = 7.0
normalBlockColor = ["blue", "darkblue"]
powerBlockColor  = ["green"]
initBlocks       = [Block t (x,y) l | x <- [20.0, 140.0, 240.0, 340.0, 440.0, 520.0], (y,t,l) <- [(60.0, PowerBlock, 1), (100.0, NormalBlock,2), (140.0,NormalBlock,1), (180.0,NormalBlock,2), (260.0,NormalBlock,2)]]

bulletRadius     = 3.0
bulletColor      = "darkgreen"

-- technical constants
leftKeyCode  = 37
rightKeyCode = 39
startKeyCode = 13
fireKeyCode  = 13
canvasName   = "canvas5"

-- type of the main wire
type MainWireType = WireP InputEvent GameState

-- startup
main = setOnLoad initilize

initilize :: IO ()
initilize = do
  wire <- newIORef mainWire
  setOnKeyDown canvasName (onKeyDown wire)
  setOnKeyUp   canvasName (onKeyUp   wire)
  setInterval 30.0 (update wire)

-- reactions to input events
onKeyDown :: IORef MainWireType -> Int -> IO ()
onKeyDown wire code = do
  w <- readIORef wire
  let (_, w') = stepWireP w 0.0 (KeyDown code)
  writeIORef wire w'
 
onKeyUp :: IORef MainWireType -> Int -> IO ()
onKeyUp wire code = do
  w <- readIORef wire
  let (_, w') = stepWireP w 0.0 (KeyUp code)
  writeIORef wire w'

-- update
update :: IORef MainWireType -> IO ()
update wire = do
  w <- readIORef wire
  let (res, w') = stepWireP w 1.0 Update
  case res of
    Left err -> alert "Error!"
    Right gs -> draw gs
  writeIORef wire w'

-- Collision utils
class CircleShaped a where
  circle      :: a -> Maybe Circle
class RoundedRectShaped a where
  roundedRect :: a -> Maybe RoundedRect

circleCollision :: (CircleShaped a, CircleShaped b) => a -> b -> Maybe Collision
circleCollision a b = do
  (Circle p1 r1) <- circle a
  (Circle p2 r2) <- circle b
  let centerDiff = p2 ^-^ p1
  CM.guard (centerDiff <.> centerDiff > r1 * r2)
  return $ Collision $ normalized centerDiff

pointInRectangle    :: Vector -> Rectangle -> Bool
pointInRectangle (px,py) (Rectangle (minX,minY) (maxX,maxY))
  | px > maxX = False
  | px < minX = False
  | py > maxY = False
  | py < minY = False
  | otherwise = True

circleRectCollision :: (CircleShaped a, RoundedRectShaped b) => a -> b -> Maybe Collision
circleRectCollision c r = do
  circle <- circle c
  rect   <- roundedRect r
  circleRectCollision' circle rect
  where
    circleRectCollision' circle@(Circle (cx,cy) cr) (RoundedRect (minX,minY) (maxX,maxY) rr)
      -- test the corners
      | cx <= innerMinX && cy <= innerMinY = circleCollision (Circle (innerMinX, innerMinY) rr) circle
      | cx >= innerMaxX && cy <= innerMinY = circleCollision (Circle (innerMaxX, innerMinY) rr) circle
      | cx >= innerMaxX && cy >= innerMaxY = circleCollision (Circle (innerMaxX, innerMaxY) rr) circle
      | cx <= innerMinX && cy <= innerMinY = circleCollision (Circle (innerMinX, innerMaxY) rr) circle
      -- test if collision with rectangle occured
      | not $ pointInRectangle (cx,cy) (Rectangle ((minX-cr), (minY-cr)) ((maxX+cr), (maxY+cr))) = Nothing
      -- collision definitly occured, find correct normal
      | cx <= innerMinX = Just $ Collision (-1.0,0.0)
      | cx >= innerMaxX = Just $ Collision (1.0, 0.0)
      | cy <= innerMinY = Just $ Collision (0.0,-1.0)
      | cy >= innerMaxY = Just $ Collision (0.0, 1.0)
      | otherwise       = Just $ Collision (0.0, 0.0)
      where
        innerMinX = minX + rr
        innerMinY = minY + rr
        innerMaxX = maxX - rr
        innerMaxY = maxY - rr

-- Circles and rectangles of game objects
instance CircleShaped Circle where
  circle c = Just c

instance CircleShaped Ball where
  circle (Ball p) = Just $ Circle p ballRadius

instance CircleShaped Bullet where
  circle (Bullet p) = Just $ Circle p bulletRadius

instance RoundedRectShaped Paddle where
  roundedRect (Paddle x) = Just $ RoundedRect (x,paddleYPos) (x+paddleWidth,paddleYPos+paddleHeight) paddleRadius

instance RoundedRectShaped Block where
  roundedRect (DyingBlock _)    = Nothing
  roundedRect (Block _ (x,y) _) = Just $ RoundedRect (x,y) (x+blockWidth,y+blockHeight) blockRadius

-- drawing function, draw a game state
draw :: GameState -> IO ()
draw StartScreen = do
  ctx <- getContext2d canvasName
  clear ctx
  setFillColor ctx "black"
  fillText ctx "Press enter to start (click the canvas for input focus)" (screenWidth / 2.0 - 100.0) (screenHeight /2.0)

draw (GameState paddle ball blocks bullets) = do
  ctx <- getContext2d canvasName
  clear ctx
  setFillColor ctx paddleColor
  fillRoundedRect ctx (xPos paddle) paddleYPos paddleWidth paddleHeight paddleRadius
  mapM_ (drawBlock ctx) $ blocks
  setFillColor ctx ballColor
  let Ball (x,y) = ball
  fillCircle ctx x y ballRadius

drawBlock :: Context2D -> Block -> IO ()
drawBlock ctx (Block t (x,y) lives) = do
  setFillColor ctx $ (if t == PowerBlock then powerBlockColor else normalBlockColor) !! (lives -1)
  fillRoundedRect ctx x y blockWidth blockHeight blockRadius
  

-- collsion detection
calcBallBlockColls :: Ball -> [Block] -> [Maybe Collision]
calcBallBlockColls b = map (circleRectCollision b)

calcBallWallColls :: Ball -> [Collision]
calcBallWallColls (Ball (bx,by)) = map snd $ filter (fst) $ [
  (bx <= 0          , Collision (1.0 , 0.0)),
  (bx >= screenWidth, Collision (-1.0, 0.0)),
  (by <= 0          , Collision (0.0 , 1.0))
  ]

calcBallPaddleColls :: Ball -> Paddle -> [Collision]
calcBallPaddleColls b p = 
  catMaybes [circleRectCollision b p]

-- Wires
-- key wires
keyDown :: Int -> EventP InputEvent
keyDown code = when (==KeyDown code)

keyUp :: Int -> EventP InputEvent
keyUp code = when (==KeyUp code)

-- main wire
mainWire :: MainWireType
mainWire = F.fix (\start -> 
             pure StartScreen . notE (keyDown startKeyCode) -->
             mainGameWire -->
             start)

mainGameWire :: MainWireType
mainGameWire = proc input -> do

  --paddle   <- paddleWire -< input

  if input == Update then
    update -< ()
    else
    empty -< ()

  where
    update :: WireP () GameState
    update = proc _ -> do
      value <- integral_ 0.0 -< 1.0
        --oldValue <- delay 1.0 -< value
        --let ballBlockColls  = calcBallBlockColls  oldBall oldBlocks :: [Maybe Collision]
        --    ballWallColls   = calcBallWallColls   oldBall
        --    ballPaddleColls = calcBallPaddleColls oldBall paddle
        --ball      <- ballWire         -< (catMaybes ballBlockColls) ++ ballWallColls ++ ballPaddleColls
        --oldBall   <- delay initBall   -< ball
        --blocks    <- blocksWire       -< ballBlockColls
        --oldBlocks <- delay initBlocks -< blocks
      returnA -< GameState initPaddle (Ball (value,0.0)) [] []

-- induvidial game objects
paddleWire :: WireP InputEvent Paddle
paddleWire = Paddle <$> (integral_ initPaddleXPos <<< paddleSpeedWire)

paddleSpeedWire :: WireP InputEvent Double 
paddleSpeedWire = (valueFromKeyDown leftKeyCode 0.0 (-paddleSpeed))
                  +
                  (valueFromKeyDown rightKeyCode 0.0 paddleSpeed)
  where
  valueFromKeyDown :: Int -> a -> a -> WireP InputEvent a
  valueFromKeyDown code upValue downValue = F.fix (\start ->
                                                   pure upValue   . notE (keyDown code) -->
                                                   pure downValue . notE (keyUp code) -->
                                                   start)

ballWire :: WireP [Collision] Ball
ballWire = pure initBall

blocksWire :: WireP [Maybe Collision] [Block]
blocksWire = pure initBlocks
