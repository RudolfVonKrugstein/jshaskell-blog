{-# LANGUAGE DoRec #-}
module Main where

import JavaScript
import Data.IORef
import FRP.Elerea.Simple
import Prelude hiding ((.), id)
import Data.VectorSpace
import qualified Control.Monad as CM
import Control.Applicative
import Data.Maybe
import Data.List

-- Game Types
-- Input events
data InputEvent = KeyUp Int | KeyDown Int
  deriving (Eq)

-- GameData
type Vector = (Double, Double) -- thanks to vector-space we can do ^+^ and similar

-- state of game objects
data Paddle     = Paddle { xPos    :: Double }
data Ball       = Ball   { ballPos :: Vector }
data Block      = Block  { blockType :: BlockType, blockPos :: Vector, blockLives :: Int}
                | DyingBlock {blockFade :: Double}
data BlockType  = NormalBlock | PowerBlock deriving (Eq)
data GameState  = GameState {
                     paddle  :: Paddle,
                     ball    :: Ball,
                     blocks  :: [Block]}
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

draw (GameState paddle ball blocks) = do
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

-- startup
main = setOnLoad initilize

initilize :: IO ()
initilize = do
  (inputSignal, inputSink) <- external []
  signal <- start $ mainSignal inputSignal
  input  <- newIORef ([] :: [InputEvent])
  setOnKeyDown canvasName (onKeyDown input)
  setOnKeyUp   canvasName (onKeyUp   input)
  setInterval 30.0 (update input inputSink signal)

-- reactions to input events
onKeyDown :: IORef [InputEvent] -> Int -> IO ()
onKeyDown input code = do
  soFar <- readIORef input
  writeIORef input $ soFar ++ [KeyDown code]
 
onKeyUp :: IORef [InputEvent] -> Int -> IO ()
onKeyUp input code = do
  soFar <- readIORef input
  writeIORef input $ soFar ++ [KeyUp code]

-- update
update :: IORef [InputEvent] -> ([InputEvent] -> IO ()) -> IO GameState  -> IO ()
update input inputSink signal = do
  i <- readIORef input
  writeIORef input []
  inputSink i
  gs <- signal
  draw gs

-- Signals


-- helper functions
integral v0 s = transfer v0 (\v v0 -> v0+v) s

transferE :: a -> (a -> b -> a) -> Signal [b] -> SignalGen (Signal a)
transferE init trans = transfer init step
  where
  step input start = foldl' trans start input

keyDown :: Int -> Signal [InputEvent] -> SignalGen (Signal Bool)
keyDown code = transferE False step
  where
  step old input
    | input == KeyUp code   = False
    | input == KeyDown code = True
    | otherwise             = old

-- main Signal
mainSignal :: Signal [InputEvent] -> SignalGen (Signal GameState)
mainSignal inputSignal = do
  leftDown <- keyDown leftKeyCode inputSignal
  rightDown <- keyDown rightKeyCode inputSignal
  let paddleSpeed :: Signal Double
      paddleSpeed = (\l r -> if l then -1.0 else 0.0 + if r then 1.0 else 0.0) <$> leftDown <*> rightDown
  paddlePos <- integral initPaddleXPos paddleSpeed
  
  return $ (\p -> GameState (Paddle p) initBall initBlocks) <$> paddlePos
