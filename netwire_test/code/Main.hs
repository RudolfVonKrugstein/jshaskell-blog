module Main where

import JavaScript
import Data.IORef
import qualified Control.Monad as CM
import Control.Wire
import Prelude hiding ((.), id)

canvasName = "canvas4"

-- input events
data InputEvent = KeyUp Int | KeyDown Int | Update
  deriving (Eq, Show)

-- main Wire type
type MainWireType = WireP InputEvent GameState

-- game state
data GameState = GameState Double

-- technical data
leftKeyCode  = 37
rightKeyCode = 39

screenWidth = 600.0
screenHeight = 400.0

-- Startup
main = setOnLoad initilize

initilize :: IO ()
initilize = do
  wire <- newIORef mainWire
  setOnKeyDown canvasName (onKeyDown wire)
  setOnKeyUp   canvasName (onKeyUp wire)
  setInterval 30.0 (update wire)

-- event reactions
onKeyDown :: IORef MainWireType -> Int -> IO ()
onKeyDown wire code = do
  w <- readIORef wire
  let (_, w') = stepWire 0.0 (KeyDown code) w
  writeIORef wire w'

onKeyUp :: IORef MainWireType -> Int -> IO ()
onKeyUp wire code = do
  w <- readIORef wire
  let (_, w') = stepWire 0.0 (KeyUp code) w
  writeIORef wire w'

-- update
update :: IORef MainWireType -> IO ()
update wire = do
  w <- readIORef wire
  let (res, w') = stepWire 30.0 Update w
  case res of
    Left err -> alert "Error"
    Right gs -> draw gs
  writeIORef wire w'

-- draw
draw :: GameState -> IO ()
draw (GameState s) = do
  ctx <- getContext2d canvasName
  clear ctx
  fillText ctx ("Speed: " ++ show s) (screenWidth /2.0 - 100.0) (screenHeight/2.0 - 20.0)

-- helper wires
-- Produces when the event is the given KeyDown event
keyDown :: Int -> EventP InputEvent 
keyDown code = when (==KeyDown code)

keyUp :: Int -> EventP InputEvent
keyUp code = when (==KeyUp code)

-- Indicates if a given key is pressed at all time
-- inhibits when it is not pressed
isKeyPressed :: Int -> EventP InputEvent
isKeyPressed code = switch (empty <$ (keyUp code) <|> identity <$ (keyDown code)) empty
  where
  identity = mkPure $ \_ x -> (Right x, identity)

-- mainWire
mainWire :: MainWireType
mainWire = GameState <$> paddleSpeed

paddleSpeed :: WireP InputEvent Double
paddleSpeed = ((1.0) . isKeyPressed leftKeyCode) <|> 0.0
                +
              (  1.0  . isKeyPressed rightKeyCode) <|> 0.0
