module Main where

import Data.IORef
import qualified Control.Monad as CM
import Control.Wire
import Prelude hiding ((.), id)

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Video as SDL
import qualified Graphics.UI.SDL.Events as SDL
import qualified Graphics.UI.SDL.Keysym as SDL
import qualified Graphics.UI.SDL.General as SDL

-- main Wire type
type MainWireType = WireP SDL.Event State

-- technical data
leftKeyCode  = SDL.SDLK_LEFT
rightKeyCode = SDL.SDLK_RIGHT

-- State
data State = State Bool Bool Double

-- Startup

main = SDL.withInit [SDL.InitEverything] $ do
  screen <- SDL.setVideoMode 800 600 32 []
  SDL.setCaption "Netwire Test" "Netwire Test"
  session0 <- clockSession

  mainLoop mainWire session0
  where
    mainLoop w session = do
      event <- SDL.pollEvent
      CM.unless (event == SDL.Quit) $ do
        (res, w', session') <- stepSession session event w
        case res of
          Left ex -> return ()
          Right x -> draw x
        mainLoop w' session'

-- draw
--draw :: Double -> IO ()
--draw s = do
--  putStrLn $ "Speed: " ++ show s
draw (State l r s) = do
  putStrLn $ "Left: " ++ show l ++ "\tRight: " ++ show r ++ "\tSpeed: " ++ show s

-- helper wires
-- Produces when the event is the given KeyDown event
keyDown :: SDL.SDLKey -> EventP SDL.Event 
keyDown = when . isSym
  where
  isSym :: SDL.SDLKey -> SDL.Event -> Bool
  isSym key (SDL.KeyDown (SDL.Keysym key' _ _)) = key == key'
  isSym _ _ = False

keyUp :: SDL.SDLKey -> EventP SDL.Event 
keyUp = when . isSym
  where
  isSym :: SDL.SDLKey -> SDL.Event -> Bool
  isSym key (SDL.KeyUp (SDL.Keysym key' _ _)) = key == key'
  isSym _ _ = False

-- Indicates if a given key is pressed at all time
-- inhibits when it is not pressed
isKeyPressed :: SDL.SDLKey -> EventP SDL.Event
isKeyPressed code = switch (empty <$ (keyUp code) <|> identity <$ (keyDown code)) empty
  where
  identity = mkPure $ \_ x -> (Right x, identity)

-- mainWire
mainWire :: MainWireType
mainWire = State <$> ((pure True) . isKeyPressed leftKeyCode <|> (pure False)) <*> ((pure True) . isKeyPressed rightKeyCode <|> (pure False)) <*> speed

speed :: WireP SDL.Event Double
speed = (((pure (-1.0)) . isKeyPressed leftKeyCode) <|> pure 0.0)
              +
              (((pure 1.0)  . isKeyPressed rightKeyCode) <|> pure 0.0)
