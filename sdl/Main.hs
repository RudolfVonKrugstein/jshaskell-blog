module Main where

import Graphics.UI.SDL
import Graphics.UI.SDL.Time
import Graphics.UI.SDL.Keysym
import Graphics.UI.SDL.Utilities as U
import Control.Monad
import Data.Word

main :: IO ()
main = start emptyRunConfig

data RunConfig = RunConfig { update         :: IO ()
                 , updateInterval :: Word32
                 , onKeyUp        :: Int -> IO ()
                 , onKeyDown      :: Int -> IO ()
                 }

emptyRunConfig = RunConfig (return ()) 20 (\_ -> return ()) (\_ -> return ())

start :: RunConfig -> IO ()
start rc@(RunConfig update interval keyUp keyDown) = withInit [InitEverything] $ do
  screen <- setVideoMode 800 600 32 []
  setCaption "SDL test" []
 
  startTime <- getTicks 
  mainLoop startTime
  where
    mainLoop :: Word32 -> IO ()
    mainLoop startTime = do
      time <- getTicks
      -- delay
      unless (time >= startTime + interval) $ delay $ startTime + interval - time
      -- events
      quit <- handleEvents
      -- update and continiue
      update
      unless quit $ mainLoop $ min (startTime + interval) time

    handleEvents ::IO Bool
    handleEvents = do
      events <- liftM ( takeWhile (/= NoEvent)) $ sequence $ repeat pollEvent
      mapM_ handleEvent events
      -- return value
      return $ Quit `elem` events
      where
        handleEvent :: Event -> IO ()
        handleEvent e = case e of
          KeyDown sym -> keyDown $ (fromIntegral . U.fromEnum . symKey $ sym)
          KeyUp   sym -> keyUp   $ (fromIntegral . U.fromEnum . symKey $ sym)
          _           -> return ()
    


