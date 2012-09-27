module Main where

import JavaScript
import Data.IORef
import Haste
import Haste.DOM
import Control.Wire
import Prelude hiding ((.), id)

type MainWire = WireP () Double

writeOut :: String -> IO ()
writeOut s = do
  oldv <- withElem "output" $ \e -> getProp e "value"
  withElem "output" $ \e -> setProp e "value" (oldv ++ s)

main = setOnLoad initilize

initilize = do
  session <- clockSession
  state <- newIORef mainWire
  setInterval 20.0 (update state)

update :: IORef MainWire -> IO ()
update state = do
  wire <- readIORef state
  let (res, wire') = stepWire 20.0 () wire
  case res of
    Left ex -> writeOut $ "Error: " ++ show ex
    Right x -> writeOut $ "Output: " ++ show x
  writeIORef state wire'

mainWire :: MainWire
mainWire =
  avg 1000 . as pDouble . noise (mkStdGen 0)
