module JavaScript where

import Language.Fay.FFI
import Language.Fay.Prelude
import Prelude (Bool,String,Double,Char,error)


data JSKeyEvent
data Document
data Element
data Context2D

instance Foreign JSKeyEvent
instance Foreign Document
instance Foreign Element
instance Foreign Context2D

alert :: String -> Fay ()
alert = foreignFay "window.alert" ""

document :: Fay Document
document = foreignFay "document" ""

window :: Fay Element
window = foreignFay "window" ""

getElementById :: String -> Fay Element
getElementById = foreignFay "document.getElementById" ""

getContext2dFromCanvas :: Element -> Fay Context2D
getContext2dFromCanvas e = foreignMethodFay "getContext" "" e "2d"

getContext2d :: String -> Fay Context2D
getContext2d canvasName = do
  c <- getElementById canvasName
  getContext2dFromCanvas c

fillRect :: Context2D -> Double -> Double -> Double -> Double -> Fay ()
fillRect = foreignMethodFay "fillRect" ""

setFillColor :: Context2D -> String -> Fay ()
setFillColor = foreignMethodFay "setFillColor" ""

clearRect :: Context2D -> Double -> Double -> Double -> Double -> Fay ()
clearRect = foreignMethodFay "clearRect" ""

canvasWidth :: Context2D -> Fay Double
canvasWidth = foreignFay "jsCanvasWidth" ""

canvasHeight :: Context2D -> Fay Double
canvasHeight = foreignFay "jsCanvasWidth" ""

clear :: Context2D -> Fay ()
clear ctx = do
  w <- canvasWidth ctx
  h <- canvasHeight ctx
  clearRect ctx 0.0 0.0 w h

keyCode :: JSKeyEvent -> Fay Double
keyCode = foreignFay "jsKeyCode" ""

addEventListener :: Element -> String -> Fay () -> Bool -> Fay ()
addEventListener =
    foreignMethodFay "addEventListener" ""

-- We need another because the callback signature is different
addKeyEventListener :: Element -> String -> (JSKeyEvent -> Fay ()) -> Bool -> Fay ()
addKeyEventListener =
    foreignMethodFay "addEventListener" ""

setOnKeyDown :: String -> (Double -> Fay ()) -> Fay ()
setOnKeyDown elemName fp = do 
  el <- getElementById elemName
  addKeyEventListener el "keydown" fp' False
  where
    fp' event = keyCode event >>= fp

setOnKeyUp :: String -> (Double -> Fay ()) -> Fay ()
setOnKeyUp elemName fp = do 
  el <- getElementById elemName
  addKeyEventListener el "keyup" fp' False
  where
    fp' event = keyCode event >>= fp

setOnLoad :: Fay () -> Fay ()
setOnLoad fp = do
  el <- window
  addEventListener el "load" fp False

setInterval :: Fay () -> Double -> Fay () 
setInterval = foreignFay "window.setInterval" ""

saveGlobalObject :: String -> a -> Fay ()
saveGlobalObject = foreignFay "jsSaveGlobalObject" ""

loadGlobalObject :: String -> Fay a
loadGlobalObject = foreignFay "jsLoadGlobalObject" ""

