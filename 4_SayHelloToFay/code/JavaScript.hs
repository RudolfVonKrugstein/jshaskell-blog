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
alert = foreignFay "window.alert" FayNone

document :: Fay Document
document = foreignValue "document" FayNone

getElementById :: String -> Fay Element
getElementById = foreignFay "document.getElementById" FayNone

getContext2dFromCanvas :: Element -> Fay Context2D
getContext2dFromCanvas e = foreignMethodFay "getContext" FayNone e "2d"

getContext2d :: String -> Fay Context2D
getContext2d canvasName = do
  c <- getElementById canvasName
  getContext2dFromCanvas c

fillRect :: Context2D -> Double -> Double -> Double -> Double -> Fay ()
fillRect = foreignMethodFay "fillRect" FayNone

setFillColor :: Context2D -> String -> Fay ()
setFillColor = foreignSetProp "setFillColor"

clearRect :: Context2D -> Double -> Double -> Double -> Double -> Fay ()
clearRect = foreignMethodFay "clearRect" FayNone

canvasWidth :: Context2D -> Fay Double
canvasWidth = foreignPropFay "width" FayNone

canvasHeight :: Context2D -> Fay Double
canvasHeight = foreignPropFay "height" FayNone

clear :: Context2D -> Fay ()
clear ctx = do
  w <- canvasWidth ctx
  h <- canvasHeight ctx
  clearRect ctx 0.0 0.0 w h

keyCode :: JSKeyEvent -> Fay Double
keyCode = foreignFay "jsKeyCode" FayNone

addEventListener :: Element -> String -> Fay () -> Bool -> Fay ()
addEventListener =
    foreignMethodFay "addEventListener" FayNone

-- We need another because the callback signature is different
addKeyEventListener :: Element -> String -> (JSKeyEvent -> Fay ()) -> Bool -> Fay ()
addKeyEventListener =
    foreignMethodFay "addEventListener" FayNone

setOnKeyDown :: String -> (Double -> Fay ()) -> Fay ()
setOnKeyDown elemName fp = do 
  el <- getElementById elemName
  addKeyEventListener el "keydown" (\e -> keyCode e >>= fp) False

setOnKeyUp :: String -> (Double -> Fay ()) -> Fay ()
setOnKeyUp elemName fp = do 
  el <- getElementById elemName
  addKeyEventListener el "keyup" (\e -> keyCode e >>= fp) False

addWinEventListener :: String -> Fay () -> Bool -> Fay ()
addWinEventListener = foreignFay "window.addEventListener" FayNone

setOnLoad :: Fay () -> Fay ()
setOnLoad fp = do
  addWinEventListener "load" fp False

setInterval :: Fay () -> Double -> Fay () 
setInterval = foreignFay "window.setInterval" FayNone

-- Ref

-- | A mutable reference like IORef.
data Ref a
instance Foreign a => Foreign (Ref a)

-- | Make a new mutable reference.
newRef :: Foreign a => a -> Fay (Ref a)
newRef = foreignFay "new Fay$$Ref" FayNone

-- | Replace the value in the mutable reference.
writeRef :: Foreign a => Ref a -> a -> Fay ()
writeRef = foreignFay "Fay$$writeRef" FayNone

-- | Get the referred value from the mutable value.
readRef :: Foreign a => Ref a -> Fay a
readRef = foreignFay "Fay$$readRef" FayNone
