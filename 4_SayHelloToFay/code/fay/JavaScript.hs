module JavaScript where

import Language.Fay.FFI
import Language.Fay.Prelude


data JSKeyEvent
data Document
data Element
data Context2D

alert :: String -> Fay ()
alert = foreignFay "window.alert" ""

document :: Fay Document
document = foreignDay "document"

getElementById :: String -> Fay Element
getElementById = foreignFay "document.getElementById" ""

getContext2dFromCanvas :: Element -> Fay Conext2D
getContext2dFromCanvas e = foreignMethodFay "getContext" "" e "2d"

getContext2d :: String -> Fay Context2D
getContext2d canvasName = do
  c <- getElementById canvasName
  getContext2dFromCanvas c

fillRect :: Context2D -> Double -> Double -> Double -> Double -> Fay ()
fillRet = foreignMethodFay "fillRect" ""

setFillColor :: Context2D -> String -> Fay ()
setFillColor = foreignMethodFay "setFillColor" ""

clearRect :: Context2D -> Double -> Double -> Double -> Double -> Fay ()
clearRect = foreignMathodFay "clearRect" ""

--- done till here

foreign import js "%1.canvas.width" canvasWidth :: Context2D -> IO Double
foreign import js "%1.canvas.height" canvasHeight :: Context2D -> IO Double
clear :: Context2D -> IO ()
clear ctx = do
  w <- canvasWidth ctx
  h <- canvasHeight ctx
  clearRect ctx 0.0 0.0 w h

foreign import js "%1.keyCode"
  keyCode :: JSKeyEvent -> IO Int


foreign import js "%1.addEventListener('keydown',%2,true)"
  jsSetOnKeyDown :: Element -> FunPtr (JSKeyEvent -> IO ()) -> IO ()
setOnKeyDown :: String -> (Int -> IO ()) -> IO ()
setOnKeyDown elemName fp = do
  cb <- mkKeyEventCb fp'
  el <- getElementById elemName
  jsSetOnKeyDown el cb
  where
    fp' event = keyCode event >>= fp

foreign import js "%1.addEventListener('keyup',%2,true)"
  jsSetOnKeyUp :: Element -> FunPtr (JSKeyEvent -> IO ()) -> IO ()

setOnKeyUp :: String -> (Int -> IO ()) -> IO ()
setOnKeyUp elemName fp = do
  cb <- mkKeyEventCb fp'
  el <- getElementById elemName
  jsSetOnKeyUp el cb
  where
    fp' event = keyCode event >>= fp

foreign import js "window.addEventListener('load', %1, 'false')"
  jsSetOnLoad :: FunPtr (IO ()) -> IO ()
setOnLoad :: IO () -> IO ()
setOnLoad fp = mkCb fp >>= jsSetOnLoad

foreign import js "setInterval(%1,%2)"
  jsSetInterval :: FunPtr (IO ()) -> Double -> IO ()
setInterval :: Double -> IO () -> IO ()
setInterval time fp = do
  cb <- mkCb fp
  jsSetInterval cb time

foreign import ccall jsSaveGlobalObject :: JSString -> a -> IO ()
foreign import ccall jsLoadGlobalObject :: JSString -> IO a

saveGlobalObject :: String -> a -> IO ()
saveGlobalObject name obj = jsSaveGlobalObject (toJS name) obj

loadGlobalObject :: String -> IO a
loadGlobalObject name = do
  ptr <- jsLoadGlobalObject (toJS name)
  return $ ptr
