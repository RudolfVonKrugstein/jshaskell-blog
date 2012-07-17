import UHC.Ptr

data JSKeyEvent
data Document
data Context2D
data Canvas
type JSString = PackedString
foreign import prim "primStringToPackedString" toJS :: String -> JSString

foreign import js "alert(%1)" jsAlert :: JSString -> IO ()
alert = jsAlert . toJS

foreign import js "document"
    document :: IO Document
foreign import js "%1.getElementById(%2)"
    jsGetElementById :: Document -> JSString -> IO Canvas
getElementById doc = jsGetElementById doc . toJS
foreign import js "%1.getContext('2d')"
    getContext2dFromCanvas :: Canvas -> IO Context2D

getContext2d :: String -> IO Context2D
getContext2d canvasName = do
  d <- document
  c <- getElementById d canvasName
  getContext2dFromCanvas c

foreign import js "%1.fillRect(%*)"
  fillRect :: Context2D -> Double -> Double -> Double -> Double -> IO ()
foreign import js "%1.setFillColor(%*)"
  jsSetFillColor :: Context2D -> JSString -> IO ()
setFillColor ctx = jsSetFillColor ctx . toJS
foreign import js "%1.clearRect(%2, %3, %4, %5)"
  clearRect :: Context2D -> Double -> Double -> Double -> Double -> IO ()

foreign import js "%1.canvas.width" canvasWidth :: Context2D -> IO Double
foreign import js "%1.canvas.height" canvasHeight :: Context2D -> IO Double
clear :: Context2D -> IO ()
clear ctx = do
  w <- canvasWidth ctx
  h <- canvasHeight ctx
  clearRect ctx 0.0 0.0 w h
  

foreign import js "wrapper" mkCb :: IO () -> IO (FunPtr (IO ()))
foreign import js "wrapper"
    mkKeyEventCb :: (JSKeyEvent -> IO ()) -> IO (FunPtr (JSKeyEvent -> IO ()))

foreign import js "%1.keyCode"
  keyCode :: JSKeyEvent -> IO Int


foreign import js "%1.addEventListener('keydown',%2,true)"
  jsSetOnKeyDown :: JSString -> FunPtr (JSKeyEvent -> IO ()) -> IO ()
setOnKeyDown :: String -> (Int -> IO ()) -> IO ()
setOnKeyDown elemName fp = do
  cb <- mkKeyEventCb fp'
  jsSetOnKeyDown (toJS elemName) cb
  where
    fp' event = keyCode event >>= fp

foreign import js "%1.addEventListener('keyup',%2,true)"
  jsSetOnKeyUp :: JSString -> FunPtr (JSKeyEvent -> IO ()) -> IO ()

setOnKeyUp :: String -> (Int -> IO ()) -> IO ()
setOnKeyUp elemName fp = do
  cb <- mkKeyEventCb fp'
  jsSetOnKeyUp (toJS elemName) cb
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
