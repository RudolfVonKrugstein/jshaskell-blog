module JavaScript(
  module Haste,
  Context2D,
  getContext2d,
  fillRect,
  fillRoundedRect,
  fillCircle,
  fillText,
  setFillColor,
  Color(..),
  clear,
  setInterval,
  setOnLoad,
  setOnKeyDown,
  setOnKeyUp,
  hasteTrace
)
 where

import Haste hiding (Event)
import Haste.Prim
import Haste.DOM
import System.IO.Unsafe

newtype Context2D = Context2D JSAny

foreign import ccall "jsGetContext2D"
  getContext2dFromCanvas :: Elem -> IO Context2D

getContext2d name = withElem name getContext2dFromCanvas

foreign import ccall "jsFillRect"
  fillRect :: Context2D -> Double -> Double -> Double -> Double -> IO ()

foreign import ccall "jsFillText"
  jsFillText :: Context2D -> JSString -> Double -> Double -> IO ()
fillText ctx str = jsFillText ctx (toJSStr str)

foreign import ccall "jsBeginPath" beginPath :: Context2D -> IO ()
foreign import ccall "jsClosePath" closePath :: Context2D -> IO ()
foreign import ccall "jsFill" fill :: Context2D -> IO ()
foreign import ccall "jsArc" arc :: Context2D -> Double -> Double -> Double -> Double -> Double -> Bool -> IO ()
foreign import ccall "jsMoveTo" moveTo :: Context2D -> Double -> Double -> IO ()
foreign import ccall "jsLineTo" lineTo :: Context2D -> Double -> Double -> IO ()

fillCircle :: Context2D -> Double -> Double -> Double -> IO ()
fillCircle ctx x y r = do
  beginPath ctx
  arc ctx x y r 0.0 (2.0 * pi) True
  closePath ctx
  fill ctx

fillRoundedRect :: Context2D -> Double -> Double -> Double -> Double -> Double -> IO ()
fillRoundedRect ctx x y w h r = do
  beginPath ctx
  let ex  = x + w
      ey  = y + h
      r2d = pi / 180.0
  moveTo ctx (x + r)    y
  lineTo ctx (ex- r)    y
  arc ctx (ex-r) (y+r) r (r2d*270.0) (r2d*360.0) False
  lineTo ctx ex (ey-r)
  arc ctx (ex-r) (ey-r) r (0.0) (r2d*90.0) False
  lineTo ctx (x+r) ey
  arc ctx (x+r) (ey-r) r (r2d*90.0) (r2d*180.0) False
  lineTo ctx x (y+r)
  arc ctx (x+r) (y+r) r (r2d*180.0) (r2d*270.0) False
  closePath ctx
  fill ctx

data Color = Color {red :: Double, green :: Double, blue :: Double, alpha :: Double}

foreign import ccall jsSetFillColor :: Context2D -> Double -> Double -> Double -> Double -> IO ()
setFillColor :: Context2D -> Color -> IO ()
setFillColor ctx color = jsSetFillColor ctx (red color) (green color) (blue color) (alpha color)

foreign import ccall "jsClear"
  clear :: Context2D -> IO ()

foreign import ccall jsSetInterval :: Double -> JSFun (IO ()) -> IO ()
setInterval :: Double -> IO () -> IO ()
setInterval time cb =
  jsSetInterval time (mkCallback $! cb)

foreign import ccall jsSetOnLoad :: JSFun (IO ()) -> IO ()
setOnLoad cb = jsSetOnLoad (mkCallback $! cb)

setOnKeyDown :: String -> (Int -> IO ()) -> IO Bool
setOnKeyDown elementName cb = withElem elementName $ \e -> setCallback e OnKeyDown cb
  
setOnKeyUp :: String -> (Int -> IO ()) -> IO Bool
setOnKeyUp elementName cb = withElem elementName $ \e -> setCallback e OnKeyUp cb

hasteTrace :: String -> a -> a
hasteTrace s i = unsafePerformIO $ do
  alert s
  return i
