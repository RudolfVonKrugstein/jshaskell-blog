module JavaScript(
  module Haste,
  Context2D,
  getContext2d,
  fillRect,
  fillCircle,
  fillText,
  setFillColor,
  clear,
  setInterval,
  setOnLoad,
  setOnKeyDown,
  setOnKeyUp
)
 where

import Haste hiding (Event)
import Haste.Prim
import Haste.DOM

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

fillCircle :: Context2D -> Double -> Double -> Double -> IO ()
fillCircle ctx x y r = do
  beginPath ctx
  arc ctx x y r 0.0 (2.0 * pi) True
  closePath ctx
  fill ctx

foreign import ccall jsSetFillColor :: Context2D -> JSString -> IO ()
setFillColor ctx = jsSetFillColor ctx . toJSStr
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

