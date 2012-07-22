module JavaScript(
  module Haste,
  getContext2d,
  fillRect,
  setFillColor,
  clear,
  setInterval,
  setOnLoad,
  setOnKeyDown,
  setOnKeyUp,
  saveGlobalObject,
  loadGlobalObject
)
 where

import Haste
import Haste.Prim
import Haste.DOM

newtype Context2D = Context2D JSAny

foreign import ccall "jsGetContext2D"
  getContext2dFromCanvas :: Elem -> IO Context2D

getContext2d name = withElem name getContext2dFromCanvas

foreign import ccall "jsFillRect"
  fillRect :: Context2D -> Double -> Double -> Double -> Double -> IO ()


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

foreign import ccall jsSaveGlobalObject :: JSString -> Ptr a -> IO ()
foreign import ccall jsLoadGlobalObject :: JSString -> IO (Ptr a)

saveGlobalObject :: String -> a -> IO ()
saveGlobalObject name obj = jsSaveGlobalObject (toJSStr name) (toPtr obj)

loadGlobalObject :: String -> IO a
loadGlobalObject name = do
  ptr <- jsLoadGlobalObject (toJSStr name)
  return $ fromPtr ptr

