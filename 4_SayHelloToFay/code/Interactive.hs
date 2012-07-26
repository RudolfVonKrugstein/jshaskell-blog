module Interactive where

import JavaScript
import Language.Fay.FFI
import Language.Fay.Prelude
import Prelude (Bool,String,Double,Char,error)

canvasName = "canvas1"

playerY = 380.0
playerWidth = 60.0
playerHeight = 20.0
playerSpeed = 3.0
playerColor = "green"

data State = State {x :: Double}
instance Foreign State
initState = State 300.0

main = setOnLoad initilize

initilize :: Fay ()
initilize = do
  saveGlobalObject "state" initState
  setInterval update 30.0
  setOnKeyDown canvasName onKeyDown
  return ()

onKeyDown :: Double -> Fay ()
onKeyDown code = do
  s <-  loadGlobalObject "state" :: Fay State
  let s' = case code of
         39 ->  s {x = (x s) + playerSpeed}
         37 ->  s {x = (x s) - playerSpeed} 
         _  ->  s
  saveGlobalObject "state" s'

update :: Fay ()
update = do
  s <-  loadGlobalObject "state" :: Fay State
  ctx <- getContext2d canvasName
  clear    ctx
  setFillColor ctx playerColor
  fillRect ctx (x s) playerY playerWidth playerHeight

