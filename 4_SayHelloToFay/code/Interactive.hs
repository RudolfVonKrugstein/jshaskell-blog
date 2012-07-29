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

main :: Fay ()
main = do
  setOnLoad initilize

initilize :: Fay ()
initilize = do
  state <- newRef initState
  setInterval (update state) 30.0
  setOnKeyDown canvasName (onKeyDown state)
  return ()

onKeyDown :: Ref State -> Double -> Fay ()
onKeyDown state code = do
  s <- readRef state
  writeRef state $ case code of
                    39 -> State $ (x s) + playerSpeed
                    37 -> State $ (x s) - playerSpeed
                    _  -> s

update :: Ref State -> Fay ()
update state = do
  s <-  readRef state
  ctx <- getContext2d canvasName
  clear    ctx
  setFillColor ctx playerColor
  fillRect ctx (x s) playerY playerWidth playerHeight

