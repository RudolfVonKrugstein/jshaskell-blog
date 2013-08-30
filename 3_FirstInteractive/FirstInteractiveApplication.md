In the [last Post](http://jshaskell.blogspot.de/2012/07/hello-world.html) we wrote the first "Hello, World!" application. We saw how to import javascript functions in UHC and haste.

We now want to do something more game like. Out goal over the next few post will be to write a breackout clone in haskell, running in the browser!
But first there are still a few things we need. To explore this we will write a little application displaying the paddle that can be moved with the arrow keys. Here is a preview (you have to click on it so that it gains focus):

<script src="https://rawgithub.com/RudolfVonKrugstein/jshaskell-blog/master/3_FirstInteractive/code/compiled/Main.js" type="text/javascript">
</script>
<canvas height="400" id="canvas1" style="background-color: white;" width="600" tabindex="0"></canvas>

**Update:** This should now also work in firefox.

For this we need to learn how to:

* Set callbacks
* Let different callback communicate
* Draw on the canvas

# GameLoop in JavaScript

Before we want to start our game, we have to allow the browser to load the full page and its elements. Otherwise we can not access e.g. the canvas (the drawing area we will use).

The browser tells us, that it is done with loading by invoking the callback window.onLoad.
Depending on how we compile with haste, our main will already be set to the window.onLoad (the option --start==asap prevents this), but in UHC we have to set a callback by hand. We will use the --start=asap option in haste so that our main code can be the same for haste and UHC.

As can be read at several places (e. g. [here](http://nokarma.org/2011/02/02/javascript-game-development-the-game-loop/index.html) or [here](http://www.playmycode.com/blog/2011/08/building-a-game-mainloop-in-javascript/)) we can not just write an infinite loop for our GameLoop in javascript because it would block the browser. The contents of the canvas will only be updated when our code returns.

So we need a function that is called in intervals. Javascript allows us to set the interval with window.setInterval.

# Setting callbacks

We need to set haskell functions as callbacks to be invoked from javascript code.

## UHC

In UHC we import a special function which converts haskell functions to callbacks that can be called from javascript (see [Improving UHC js](http://www.norm2782.com/improving-uhc-js-report.pdf)).

```haskell
import UHC.Ptr
foreign import js "wrapper" mkCb :: IO () -> IO (FunPtr (IO ()))
```

This converts an IO action to a function pointer that can be passed to javascript.

If we want our callback to have different or more arguments, we have to import wrapper with a different signature. Here we create callbacks that we can pass as key event handlers.

```haskell
data JSKeyEvent
foreign import js "wrapper"
    mkKeyEventCb :: (JSKeyEvent -> IO ()) -> IO (FunPtr (JSKeyEvent -> IO ()))
```

We now need to import the functions with which we set the callbacks. The interval function is set with "setInterval" while "onLoad" and the key event callbacks can be set with "addEventListener" which must be called on an element of the webpage. This element can be retrieved with "getElementById". We define simplified versions that take care of creating the callback for us.

```haskell
data Element

foreign import js "document.getElementById(%1)"
    jsGetElementById :: JSString -> IO Element
getElementById = jsGetElementById . toJS

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
```

Remember that the >>= operator chains monadic actions. "setOnKeyDown" and "setOnKeyUp" set the event listener on an element defined by the given name. They define wrapper functions that extract the keycode and passes it to our callback functions. This is convenient because the keycode is the information we are really interested in.

We will follow the convention, that functions taking javascript specific parameters (such as JSString) will be prefixed by "js" and have corresponding functions without the "js" prefix.

## Haste

> **Update:** The way FFI functions have to writting with haste has changed since the blog post has original been written. At that time returning values from javascript to haskell
> was a little bit more cumbersome. I have updated this blog post to reflect the new way of doing it. I hope I did not forget something in the process. So if you find an error, please comment.

Setting callbacks in haste is a little different.

For every callback function a javascript function has to be created which invokes the special function "A()" with the callback. This function can than be used for the callback. Read the section "Callbacks" of [js-externls.txt](https://github.com/valderman/haste-compiler/blob/master/doc/js-externals.txt) in the doc subdirectory of the [haste github repository](https://github.com/valderman/haste-compiler).

The arguments for the haskell function are the second argument of "A()" and have to be passed as a list similar to the required return value of javascript function included by the FFI. Again, this is explained in [js-externals.txt](https://github.com/valderman/haste-compiler/blob/master/doc/js-externals.txt).

```javascript
function jsSetInterval(msecs, cb) {
    window.setInterval(function() {A(cb,[0]);}, msecs);
    return;
}

function jsSetOnLoad(cb) {
    window.addEventListener('load', function() {A(cb,[0]);}, false);
    return;
}
```

For "setOnKeyUp" and "setOnKeyDown" we do not need to define any haskell function, because we can set them using the haste library in haskell.
On the haskell side callbacks have to be created with "mkCallback" and have the Type "JSFun a".

```haskell
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
```

The "withElem" functions is defined in the haste library and executes an action with a webpage element defined by the provided name.

# Letting callbacks communicate

## UHC

In a javascript program we would let the onKeyUp, onKeyDown and Interval functions communicate through global variables. In haskell we do not have a mechanism such as global variables (at least non that I am aware of). In a normal situation we do not need it, because the only moment when main exits is when the program ends.

To store global variables we write a few helper functions in javascript:

```javascript
var allObjects = {}

function jsSaveGlobalObject(name, obj) {
 allObjects[name] = obj;
}

function jsLoadGlobalObject(name) {
 return allObjects[name];
}
```

And include them from haskell:

```haskell
foreign import ccall jsSaveGlobalObject :: JSString -> a -> IO ()
foreign import ccall jsLoadGlobalObject :: JSString -> IO a

saveGlobalObject :: String -> a -> IO ()
saveGlobalObject name obj = jsSaveGlobalObject (toJS name) obj

loadGlobalObject :: String -> IO a
loadGlobalObject name = do
  ptr <- jsLoadGlobalObject (toJS name)
  return $ ptr
```

We can now load the current state with

```haskell
state <- jsLoadGlobalObject "state" :: IO State
```

When we enter one of our callback functions and save it with a corresponding call to "saveGlobalObject".

## Haste

For haste the mechanism works the same way, only that the javascript helper functions have to format their return values the way haste needs them:

```javascript
var allObjects = {}

function jsSaveGlobalObject(name, obj) {
 allObjects[name] = obj;
 return;
}

function jsLoadGlobalObject(name) {
 return allObjects[name];
}
```

To be able to pass arbitrary objects to the FFI, they have to be converted to a pointer via "fromPtr" and "toPtr".

```haskell
foreign import ccall jsSaveGlobalObject :: JSString -> Ptr a -> IO ()
foreign import ccall jsLoadGlobalObject :: JSString -> IO (Ptr a)

saveGlobalObject :: String -> a -> IO ()
saveGlobalObject name obj = jsSaveGlobalObject (toJSStr name) (toPtr obj)

loadGlobalObject :: String -> IO a
loadGlobalObject name = do
  ptr <- jsLoadGlobalObject (toJSStr name)
  return $ fromPtr ptr
```

# Drawing on the canvas

Example code for drawing on the canvas in javascript looks like this:

```javascript
context = document.getElementById("canvas").getContext("2d");
context.clearRect(0.0, 0.0, context.canvas.width, context.canvas.height)
context.setFillColor("green");
context.fillRect(10.0,10.0,100.0,100.0);
```

Basically all we have to do is import these functions via the FFI.

## UHC

Getting the context needs several steps:

* Get the canvas via getElementById
* Get the context via getContext

So this is what we do:

```haskell
data Context2D
foreign import js "%1.getContext('2d')"
    getContext2dFromCanvas :: Element -> IO Context2D

getContext2d :: String -> IO Context2D
getContext2d canvasName = do
  c <- getElementById canvasName
  getContext2dFromCanvas c
```

Importing the rest of the functions is straight forward:

```haskell
foreign import js "%1.fillRect(%*)"
  fillRect :: Context2D -> Double -> Double -> Double -> Double -> IO ()
foreign import js "jsSetFillColor(%*)"
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
```

We have defined "clear" for convenience. It clears the whole canvas.

** Note: ** For some reason, "context.setFillColor" does not work on firefox. Therefor a helper function is defined setting the color via fillStyle.

```JavaScript
function jsSetFillColor(context, color) {
	context.fillStyle = color;
}
```


## Haste

Again, for haste we have to write javascript functions with the correct return type:

```javascript
function jsGetContext2d(canvas) {
 return canvas.getContext("2d");
}

function jsFillRect(context, x, y, width, height) {
 context.fillRect(x,y,width,height);
 return;
}

function jsSetFillColor(context, color) {
 context.fillStyle = color;
 return;
}

function jsClear(context) {
 context.clearRect(0.0, 0.0, context.canvas.width, context.canvas.height);
 return;
}
```

And here the haskell part:

```haskell
import Haste.Prim
import Haste.DOM
newtype Context2D = Context2D JSAny

foreign import ccall "jsGetContext2D"
  jsGetContext2d :: Elem -> IO Context2D
getContext2d name = withElem name getContext2D

foreign import ccall "jsFillRect"
  fillRect :: Context2D -> Double -> Double -> Double -> Double -> IO ()
foreign import ccall jsSetFillColor :: Context2D -> JSString -> IO ()
setFillColor ctx = jsSetFillColor ctx . toJSStr
foreign import ccall "jsClear"
  clear :: Context2D -> IO ()
```

Again we use "withElem" function from Haskell.DOM which executes an action with the element specified by the provided name.

# Putting it all together

All imported functions are named, so that the main code is the same for UHC and haste. The folloging code should explain itself through the comments:

```{.haskell .numberlines}
module Main where

import JavaScript

canvasName = "canvas1"

playerY = 380.0
playerWidth = 60.0
playerHeight = 20.0
playerSpeed = 3.0
playerColor = "green"

data State = State {x :: Double}
initState = State 300.0

main = setOnLoad initilize

initilize :: IO ()
initilize = do
  saveGlobalObject "state" initState
  setInterval 30.0 update
  setOnKeyDown canvasName onKeyDown
  return ()

onKeyDown :: Int -> IO ()
onKeyDown code = do
  s <-  loadGlobalObject "state" :: IO State
  let s' = case code of
         39 ->  s {x = (x s) + playerSpeed}
         37 ->  s {x = (x s) - playerSpeed} 
         _  ->  s
  saveGlobalObject "state" s'

update :: IO ()
update = do
  s <-  loadGlobalObject "state" :: IO State
  ctx <- getContext2d canvasName
  clear    ctx
  setFillColor ctx playerColor
  fillRect ctx (x s) playerY playerWidth playerHeight
```

To compile with UHC you need [JavaScript.hs](https://github.com/RudolfVonKrugstein/jshaskell-blog/blob/master/3_FirstInteractive/code/uhc/JavaScript.hs) and [helpers.js](https://github.com/RudolfVonKrugstein/jshaskell-blog/blob/master/3_FirstInteractive/code/uhc/helpers.js). Than run:

```bash
uhc -tjs Main.hs
```

Edit the resulting HTML page and add

```html
<script type="text/javascript" src="helpers.js"></script>
```

into the head and

```html
<canvas id="canvas1" width=600 height=500 tabindex="0"></canvas>
```

to the body.

For haste you need this [JavaScript.hs](https://github.com/RudolfVonKrugstein/jshaskell-blog/blob/master/3_FirstInteractive/code/haste/JavaScript.hs) und [helpers.js](https://github.com/RudolfVonKrugstein/jshaskell-blog/blob/master/3_FirstInteractive/code/haste/helpers.js) and compile it with:

```bash
hastec --with-js=helpers.js Main.hs --start=asap
```

The "--start=asap" parameter is necessary because we set the onLoad function ourself. Than embed it in this HTML page:

```html
<!DOCTYPE html><html><head><title>Main</title>
		<script type="text/javascript" src="Main.js"></script>
	</head>
	<body>
		<canvas id="canvas1" height="400" width="600" tabindex="0">Your browser des not support canvas</canvas>
	</body>
</html>
```

The running example can be seen at the beginning of this post.

## Summary

This has been a rather long post, but we have learned a lot. With the tools at hand we can now update out game with an callback function we set via "setInterval" and receive keyboard events.
The next post will be another step towards our goal of a breakout clone, we will implement the ball and let it bounce on the paddle and the walls.
