In the [last Pos](http://jshaskell.blogspot.de/2012/07/hello-world.html) we wrote the first "Hello, World!" application. We saw how to import javascript functions in UHC and haste.

We now want to do something more game like. Out goal over the next few post will be to write a breackout clone written in haskell, running in the browser!
But first there are still a few things we need. To explore this we will write a little application displaying the paddle that can be moved with the arrow keys. Here is a preview:
<script src="https://sites.google.com/site/mindlettice/interactiveApp.txt" type="text/javascript">
</script>
<canvas height="400" id="canvas1" style="background-color: white;" width="600"></canvas>

For this we need to learn howto:

* Set callbacks</li>
* Let different callback communicate
* Draw on the canvas

# GameLoop in JavaScript

Before we want to start our game, we have to allow the browser to load the full page and its elements. Otherwise we can not access e.g. the canvas (the drawing area we will use).

The browser tells us, that it is done with loading by invoking the callback window.onLoad.
When compiling with haste, our main will already be set to the window.onLoad, but in UHC we have to set a callback by hand.

As can be read at several places (e. g. (here)[http://nokarma.org/2011/02/02/javascript-game-development-the-game-loop/index.html) or [here](http://www.playmycode.com/blog/2011/08/building-a-game-mainloop-in-javascript/)) we can not just write an infinite loop for our GameLoop in javascript because it would block the browser. The contents of the canvas will only be updated when our code returns.

So we need a function that is called in intervals. Javascript allows us to set the interval with e. g. window.setInterval.

# Setting callbacks

We need to set haskell functions as callbacks to be invoked from javascript code.

## UHC

In UHC we import a special function for this which converts haskell functions to callbacks that can be called from javascript (see [Improving UHC js](http://www.norm2782.com/improving-uhc-js-report.pdf).

```haskell
import UHC.Ptr
foreign import js "wrapper" mkCb :: IO () -> IO (FunPtr (IO ())
```

This converts an IO action to a function pointer that can be passed to javascript.

If we want our callback to have different or more arguments, we have to import wrapper with a different signature. Here we create callbacks that we can pass as key event handlers.

```haskell
data JSKeyEvent
foreign import js "wrapper"
    mkKeyEventCb :: (JSKeyEvent -> IO ()) -> IO (FunPtr (JSKeyEvent -> IO ()))
```

We now need to import the functions with which we set the callbacks. The interval function is set with "setInterval" while "onLoad" and the key event callbacks can be set with "addEventListener".

For these functions we define simplified version that take care of creating the the callback for us.

```haskell
foreign import js "%1.keyCode"
  keyCode :: JSKeyEvent -> IO Int


foreign import js "window.addEventListener('keydown',%1,true)"
  jsSetOnKeyDown :: FunPtr (JSKeyEvent -> IO ()) -> IO ()
setOnKeyDown (Int -> IO ()) -> IO ()
setOnKeyDown fp = dp
  cb <- mkKeyEventCb fp'
  jsSetOnKeyDown cb
  where
    fp' event = keyCode event >>= fp

foreign import js "window.addEventListener('keyup',%1,true)"
  jsSetOnKeyUp :: FunPtr (JSKeyEvent -> IO ()) -> IO ()

setOnKeyDown (Int -> IO ()) -> IO ()
setOnKeyDown fp = dp
  cb <- mkKeyEventCb fp'
  jsSetOnKeyUp cb
  where
    fp' event = keyCode event >>= fp

foreign import js "window.addEventListener('load', %1, 'false')"
  jsSetOnLoad :: FunPtr (IO ()) -> IO ()
setOnLoad :: IO () -> IO ()
setOnLoad fp = mkCb fp >>= setOnLoad_

foreign import js "setInterval(%1,%2)"
  jsSetInterval :: FunPtr (IO ()) -> Int -> IO ()
setInterval :: IO () -> Int -> IO ()
setInterval fp time = mkCb fp >>= (jsSetInterval time)
```

Remember than the >>= operator chains monadic actions.
setKey\[Down|Up\] define wrapper functions hat extract the keycode and passes it to our callback functions because that is the information we are really interested in.

We will follow the convention, that functions taking javascript specific parameters (such as JSString) will be prefixed by "js" and have corresponding functions without the "js" prefix.

## Haste

Setting callbacks in haste is a little different.

For every callback function a javascript function has to be created which invokes the special function "A()" with out callback. This function can than be used for the callback. Read the section "Callbacks" of [js-externls.txt](https://github.com/valderman/haste-compiler/blob/master/doc/js-externals.txt) in the doc subdirectory of the [haste github repository](https://github.com/valderman/haste-compiler).

The arguments for the haskell callback are the second argument of "A()" and have to be passed as a list similar to the required return value of javascript function included by the FFI. Again, this is explained in [js-externals.txt](https://github.com/valderman/haste-compiler/blob/master/doc/js-externals.txt).

```javascript
function jsSetInterval(msecs, cb, _) {
	window.setInterval(function() {A(cb,[0]);}, msecs);
	return [1,0];
}

function jsSetOnKeyDown(cb, _) {
	window.addEventListener('keydown', function(e) {A(cb,[[1,parseInt(e.keyCode)],0]);}, true);
	return [1,0];
}

function jsSetOnKeyUp(cb, _) {
	window.addEventListener('keyup', function(e) {A(cb,[[1,parseInt(e.keyCode)],0]);}, true);
	return [1,0];
}
```

As you can see, out versions jsSetOnKey\[Up|Down\] do not pass the key event itself but the extracted keycode. On the haskell callbacks have to be created with "mkCallback" and have the Type "Callback a".

```haskell
foreign import ccall jsSetInterval :: Int -> Callback a -> IO ()
setInterval :: Int -> IO () -> IO ()
setInterval time cb =
  jsSetInterval time (mkCallback $! cb)

foreign import ccall jsSetOnKeyDown :: Callback a -> IO ()
foreign import ccall jsSetOnKeyUp :: Callback a -> IO ()

setOnKeyDown :: (Int -> IO ()) -> IO ()
setOnKeyDown cb =
  jsSetOnKeyDown (mkCallback $! cb)
setOnKeyUp :: (Int -> IO ()) -> IO ()
setOnKeyUp cb =
  jsSetOnKeyUp (mkCallback $! cb)
```

# Letting callbacks communicate

## UHC

In a javascript program we would let the onKeyUp, onKeyDown and Interval functions communicate through global variables. In haskell we do not have a mechanism such as global variables (at least non that I am aware of). In a normal situation we do need it, because the only moment when main exits is, when the program ends.

To store global variables we write a few helper functions in javascript:

```javascript
var allObjects = {}

function saveObject(name, obj) {
 allObjects[name] = obj;
}

function loadObject(name) {
 return allObjects[name];
}
```

And include them from haskell:

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

We can now load the current state with

```haskell
state <- loadObject "state" :: IO State
```

when we enter one of our callback functions and save it with a corresponing call to "saveObject".

## Haste

For haste the mechanism works the same way, only that the javascript helper functions have to format their return values as haste needs them:

```javascript
var allObjects = {}

function jsSaveGlobalObject(name, obj) {
 allObjects[name] = obj;
 return [1,0];
}

function jsLoadGlobalObject(name) {
 return [1,0,allObjects[name]];
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

Example code for drawing on the canvas in javascript looks like this:</div>

```javascript
context = document.getElementById("canvas").getContext("2d");<br />
context.clearRect(0.0, 0.0, context.canvas.width, context.canvas.height)</div>
context.setFillColor(0.0, 1.0, 0.0, 1.0);</div>
context.fillRect(10.0,10.0,100.0,100.0);</div>
```

Basically all we have to do is import these functions via the FFI.

## UHC

Getting the context needs several steps:

* Get the document
* Get the canvas via getElementById
* Get the context via getContext

So this is what we do:

```haskell
data Document
data Context2D
data Canvas

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
  getContext2dFromCanvas
```

Importing the rest of the functions is straight forward:</div>

```haskell
foreign import js "%1.fillRect(%*)"
  fillRect :: Context2D -> Double -> Double -> Double -> Double -> IO ()
foreign import js "%1.setFillColor(%*)"
  setFillColor :: Context2D -> Double -> Double -> Double -> Double -> IO ()
```

## Haste

Again, for haste we have to write javascript functions with the correct return type:

```javascript
function jsGetContext2d(canvas, _) {
 return [1,0,canvas.getContext("2d")];
}

function jsFillRect(context, x, y, width, height, _) {
 context.fillRect(x,y,width,height);
 return [1,0];
}

function jsSetFillColor(context, color, _) {
 context.setFillColor(color);
 return [1,0];
}

function jsClear(context, _) {
 context.clearRect(0.0, 0.0, context.canvas.width, context.canvas.height);
        return [1,0]
}
```

And here the haskell part:</div>

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

Here we use the "withElem" function from Haskell.DOM which executes an action with the element specified by the provided name.

# Putting it all together

All imported functions are named, so that the main code is the same for UHC and haste. The folloging code should explain itself through the comments:

To compile it with UHC, you need JavaScript.hs und helpers.js. Than run

```bash
uhc -tjs Main.hs
```

Edit the resulting html page and add

```html
<script type="text/javascript" src="helpers.js"></script>
```

into the head and

```
<canvas id="canvas1" width=600 height=500></canvas>
```

to the body.

For haste you need this JavaScript.hs und helpers.js and compile it with:

```bash
hastec --with-js=helpers.js Main.hs</div>
```

Than embed it in this html page:

## Summary

This has been a rather long post, but we have learned a lot. With the tools at hand we can now update out game with an callback function we set via "setInterval" and receive keyboard events.
The next post will be another step towards our goal of a breakout clone, we will implement the ball and let it bounce on the paddle and the walls.
