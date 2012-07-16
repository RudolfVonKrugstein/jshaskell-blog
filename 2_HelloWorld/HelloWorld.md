The first thing we need on our journey is a compiler that translates out haskell to javascript. On this page: [The JavaScript Problem](http://www.haskell.org/haskellwiki/The_JavaScript_Problem) you can find a list of compilers that are able to do that. I will focus on UHC and haste. GHCJS also seems promising, and I might try it later.

So we want to write a little hello world program. The equivalent in javascript would be

```Javascript
document.write("Hello, World!");
```

which replaces the contents of the document with "Hello, World!". Let us get started.

# UHC
The [Utrecht Haskell Compiler](http://www.cs.uu.nl/wiki/UHC/) has a backend allowing to compile haskell to javascript. This page links all the information about the JavaScript backend: [UHC-JS](http://uu-computerscience.github.com/uhc-js/)

## Installing UHC

First we have to install UHC. You can find the code for UHC here [UHC GitHub](https://github.com/UU-ComputerScience/uhc). The build instructions can be found in the EHC sub directory.

But first some dependencies are needed. On ubuntu linux I install them with apt-get:

```bash
apt-get install ghc cabal-install build-essentials libtool uuagc
```

We also need to install a few haskell packages via cabal:

```bash
cabal update
cabal install network uulib syb fgl
```

To install UHC, first clone the repository and change into the EHC directory. Then build and install UHC.

```bash
git clone git://github.com/UU-ComputerScience/uhc.git
cd uhc/EHC
./configure
make
sudo make install
```

In blogs it is common to suggesting getting a cup of coffee at a moment like this, because the make command may take a while. So get a cup of coffee!

If everthing worked out, UHC should be installed and you can compile to javascript via

```bash
uhc -tjs Main.hs
```

(Or however your haskell file is called).


## Hello World with UHC

We need to do two things:

* Get the document.
* Call write on the document with "Hello, World!" as parameter.

The FFI (ForeignFunctionInterface) of the js backend of UHC is described here [Improving UHC js](http://www.norm2782.com/improving-uhc-js-report.pdf)
For our purposes, it works like this:

```haskell
foreign import js "jscommand" haskellName :: Type
```

Where "jscommand" is the command in javascript, "haskellName" the name of the haskell function we want to define and "Type" the type of the haskell function. "jscommand" may contain "%N" where N is a number refering to the N-th parameter of the haskell function.

So to get the document we first define a type for it and then import a corresponding javascript command

```haskell
data Document
foreign import js "document" getDocument :: IO Document
```

To get the document we just have to call "document" in javascript. This returns us the document in the IO monad. The document is in the IO monad because we a calling a foreign function which might have side effects.

Now we want to call "write" on a document.

```haskell
foreign import js "%1.write(%2)" write :: Document -> JSString -> IO ()
```

Because the "%1" is in front of the ".write", the first argument to haskell function "write" (which is the document) is the object write is called on (this can all be found in [Improving UHC js](http://www.norm2782.com/improving-uhc-js-report.pdf).

Note that the second argument is of type JSString and not of String. This is because a string in haskell is not the same as a string in javascript. We have to convert a haskell string to a javascript string

```haskell
type JSString = PackedString
foreign import prim "primStringToPackedString" toJS :: String -> JSString
```

Now we are ready to write our hello world:

```haskell
type JSString = PackedString
foreign import prim "primStringToPackedString" toJS :: String -> JSString

data Document
foreign import js "document" getDocument :: IO Document
foreign import js "%1.write(%2)" write :: Document -> JSString -> IO ()

main = do
  doc <- getDocument
  write doc $ toJS "Hello, World!"
```

# Haste

## Installing haste

Haste can be found here: [Haste GitHub](https://github.com/valderman/haste-compiler). Instructions for installation can also be found on that page (just read the Building section). It requires installation of fursuit, which can be found here: [Fursuit GitHub](https://github.com/valderman/fursuit).

## Hello World with haste

Again, we need a way to import a function to get the document and to write into the contents of the document. Information on how to import javascript functions can be found in <a [the doc subdirectory](https://github.com/valderman/haste-compiler/blob/master/doc/js-externals.txt) of the haste repository.

Haste is not as flexible as UHC when importing JavaScript functions. It does not allow placing the parameters of the haskell function in the javascript code with "%N". It also does not allow the custom type "Document" to be used as a parameter or return type. Instead "JSAny" must be used. Also the return type of the JavaScript functions must be

```Javascript
[1,anything,actual_return_object]
```

Last but not least, they must take an extra parameter for the "state of the world" (nothing has to be done with it, it just has to exist).
So we create a file "helpers.hs" with out Javascript helper functions:

```Javascript
function objWrite(obj, text, _realWorld) {
    obj.write(text); // Call the method
    return [1,0]; // Return ()
}

function getDocument(_realWorld) {
    return [1, 0, document];
}
```

This now allows us to write hello world in Haste:

```haskell
import Haste
import Haste.Prim

type Document = JSAny

foreign import ccall "objWrite" write :: Document -> JSString -> IO ()
foreign import ccall "getDocument" getDocument :: IO Document

main = do
  doc <- getDocument
  write doc $ toJSStr "Hello World!"
```

Compile this with:

```bash
hastec Main.hs --with-js=helpers.js
```

Now all we need to do is embed this file in a html page:

```html
<!DOCTYPE html><html><head><title>Main</title>
<script type="text/javascript" src="Main.js"></script>
</head>
<body>
</body>
</html>
```

Open this with a browser of your choice (I only tried chromium) and it should work.

# *Edit:* Trying it out

As suggested in a comment, I have uploaded the compiled javascript file (the haste version because the UHC version has several dependencies) here: [Hello, World!](https://sites.google.com/site/mindlettice/home/files/HelloWorld.js?attredirects=0&amp;d=1)

Download it and [this](https://sites.google.com/site/mindlettice/home/files/HelloWorld.html?attredirects=0&amp;d=1) html file into the same directory. Than open the html with a browser of your choice (tested on ubuntu linux, chromium).

Edit: Rewrote with pandoc
