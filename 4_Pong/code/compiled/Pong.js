/* Eval
   Evaluate the given thunk t into head normal form.
   If the "thunk" we get isn't actually a thunk, just return it.
*/
function E(t) {
    if(t instanceof Thunk) {
        if(t.f) {
            t.x = t.f();
            t.f = 0;
        }
        return t.x;
    }
    return t;
}

/* Thunk
   Creates a thunk representing the given closure.
   Since we want automatic memoization of as many expressions as possible, we
   use a JS object as a sort of tagged pointer, where the member x denotes the
   object actually pointed to. If a "pointer" points to a thunk, it has a
   member 't' which is set to true; if it points to a value, be it a function,
   a value of an algebraic type of a primitive value, it has no member 't'.

   When a thunk is evaluated, by reading the member 'x' of the "pointer," the
   closure is evaluated and the getter removed, to be replaced with the value
   returned by the thunk, and the getter finally returns the return value of
   the closure.
*/

function T(f) {
    return new Thunk(f);
}

function Thunk(f) {
    this.f = f;
}

/* Integer literal
   Generates an Integer literal from a Number.
   This might be dependent on using integer-simple for Integers.
*/
function I(n) {
    if(n > 0) {
        return [1,[1, n, 2]];
    } else if(n < 0) {
        return [2,[1,n,2]];
    } else {
        return [3]
    }
}

/* Apply
   Applies the function f to the arguments args. If the application is under-
   saturated, a closure is returned, awaiting further arguments. If it is over-
   saturated, the function is fully applied, and the result (assumed to be a
   function) is then applied to the remaining arguments.
*/
function A(f, args) {
    f = f instanceof Thunk ? E(f) : f;
    // Closure does some funny stuff with functions that occasionally
    // results in non-functions getting applied, so we have to deal with
    // it.
    if(!f.apply) {
        return f;
    }

    var arity = f.arity ? f.arity : f.length;
    if(args.length === arity) {
        return f.apply(null, args);
    }
    if(args.length > arity) {
        var first = args.splice(0, arity);
        return A(f.apply(null, first), args);
    } else {
        var g = function() {
            var as = args.concat(Array.prototype.slice.call(arguments));
            return A(f, as);
        };
        g.arity = arity - args.length;
        return g;
    }
}

/* Throw an error.
   We need to be able to use throw as an exception so we wrap it in a function.
*/
function die(err) {
    throw err;
}

function quot(a, b) {
    return (a-a%b)/b;
}

// 32 bit integer multiplication, with correct overflow behavior
// note that |0 or >>>0 needs to be applied to the result, for int and word
// respectively.
function imul(a, b) {
  // ignore high a * high a as the result will always be truncated
  var lows = (a & 0xffff) * (b & 0xffff); // low a * low b
  var aB = (a & 0xffff) * (b & 0xffff0000); // low a * high b
  var bA = (a & 0xffff0000) * (b & 0xffff); // low b * high a
  return lows + aB + bA; // sum will not exceed 52 bits, so it's safe
}

function addC(a, b) {
    var x = a+b;
    return [1, x & 0xffffffff, x > 0x7fffffff];
}

function subC(a, b) {
    var x = a-b;
    return [1, x & 0xffffffff, x < -2147483648];
}

function sinh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / 2;
}

function tanh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / (Math.exp(arg) + Math.exp(-arg));
}

function cosh (arg) {
    return (Math.exp(arg) + Math.exp(-arg)) / 2;
}

function log2(x) {
    var high = 1024;
    var low = -1024;
    var i = 0;
    var x2;
    for(;;) {
        x2 = Math.pow(2, i);
        if(x2 <= (x >> 1)) {
            low = i;
            i += (high - i) >> 1;
        } else if(x2 > x) {
            high = i;
            i += (low - i) >> 1;
        } else {
            return i;
        }
    }
    return i;
}

function decodeFloat(x) {
    if(isNaN(x)) {
        return [1, -6755399441055744, 972];
    }
    var sig = x > 0 ? 1 : -1;
    if(!isFinite(x)) {
        return [1, sig * 4503599627370496, 972];
    }
    x = Math.abs(x);
    var exp = log2(x)-52;
    var man = x/Math.pow(2, exp);
    return [1, sig*man, exp];
}

function decodeDouble(x) {
    var decoded = decodeFloat(x);
    var sign = decoded[1] < 0 ? -1 : 1;
    var mantissa = decoded[1]*sign;
    var manLow = mantissa % 0x100000000;
    var manHigh = Math.floor(mantissa / 0x100000000);
    return [1, sign, manHigh, manLow, decoded[2]];
}

function newArr(n, x) {
    var arr = [];
    for(; n >= 0; --n) {
        arr.push(x);
    }
    // Use 0 for the never-examined state argument.
    return [1, 0, arr];
}

function err(str) {
    die(toJSStr(str)[1]);
}

/* unpackCString#
   NOTE: update constructor tags if the code generator starts munging them.
*/
function unCStr(str) {
    return unAppCStr(str, [1]);
}

function unAppCStr(str, chrs) {
    var i = arguments[2] ? arguments[2] : 0;
    if(i >= str.length) {
        return E(chrs);
    } else {
        return [2,[1,str.charAt(i)],T(function() {
            return unAppCStr(str,chrs,i+1);
        })];
    }
}

function fromJSStr(str) {
    return unCStr(E(str)[1]);
}

function toJSStr(str) {
    str = E(str);
    var s = '';
    while(str[0] == 2) {
        var cs = readHSUnicodeChar(str);
        s += cs[0];
        str = cs[1];
    }
    return [1,s];
}

function readHSUnicodeChar(str) {
    var c = E(str[1])[1];
    // If we get slashes, read all numbers we encounter.
    if(c == '\\') {
        var num = '';
        str = E(str[2]);
        if(str == 1) {
            return ['\\', str];
        }
        c = E(str[1])[1];
        while(c >= '0' && c <= '9') {
            num += c;
            str = E(str[2]);
            c = E(str[1])[1];
        }
        if(num.length == 0) {
            return ['\\', str];
        }
        c = String.fromCharCode(Number(num));
        return [c, str];
    } else {
        return [c, E(str[2])];
    }
}

// newMutVar
function nMV(val, st) {
    return [1,st,{x: val}];
}

// readMutVar
function rMV(mv, st) {
    return [1,st,mv.x];
}

// writeMutVar
function wMV(mv, val, st) {
    mv.x = val;
    return [1,st];
}

function localeEncoding(theWorld) {
    return [1,theWorld,'UTF-8'];
}

// every newSomethingSomethingByteArray
function newBA(size, theWorld) {
    var s = '';
    while(size >= 0) {
        s += '';
        --size;
    }
    return [1,theWorld,s];
}

function wOffAddr(addr, off, val, theWorld) {
    addr[off] = val;
    return theWorld;
}

function isDoubleNaN(d,_) {
    return [1,0,isNaN(d)];
}
var isFloatNaN = isDoubleNaN;

function isDoubleInfinite(d,_) {
    return [1,0,d === Infinity];
}
var isFloatInfinite = isDoubleInfinite;

function isDoubleNegativeZero(x,_) {
    return [1,0,x===0 && (1/x)===-Infinity];
}
var isFloatNegativeZero = isDoubleNegativeZero;

function strEq(a, b, _) {
    return [1, 0, a == b];
}

function strOrd(a, b, _) {
    var ord;
    if(a < b) {
        ord = [1];
    } else if(a == b) {
        ord = [2];
    } else {
        ord = [3];
    }
    return [1, 0, [1, ord]];
}

function jsCatch(act, handler, _) {
    try {
        return [1,0,A(act,[0])[2]];
    } catch(e) {
        return [1,0,A(handler,[e,0])[2]];
    }
}

function hs_eqWord64(a, b, _) {
    return [1,0,a==b];
}

var realWorld = 0;
var coercionToken = undefined;

/* Haste represents constructors internally using 1 for the first constructor,
   2 for the second, etc.
   However, dataToTag should use 0, 1, 2, etc. Also, booleans might be unboxed.
 */
function dataToTag(x) {
    if(x instanceof Array) {
        return x[0]-1;
    } else {
        return x-1;
    }
}

function jsAlert(val,_) {
    if(typeof alert != 'undefined') {
        alert(val);
    } else {
        print(val);
    }
    return [1,0];
}

function jsLog(val,_) {
    console.log(val);
    return [1,0];
}

function jsPrompt(str,_) {
    var val;
    if(typeof prompt != 'undefined') {
        val = prompt(str);
    } else {
        print(str);
        val = readline();
    }
    return [1,0,val == undefined ? '' : val.toString()];
}

function jsEval(str,_) {
    var x = eval(str);
    return [1,0,x == undefined ? '' : x.toString()];
}

function isNull(obj,_) {
    return [1,0,[obj === null]];
}

function jsRead(str,_) {
    return [1,0,Number(str)];
}

function jsShowI(val, _) {return [1,0,val.toString()];}
function jsShow(val, _) {
    var ret = val.toString();
    return [1,0,val == Math.round(val) ? ret + '.0' : ret];
}

function jsSetCB(elem, evt, cb, _) {
    // Count return press in single line text box as a change event.
    if(evt == 'change' && elem.type.toLowerCase() == 'text') {
        setCB(elem, 'keyup', function(k) {
            if(k == '\n') {
                A(cb,[[1,k.keyCode], 0]);
            }
        });
    }

    var fun;
    switch(evt) {
    case 'click':
    case 'dblclick':
    case 'mouseup':
    case 'mousedown':
        fun = function(x) {A(cb,[[1,x.button], 0]);};
        break;
    case 'keypress':
    case 'keyup':
    case 'keydown':
        fun = function(x) {A(cb,[[1,x.keyCode], 0]);};
        break;        
    default:
        fun = function() {A(cb,[0]);};
        break;
    }
    return setCB(elem, evt, fun);
}

function setCB(elem, evt, cb) {
    if(elem.addEventListener) {
        elem.addEventListener(evt, cb, false);
        return [1,0,true];
    } else if(elem.attachEvent) {
        elem.attachEvent('on'+evt, cb);
        return [1,0,true];
    }
    return [1,0,false];
}

function jsSetTimeout(msecs, cb, _) {
    window.setTimeout(function() {A(cb,[0]);}, msecs);
    return [1,0];
}

// Round a Float/Double.
function rintDouble(d, _) {
    return [1,0,Math.round(d)];
}
var rintFloat = rintDouble;

// Degenerate versions of u_iswspace, u_iswalnum and u_iswalpha.
function u_iswspace(c, _) {
    return [1,0, c==9 || c==10 || c==13 || c==32];
}

function u_iswalnum(c, _) {
    return [1,0, (c >= 48 && c <= 57) || u_iswalpha(c)[0]];
}

// [a-zA-ZåäöÅÄÖ]
function u_iswalpha(c, _) {
    return [1,0, (c >= 65 && c <= 90) || (c >= 97 && c <= 122) ||
                  c == 229 || c == 228 || c == 246 ||
                  c == 197 || c == 196 || c == 214];
}

function jsGet(elem, prop, _) {
    return [1,0,elem[prop].toString()];
}

function jsSet(elem, prop, val, _) {
    elem[prop] = val;
    return [1,0];
}

function jsGetStyle(elem, prop, _) {
    return [1,0,elem.style[prop].toString()];
}

function jsSetStyle(elem, prop, val, _) {
    elem.style[prop] = val;
    return [1,0];
}

function jsKillChild(child, parent, _) {
    parent.removeChild(child);
    return [1,0];
}

function jsClearChildren(elem, _) {
    while(elem.hasChildNodes()){
        elem.removeChild(elem.lastChild);
    }
    return [1,0];
}

function jsFind(elem, _) {
    var e = document.getElementById(elem)
    if(e) {
        return [1,0,[2,[1,e]]];
    }
    return [1,0,[1]];
}

function jsCreateElem(tag, _) {
    return [1,0,document.createElement(tag)];
}

function jsGetChildBefore(elem, _) {
    elem = elem.previousSibling;
    while(elem) {
        if(typeof elem.tagName != 'undefined') {
            return [1,0,[2,[1,elem]]];
        }
        elem = elem.previousSibling;
    }
    return [1,0,[1]];
}

function jsGetLastChild(elem, _) {
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,0,[2,[1,elem.childNodes[i]]]];
        }
    }
    return [1,0,[1]];
}

function jsGetChildren(elem, _) {
    var children = [1];
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            children = [2, [1,elem.childNodes[i]], children];
        }
    }
    return [1,0,children];
}

function jsSetChildren(elem, children, _) {
    children = E(children);
    jsClearChildren(elem, 0);
    while(children[0] === 2) {
        elem.appendChild(E(E(children[1])[1]));
        children = E(children[2]);
    }
    return [1,0];
}

function jsAppendChild(child, container, _) {
    container.appendChild(child);
    return [1,0];
}

function jsAddChildBefore(child, container, after, _) {
    container.insertBefore(child, after);
    return [1,0];
}

function jsRand(_) {
    return [1,0,Math.random()];
}

// Concatenate a Haskell list of JS strings
function jsCat(strs, sep, _) {
    var arr = [];
    strs = E(strs);
    while(strs[0] != 1) {
        strs = E(strs);
        arr.push(E(strs[1])[1]);
        strs = E(strs[2]);
    }
    return [1,0,arr.join(sep)];
}

// Escape all double quotes in a string
function jsUnquote(str, _) {
    return [1,0,str.replace(/"/, '\\"')];
}

// Parse a JSON message into a Haste.JSON.JSON value.
// As this pokes around inside Haskell values, it'll need to be updated if:
// * Haste.JSON.JSON changes;
// * E() starts to choke on non-thunks;
// * data constructor code generation changes; or
// * Just and Nothing change tags.
function jsParseJSON(str, _) {
    try {
        var js = JSON.parse(str);
        var hs = toHS(js);
    } catch(_) {
        return [1,0,[1]];
    }
    return [1,0,[2,hs]];
}

function toHS(obj) {
    switch(typeof obj) {
    case 'number':
        return [1, [1, jsRead(obj)[2]]];
    case 'string':
        return [2, [1, obj]];
        break;
    case 'boolean':
        return [3, obj]; // Booleans are special wrt constructor tags!
        break;
    case 'object':
        if(obj instanceof Array) {
            return [4, arr2lst(obj, 0)];
        } else {
            // Object type but not array - it's a dictionary.
            // The RFC doesn't say anything about the ordering of keys, but
            // considering that lots of people rely on keys being "in order" as
            // defined by "the same way someone put them in at the other end,"
            // it's probably a good idea to put some cycles into meeting their
            // misguided expectations.
            var ks = [];
            for(var k in obj) {
                ks.unshift(k);
            }
            var xs = [1];
            for(var i in ks) {
                xs = [2, [1, [1,ks[i]], toHS(obj[ks[i]])], xs];
            }
            return [5, xs];
        }
    }
}

function arr2lst(arr, elem) {
    if(elem >= arr.length) {
        return [1];
    }
    return [2, toHS(arr[elem]), T(function() {return arr2lst(arr,elem+1);})]
}

function ajaxReq(method, url, async, postdata, cb, _) {
    var xhr = new XMLHttpRequest();
    xhr.open(method, url, async);
    xhr.setRequestHeader('Cache-control', 'no-cache');
    xhr.onreadystatechange = function() {
        if(xhr.readyState == 4) {
            if(xhr.status == 200) {
                A(cb,[[1,xhr.responseText],0]);
            } else {
                A(cb,[[1,""],0]); // Nothing
            }
        }
    }
    xhr.send(postdata);
    return [1,0];
}

function u_towlower(charCode, _) {
    return [1, 0, String.fromCharCode(charCode).toLowerCase().charCodeAt(0)];
}

function u_towupper(charCode, _) {
    return [1, 0, String.fromCharCode(charCode).toUpperCase().charCodeAt(0)];
}

// MVar implementation.
// Since Haste isn't concurrent, takeMVar and putMVar don't block on empty
// and full MVars respectively, but terminate the program since they would
// otherwise be blocking forever.

function newMVar(st) {
    return [1, st, {empty: true}];
}

function tryTakeMVar(mv, st) {
    if(mv.empty) {
        return [1, st, 0, undefined];
    } else {
        mv.empty = true;
        mv.x = null;
        return [1, st, 1, mv.x];
    }
}

function takeMVar(mv, st) {
    if(mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to take empty MVar!");
    }
    mv.empty = true;
    mv.x = null;
    return [1,st,mv.x];
}

function putMVar(mv, val, st) {
    if(!mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to put full MVar!");
    }
    mv.empty = false;
    mv.x = val;
    return [1,st];
}

function tryPutMVar(mv, val, st) {
    if(!mv.empty) {
        return [1, st, 0];
    } else {
        mv.empty = false;
        mv.x = val;
        return [1, st, 1];
    }
}

function sameMVar(a, b) {
    return (a == b);
}

function isEmptyMVar(mv, st) {
    return [1, st, mv.empty ? 1 : 0];
}

function jsGetContext2D(canvas, _) {
	return [1,0,canvas.getContext("2d")];
}

function jsBeginPath(context, _) {
	context.beginPath();
	return [1,0];
}

function jsClosePath(context, _) {
	context.closePath();
	return [1,0];
}

function jsFill(context, _) {
	context.fill();
	return [1,0];
}

function jsArc(context, x, y, r, minPhi, maxPhi, ccw, _) {
	context.arc(x,y,r,minPhi,maxPhi,ccw);
	return [1,0];
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

function jsSetInterval(msecs, cb, _) {
	window.setInterval(function() {A(cb,[0]);}, msecs);
	return [1,0];
}

function jsSetOnLoad(cb, _) {
	window.addEventListener('load', function() {A(cb,[0]);}, false);
	return [1,0];
}

var allObjects = {}

function jsSaveGlobalObject(name, obj) {
	allObjects[name] = obj;
	return [1,0];
}

function jsLoadGlobalObject(name) {
	return [1,0,allObjects[name]];
}


var _0 = [1];var _1 = function(_2){return [1,_2];};var _3 = function(_4,_5){var _6 = E(_5);var _7 = A(_4,[_6]);return _7;};var _8 = function(_9,_a){var _b = A(_3,[_1,_9]);var _c = _b[1];var _d = jsSetOnLoad(_c,_a);var _e = _d[1];var _f = [1,_e,_0];return _f;};var _g = function(_h,_i){return _8(_h,_i);};var _j = function(_k,_l,_m){var _n = A(_k,[_m]);var _o = _n[1];var _p = A(_l,[_o]);return _p;};var _q = function(_r,_s,_t){return _j(_r,_s,_t);};var _u = function(_v,_w,_x){var _y = A(_v,[_x]);var _z = _y[1];var _A = _y[2];var _B = A(_w,[_A,_z]);return _B;};var _C = function(_D,_E){return [1,_E,_D];};var _F = T(function(){return unCStr("Maybe.fromJust: Nothing");});var _G = T(function(){return err(_F);});var _H = function(_I,_J,_K){var _L = T(function(){var _M = A(_I,[_K]);var _N = _M[1];var _O = _M[2];var _P = T(function(){var _Q = E(_L);if(_Q[0]==1){var _R = E(_G);}else{var _S = _Q[1];var _R = E(_S);}return _R;});var _T = A(_J,[_P]);var _U = _T[1];var _V = _T[2];var _W = hs_eqWord64(_N,_U,realWorld);var _X = _W[2];var _Y = E(_X);if(_Y){var _Z = hs_eqWord64(_O,_V,realWorld);var _10 = _Z[2];var _11 = E(_10);var _12 = _11?[2,_K]:[1];var _13 = _12;}else{var _13 = [1];}return _13;});return E(_L);};var _14 = function(_15){var _16 = E(_15);var _17 = _16[1];var _18 = E(_17);return _18;};var _19 = T(function(){return unCStr("base");});var _1a = T(function(){return unCStr("GHC.IO.Exception");});var _1b = T(function(){return unCStr("IOException");});var _1c = [1,7238999624334008320,1.0769272474234763e19,_19,_1a,_1b];var _1d = [1];var _1e = [1,7238999624334008320,1.0769272474234763e19,_1c,_1d];var _1f = function(_1g){return E(_1e);};var _1h = function(_1i){var _1j = E(_1i);var _1k = _1j[1];var _1l = _1j[2];var _1m = _14(_1k);var _1n = _H(_1m,_1f,_1l);return _1n;};var _1o = function(_1p,_1q){var _1r = E(_1p);if(_1r[0]==1){var _1s = E(_1q);}else{var _1t = _1r[1];var _1u = _1r[2];var _1v = T(function(){return _1o(_1u,_1q);});var _1s = [2,_1t,_1v];}return _1s;};var _1w = T(function(){return unCStr(": ");});var _1x = T(function(){return unCStr("already exists");});var _1y = T(function(){return unCStr("does not exist");});var _1z = T(function(){return unCStr("protocol error");});var _1A = T(function(){return unCStr("failed");});var _1B = T(function(){return unCStr("invalid argument");});var _1C = T(function(){return unCStr("inappropriate type");});var _1D = T(function(){return unCStr("hardware fault");});var _1E = T(function(){return unCStr("unsupported operation");});var _1F = T(function(){return unCStr("timeout");});var _1G = T(function(){return unCStr("resource vanished");});var _1H = T(function(){return unCStr("interrupted");});var _1I = T(function(){return unCStr("resource busy");});var _1J = T(function(){return unCStr("resource exhausted");});var _1K = T(function(){return unCStr("end of file");});var _1L = T(function(){return unCStr("illegal operation");});var _1M = T(function(){return unCStr("permission denied");});var _1N = T(function(){return unCStr("user error");});var _1O = T(function(){return unCStr("unsatisified constraints");});var _1P = T(function(){return unCStr("system error");});var _1Q = function(_1R,_1S){var _1T = E(_1R);switch(_1T[0]){case 1:var _1U = _1o(_1x,_1S);break;case 2:var _1U = _1o(_1y,_1S);break;case 3:var _1U = _1o(_1I,_1S);break;case 4:var _1U = _1o(_1J,_1S);break;case 5:var _1U = _1o(_1K,_1S);break;case 6:var _1U = _1o(_1L,_1S);break;case 7:var _1U = _1o(_1M,_1S);break;case 8:var _1U = _1o(_1N,_1S);break;case 9:var _1U = _1o(_1O,_1S);break;case 10:var _1U = _1o(_1P,_1S);break;case 11:var _1U = _1o(_1z,_1S);break;case 12:var _1U = _1o(_1A,_1S);break;case 13:var _1U = _1o(_1B,_1S);break;case 14:var _1U = _1o(_1C,_1S);break;case 15:var _1U = _1o(_1D,_1S);break;case 16:var _1U = _1o(_1E,_1S);break;case 17:var _1U = _1o(_1F,_1S);break;case 18:var _1U = _1o(_1G,_1S);break;case 19:var _1U = _1o(_1H,_1S);break;}return _1U;};var _1V = T(function(){return unCStr(" (");});var _1W = [1,')'];var _1X = [1,'}'];var _1Y = T(function(){return unCStr("{handle: ");});var _1Z = function(_20,_21,_22,_23,_24,_25){var _26 = T(function(){var _27 = T(function(){var _28 = T(function(){var _29 = E(_23);if(_29[0]==1){var _2a = E(_25);}else{var _2b = T(function(){var _2c = [2,_1W,_25];return _1o(_29,_2c);});var _2a = _1o(_1V,_2b);}return _2a;});return _1Q(_21,_28);});var _2d = E(_22);if(_2d[0]==1){var _2e = E(_27);}else{var _2f = T(function(){return _1o(_1w,_27);});var _2e = _1o(_2d,_2f);}return _2e;});var _2g = E(_24);if(_2g[0]==1){var _2h = E(_20);if(_2h[0]==1){var _2i = E(_26);}else{var _2j = _2h[1];var _2k = E(_2j);if(_2k[0]==1){var _2l = _2k[1];var _2m = T(function(){var _2n = T(function(){return _1o(_1w,_26);});var _2o = [2,_1X,_2n];return _1o(_2l,_2o);});var _2p = _1o(_1Y,_2m);}else{var _2q = _2k[1];var _2r = T(function(){var _2s = T(function(){return _1o(_1w,_26);});var _2t = [2,_1X,_2s];return _1o(_2q,_2t);});var _2p = _1o(_1Y,_2r);}var _2i = _2p;}var _2u = _2i;}else{var _2v = _2g[1];var _2w = T(function(){return _1o(_1w,_26);});var _2u = _1o(_2v,_2w);}return _2u;};var _2x = function(_2y){var _2z = E(_2y);var _2A = _2z[1];var _2B = _2z[2];var _2C = _2z[3];var _2D = _2z[4];var _2E = _2z[6];var _2F = _1Z(_2A,_2B,_2C,_2D,_2E,_1d);return _2F;};var _2G = [1,','];var _2H = [1,']'];var _2I = [1,'['];var _2J = function(_2K,_2L){var _2M = E(_2K);if(_2M[0]==1){var _2N = unAppCStr("[]",_2L);}else{var _2O = _2M[1];var _2P = _2M[2];var _2Q = T(function(){var _2R = E(_2O);var _2S = _2R[1];var _2T = _2R[2];var _2U = _2R[3];var _2V = _2R[4];var _2W = _2R[6];var _2X = T(function(){var _2Y = [2,_2H,_2L];var _2Z = function(_30){var _31 = E(_30);if(_31[0]==1){var _32 = E(_2Y);}else{var _33 = _31[1];var _34 = _31[2];var _35 = T(function(){var _36 = E(_33);var _37 = _36[1];var _38 = _36[2];var _39 = _36[3];var _3a = _36[4];var _3b = _36[6];var _3c = T(function(){return _2Z(_34);});var _3d = _1Z(_37,_38,_39,_3a,_3b,_3c);return _3d;});var _32 = [2,_2G,_35];}return _32;};return _2Z(_2P);});var _3e = _1Z(_2S,_2T,_2U,_2V,_2W,_2X);return _3e;});var _2N = [2,_2I,_2Q];}return _2N;};var _3f = function(_3g,_3h,_3i){var _3j = E(_3h);var _3k = _3j[1];var _3l = _3j[2];var _3m = _3j[3];var _3n = _3j[4];var _3o = _3j[6];var _3p = _1Z(_3k,_3l,_3m,_3n,_3o,_3i);return _3p;};var _3q = [1,_3f,_2x,_2J];var _3r = T(function(){return [1,_1f,_3q,_3s,_1h];});var _3s = function(_3t){return [1,_3r,_3t];};var _3u = [1];var _3v = [8];var _3w = function(_3x){return [1,_3u,_3v,_1d,_3x,_3u,_3u];};var _3y = function(_3z,_3A){var _3B = T(function(){var _3C = T(function(){return _3w(_3z);});return _3s(_3C);});return die(_3B,_3A);};var _3D = function(_3E,_3F){return _3y(_3E,_3F);};var _3G = [1,_u,_q,_C,_3D];var _3H = function(_3I){var _3J = E(_3I);var _3K = _3J[1];var _3L = E(_3K);return _3L;};var _3M = function(_3N){var _3O = E(_3N);var _3P = _3O[2];var _3Q = E(_3P);return _3Q;};var _3R = function(_3S,_3T){var _3U = nMV(_3S,_3T);var _3V = _3U[1];var _3W = _3U[2];var _3X = [1,_3W];var _3Y = [1,_3V,_3X];return _3Y;};var _3Z = function(_40,_41){return _3R(_40,_41);};var _42 = function(_43,_44,_45){var _46 = E(_43);var _47 = _46[1];var _48 = A(_3,[_1,_44]);var _49 = _48[1];var _4a = jsSetInterval(_47,_49,_45);var _4b = _4a[1];var _4c = [1,_4b,_0];return _4c;};var _4d = function(_4e,_h,_i){return _42(_4e,_h,_i);};var _4f = function(_4g,_4h){return A(_4g,[_4h]);};var _4i = [15,coercionToken];var _4j = "load";var _4k = [1,_4j];var _4l = "mousemove";var _4m = [1,_4l];var _4n = "mouseover";var _4o = [1,_4n];var _4p = "mouseout";var _4q = [1,_4p];var _4r = "click";var _4s = [1,_4r];var _4t = "dblclick";var _4u = [1,_4t];var _4v = "mousedown";var _4w = [1,_4v];var _4x = "mouseup";var _4y = [1,_4x];var _4z = "keypress";var _4A = [1,_4z];var _4B = "keyup";var _4C = [1,_4B];var _4D = "keydown";var _4E = [1,_4D];var _4F = "unload";var _4G = [1,_4F];var _4H = "change";var _4I = [1,_4H];var _4J = "focus";var _4K = [1,_4J];var _4L = "blur";var _4M = [1,_4L];var _4N = function(_4O,_4P,_4Q,_4R){var _4S = [1,_4Q];var _4T = _4S[1];var _4U = function(_4V){var _4W = E(_4Q);var _4X = jsSetCB(_4O,_4V,_4T,_4R);var _4Y = _4X[1];var _4Z = _4X[2];var _50 = T(function(){var _51 = E(_4Z);return _51?true:false;});var _52 = [1,_4Y,_50];return _52;};var _53 = E(_4P);switch(_53[0]){case 1:var _54 = E(_4k);var _55 = _54[1];var _56 = _4U(_55);var _57 = _56;break;case 2:var _58 = E(_4G);var _59 = _58[1];var _5a = _4U(_59);var _57 = _5a;break;case 3:var _5b = E(_4I);var _5c = _5b[1];var _5d = _4U(_5c);var _57 = _5d;break;case 4:var _5e = E(_4K);var _5f = _5e[1];var _5g = _4U(_5f);var _57 = _5g;break;case 5:var _5h = E(_4M);var _5i = _5h[1];var _5j = _4U(_5i);var _57 = _5j;break;case 6:var _5k = E(_4m);var _5l = _5k[1];var _5m = _4U(_5l);var _57 = _5m;break;case 7:var _5n = E(_4o);var _5o = _5n[1];var _5p = _4U(_5o);var _57 = _5p;break;case 8:var _5q = E(_4q);var _5r = _5q[1];var _5s = _4U(_5r);var _57 = _5s;break;case 9:var _5t = E(_4s);var _5u = _5t[1];var _5v = _4U(_5u);var _57 = _5v;break;case 10:var _5w = E(_4u);var _5x = _5w[1];var _5y = _4U(_5x);var _57 = _5y;break;case 11:var _5z = E(_4w);var _5A = _5z[1];var _5B = _4U(_5A);var _57 = _5B;break;case 12:var _5C = E(_4y);var _5D = _5C[1];var _5E = _4U(_5D);var _57 = _5E;break;case 13:var _5F = E(_4A);var _5G = _5F[1];var _5H = _4U(_5G);var _57 = _5H;break;case 14:var _5I = E(_4C);var _5J = _5I[1];var _5K = _4U(_5J);var _57 = _5K;break;case 15:var _5L = E(_4E);var _5M = _5L[1];var _5N = _4U(_5M);var _57 = _5N;break;}return _57;};var _5O = function(_5P,_5Q,_5R,_5S){var _5T = E(_5P);var _5U = _5T[1];var _5V = _4N(_5U,_5Q,_5R,_5S);return _5V;};var _5W = function(_5X,_5Y,_5Z,_2){return _5O(_5X,_5Y,_5Z,_2);};var _60 = T(function(){return unCStr(" could be found!");});var _61 = function(_62){var _63 = T(function(){return _1o(_62,_60);});var _64 = unAppCStr("No element with ID ",_63);var _65 = err(_64);return _65;};var _66 = function(_67,_68,_69){var _6a = toJSStr(_67);var _6b = _6a[1];var _6c = jsFind(_6b,_69);var _6d = _6c[1];var _6e = _6c[2];var _6f = [1,_6e];var _6g = _6f[1];var _6h = E(_6g);if(_6h[0]==1){var _6i = _61(_67);}else{var _6j = _6h[1];var _6i = A(_68,[_6j,_6d]);}return _6i;};var _6k = function(_6l,_6m,_6n){return _66(_6l,_6m,_6n);};var _6o = function(_6p,_6q){var _6r = function(_6s){return A(_5W,[_6s,_4i,_6q]);};var _6t = T(function(){return A(_6k,[_6p]);});return A(_4f,[_6t,_6r]);};var _6u = [14,coercionToken];var _6v = function(_6w,_6x){var _6y = function(_6z){return A(_5W,[_6z,_6u,_6x]);};var _6A = T(function(){return A(_6k,[_6w]);});return A(_4f,[_6A,_6y]);};var _6B = T(function(){return A(unCStr,["canvas2"]);});var _6C = function(_6D,_6E){var _6F = E(_6D);var _6G = _6F[1];var _6H = rMV(_6G,_6E);return _6H;};var _6I = function(_40,_41){return _6C(_40,_41);};var _6J = function(_6K,_6L,_6M){var _6N = E(_6K);var _6O = _6N[1];var _6P = wMV(_6O,_6L,_6M);var _6Q = [1,_6P,_0];return _6Q;};var _6R = function(_6S,_40,_41){return _6J(_6S,_40,_41);};var _6T = function(_6U,_6V){var _6W = function(_6X){var _6Y = T(function(){var _6Z = [2,_6V];var _70 = [2,_6Z,_1d];return A(_1o,[_6X,_70]);});return A(_6R,[_6U,_6Y]);};var _71 = T(function(){return A(_6I,[_6U]);});return A(_3H,[_3G,_71,_6W]);};var _72 = function(_73,_74){var _75 = function(_76){var _77 = T(function(){var _78 = [1,_74];var _79 = [2,_78,_1d];return A(_1o,[_76,_79]);});return A(_6R,[_73,_77]);};var _7a = T(function(){return A(_6I,[_73]);});return A(_3H,[_3G,_7a,_75]);};var _7b = function(_7c){return E(_7c);};var _7d = function(_7e){return _7b(_7e);};var _7f = function(_7g,_7h,_7i){var _7j = T(function(){return A(_7h,[_7i]);});return A(_7g,[_7j]);};var _7k = function(_7l,_7m){var _7n = E(_7l);var _7o = _7n[1];var _7p = jsClear(_7o,_7m);var _7q = _7p[1];var _7r = [1,_7q,_0];return _7r;};var _7s = function(_h,_i){return _7k(_h,_i);};var _7t = function(_7u){var _7v = E(_7u);var _7w = _7v[1];var _7x = _7w+1;var _7y = _7w-1;var _7z = _7y/_7x;var _7A = Math.sqrt(_7z);var _7B = _7w+1;var _7C = _7B*_7A;var _7D = _7w+_7C;var _7E = Math.log(_7D);var _7F = [1,_7E];return _7F;};var _7G = function(_7H){var _7I = E(_7H);var _7J = _7I[1];var _7K = _7J*_7J;var _7L = 1+_7K;var _7M = Math.sqrt(_7L);var _7N = _7J+_7M;var _7O = Math.log(_7N);var _7P = [1,_7O];return _7P;};var _7Q = function(_7R){var _7S = E(_7R);var _7T = _7S[1];var _7U = 1-_7T;var _7V = 1+_7T;var _7W = _7V/_7U;var _7X = Math.log(_7W);var _7Y = 0.5*_7X;var _7Z = [1,_7Y];return _7Z;};var _80 = function(_81,_82){var _83 = E(_82);var _84 = _83[1];var _85 = E(_81);var _86 = _85[1];var _87 = Math.log(_86);var _88 = Math.log(_84);var _89 = _88/_87;var _8a = [1,_89];return _8a;};var _8b = [1,3.141592653589793];var _8c = T(function(){var _8d = 0/0;var _8e = [1,_8d];return _8e;});var _8f = T(function(){var _8g = (-1)/0;var _8h = [1,_8g];return _8h;});var _8i = T(function(){var _8j = 1/0;var _8k = [1,_8j];return _8k;});var _8l = [1,0];var _8m = I(0);var _8n = I(1);var _8o = T(function(){return unCStr("base");});var _8p = T(function(){return unCStr("GHC.Exception");});var _8q = T(function(){return unCStr("ArithException");});var _8r = [1,3089387606753565184,7918018744409604096,_8o,_8p,_8q];var _8s = [1,3089387606753565184,7918018744409604096,_8r,_1d];var _8t = function(_8u){return E(_8s);};var _8v = function(_8w){var _8x = E(_8w);var _8y = _8x[1];var _8z = _8x[2];var _8A = _14(_8y);var _8B = _H(_8A,_8t,_8z);return _8B;};var _8C = T(function(){return unCStr("denormal");});var _8D = T(function(){return unCStr("divide by zero");});var _8E = T(function(){return unCStr("loss of precision");});var _8F = T(function(){return unCStr("arithmetic underflow");});var _8G = T(function(){return unCStr("arithmetic overflow");});var _8H = function(_8I){var _8J = E(_8I);switch(_8J[0]){case 1:var _8K = E(_8G);break;case 2:var _8K = E(_8F);break;case 3:var _8K = E(_8E);break;case 4:var _8K = E(_8D);break;case 5:var _8K = E(_8C);break;}return _8K;};var _8L = function(_8M,_8N){var _8O = E(_8M);if(_8O[0]==1){var _8P = unAppCStr("[]",_8N);}else{var _8Q = _8O[1];var _8R = _8O[2];var _8S = T(function(){var _8T = T(function(){var _8U = [2,_2H,_8N];var _8V = function(_8W){var _8X = E(_8W);if(_8X[0]==1){var _8Y = E(_8U);}else{var _8Z = _8X[1];var _90 = _8X[2];var _91 = T(function(){var _92 = E(_8Z);switch(_92[0]){case 1:var _93 = T(function(){return _8V(_90);});var _94 = _1o(_8G,_93);break;case 2:var _95 = T(function(){return _8V(_90);});var _94 = _1o(_8F,_95);break;case 3:var _96 = T(function(){return _8V(_90);});var _94 = _1o(_8E,_96);break;case 4:var _97 = T(function(){return _8V(_90);});var _94 = _1o(_8D,_97);break;case 5:var _98 = T(function(){return _8V(_90);});var _94 = _1o(_8C,_98);break;}return _94;});var _8Y = [2,_2G,_91];}return _8Y;};return _8V(_8R);});var _99 = E(_8Q);switch(_99[0]){case 1:var _9a = _1o(_8G,_8T);break;case 2:var _9a = _1o(_8F,_8T);break;case 3:var _9a = _1o(_8E,_8T);break;case 4:var _9a = _1o(_8D,_8T);break;case 5:var _9a = _1o(_8C,_8T);break;}return _9a;});var _8P = [2,_2I,_8S];}return _8P;};var _9b = function(_9c){return _1o(_8G,_9c);};var _9d = function(_9c){return _1o(_8C,_9c);};var _9e = function(_9c){return _1o(_8D,_9c);};var _9f = function(_9c){return _1o(_8E,_9c);};var _9g = function(_9c){return _1o(_8F,_9c);};var _9h = function(_9i,_9j){var _9k = E(_9j);switch(_9k[0]){case 1:var _9l = E(_9b);break;case 2:var _9l = E(_9g);break;case 3:var _9l = E(_9f);break;case 4:var _9l = E(_9e);break;case 5:var _9l = E(_9d);break;}return _9l;};var _9m = [1,_9h,_8H,_8L];var _9n = T(function(){return [1,_8t,_9m,_9o,_8v];});var _9o = function(_9c){return [1,_9n,_9c];};var _9p = [4];var _9q = function(_9r,_9s){var _9t = T(function(){return A(_9s,[_9r]);});return die(_9t);};var _9u = T(function(){return _9q(_9p,_9o);});var _9v = function(_9w,_9x){var _9y = E(_9w);if(_9y[0]==1){var _9z = _9y[1];var _9A = _9y[2];var _9B = E(_9x);if(_9B[0]==1){var _9C = _9B[1];var _9D = _9B[2];var _9E = _9v(_9A,_9D);if(_9E[0]==2){var _9F = _9z<_9C;if(_9F){var _9G = [1];}else{var _9H = _9z>_9C;var _9G = _9H?[3]:[2];}var _9I = _9G;}else{var _9I = E(_9E);}var _9J = _9I;}else{var _9J = [3];}var _9K = _9J;}else{var _9L = E(_9x);var _9K = _9L[0]==1?[1]:[2];}return _9K;};var _9M = function(_9N,_9O){var _9P = E(_9N);switch(_9P[0]){case 1:var _9Q = _9P[1];var _9R = E(_9O);if(_9R[0]==1){var _9S = _9R[1];var _9T = _9v(_9Q,_9S);}else{var _9T = [3];}var _9U = _9T;break;case 2:var _9V = _9P[1];var _9W = E(_9O);if(_9W[0]==2){var _9X = _9W[1];var _9Y = _9v(_9X,_9V);}else{var _9Y = [1];}var _9U = _9Y;break;case 3:var _9Z = E(_9O);switch(_9Z[0]){case 1:var _a0 = [1];break;case 2:var _a0 = [3];break;case 3:var _a0 = [2];break;}var _9U = _a0;break;}return _9U;};var _a1 = function(_a2,_a3,_a4){while(1){var _a5 = E(_a3);if(_a5[0]==1){var _a6 = _a5[1];var _a7 = _a5[2];var _a8 = __word_encodeDouble(_a6,_a4,realWorld);var _a9 = _a8[2];var _aa = _a4+32|0;var _ab = _a2+_a9;_a2=_ab;_a3=_a7;_a4=_aa;continue;var _ac = die("Unreachable!");var _ad = _ac;}else{var _ad = E(_a2);}return _ad;}};var _ae = function(_af,_ag){var _ah = E(_af);switch(_ah[0]){case 1:var _ai = _ah[1];var _aj = _a1(0,_ai,_ag);break;case 2:var _ak = _ah[1];var _al = [1,E(_ak)];var _am = _ae(_al,_ag);var _an = -_am;var _aj = _an;break;case 3:var _aj = 0;break;}return _aj;};var _ao = function(_ap,_aq){var _ar = _9M(_ap,_aq);return _ar[0]==2?true:false;};var _as = function(_at){var _au = E(_at);switch(_au[0]){case 1:var _av = _au[1];var _aw = E(_av);if(_aw[0]==1){var _ax = _aw[1];var _ay = E(_ax);}else{var _ay = 0;}var _az = _ay;break;case 2:var _aA = _au[1];var _aB = E(_aA);if(_aB[0]==1){var _aC = _aB[1];var _aD = 0-_aC>>>0;}else{var _aD = 0;}var _az = _aD;break;case 3:var _az = 0;break;}return _az;};var _aE = function(_aF){var _aG = _as(_aF);var _aH = _aG&4294967295;return _aH;};var _aI = [2];var _aJ = [1,E(47),E(_aI)];var _aK = function(_aL,_aM,_aN){var _aO = E(_aL);if(_aO[0]==1){var _aP = _aO[1];var _aQ = _aO[2];var _aR = _aP==_aM;if(_aR){var _aS = _aT(_aQ,_aN);var _aU = _aS[0]==1?[1,E(0),E(_aS)]:[2];}else{var _aV = _aP>_aM;if(_aV){var _aW = _aT(_aQ,_aN);var _aX = _aP-_aM>>>0;var _aY = [1,E(_aX),E(_aW)];var _aZ = _aY;}else{var _b0 = _aT(_aQ,_aN);var _b1 = _aK(_b0,1,_aI);var _b2 = 4294967295-_aM>>>0;var _b3 = _b2+1>>>0;var _b4 = _b3+_aP>>>0;var _b5 = [1,E(_b4),E(_b1)];var _aZ = _b5;}var _aU = _aZ;}var _b6 = _aU;}else{var _b6 = E(_aJ);}return _b6;};var _aT = function(_b7,_b8){var _b9 = E(_b7);if(_b9[0]==1){var _ba = _b9[1];var _bb = _b9[2];var _bc = E(_b8);if(_bc[0]==1){var _bd = _bc[1];var _be = _bc[2];var _bf = _ba==_bd;if(_bf){var _bg = _aT(_bb,_be);var _bh = _bg[0]==1?[1,E(0),E(_bg)]:[2];}else{var _bi = _ba>_bd;if(_bi){var _bj = _aT(_bb,_be);var _bk = _ba-_bd>>>0;var _bl = [1,E(_bk),E(_bj)];var _bm = _bl;}else{var _bn = _aT(_bb,_be);var _bo = _aK(_bn,1,_aI);var _bp = 4294967295-_bd>>>0;var _bq = _bp+1>>>0;var _br = _bq+_ba>>>0;var _bs = [1,E(_br),E(_bo)];var _bm = _bs;}var _bh = _bm;}var _bt = _bh;}else{var _bt = E(_b9);}var _bu = _bt;}else{var _bv = E(_b8);var _bu = _bv[0]==1?E(_aJ):[2];}return _bu;};var _bw = [1,E(1),E(_aI)];var _bx = function(_by){var _bz = E(_by);if(_bz[0]==1){var _bA = _bz[1];var _bB = _bz[2];var _bC = _bA==4294967295;if(_bC){var _bD = _bx(_bB);var _bE = [1,E(0),E(_bD)];var _bF = _bE;}else{var _bG = _bA+1>>>0;var _bH = [1,E(_bG),E(_bB)];var _bF = _bH;}var _bI = _bF;}else{var _bI = E(_bw);}return _bI;};var _bJ = T(function(){return _bx(_aI);});var _bK = function(_bL,_bM,_bN,_bO,_bP){var _bQ = _bM<_bO;if(_bQ){var _bR = _bK(_bL,_bO,_bP,_bM,_bN);}else{var _bS = _bO>=2147483648;if(_bS){var _bT = _bU(1,_bN,_bP);var _bV = _bO-2147483648>>>0;var _bW = _bM-2147483648>>>0;var _bX = _bW+_bV>>>0;var _bY = _bX+_bL>>>0;var _bZ = [1,E(_bY),E(_bT)];var _c0 = _bZ;}else{var _c1 = _bM>=2147483648;if(_c1){var _c2 = _bM-2147483648>>>0;var _c3 = _c2+_bO>>>0;var _c4 = _c3+_bL>>>0;var _c5 = _c4<2147483648;if(_c5){var _c6 = _bU(0,_bN,_bP);var _c7 = _c4+2147483648>>>0;var _c8 = [1,E(_c7),E(_c6)];var _c9 = _c8;}else{var _ca = _bU(1,_bN,_bP);var _cb = _c4-2147483648>>>0;var _cc = [1,E(_cb),E(_ca)];var _c9 = _cc;}var _cd = _c9;}else{var _ce = _bU(0,_bN,_bP);var _cf = _bM+_bO>>>0;var _cg = _cf+_bL>>>0;var _ch = [1,E(_cg),E(_ce)];var _cd = _ch;}var _c0 = _cd;}var _bR = _c0;}return _bR;};var _bU = function(_ci,_cj,_ck){var _cl = E(_cj);if(_cl[0]==1){var _cm = _cl[1];var _cn = _cl[2];var _co = E(_ck);if(_co[0]==1){var _cp = _co[1];var _cq = _co[2];var _cr = _cm<_cp;if(_cr){var _cs = _bK(_ci,_cp,_cq,_cm,_cn);}else{var _ct = _cp>=2147483648;if(_ct){var _cu = _bU(1,_cn,_cq);var _cv = _cp-2147483648>>>0;var _cw = _cm-2147483648>>>0;var _cx = _cw+_cv>>>0;var _cy = _cx+_ci>>>0;var _cz = [1,E(_cy),E(_cu)];var _cA = _cz;}else{var _cB = _cm>=2147483648;if(_cB){var _cC = _cm-2147483648>>>0;var _cD = _cC+_cp>>>0;var _cE = _cD+_ci>>>0;var _cF = _cE<2147483648;if(_cF){var _cG = _bU(0,_cn,_cq);var _cH = _cE+2147483648>>>0;var _cI = [1,E(_cH),E(_cG)];var _cJ = _cI;}else{var _cK = _bU(1,_cn,_cq);var _cL = _cE-2147483648>>>0;var _cM = [1,E(_cL),E(_cK)];var _cJ = _cM;}var _cN = _cJ;}else{var _cO = _bU(0,_cn,_cq);var _cP = _cm+_cp>>>0;var _cQ = _cP+_ci>>>0;var _cR = [1,E(_cQ),E(_cO)];var _cN = _cR;}var _cA = _cN;}var _cs = _cA;}var _cS = _cs;}else{var _cT = _ci==0;var _cS = _cT?E(_cl):_bx(_cl);}var _cU = _cS;}else{var _cV = E(_ck);if(_cV[0]==1){var _cW = _ci==0;var _cX = _cW?E(_cV):_bx(_cV);}else{var _cY = _ci==0;var _cX = _cY?[2]:E(_bJ);}var _cU = _cX;}return _cU;};var _cZ = function(_d0,_d1){while(1){var _d2 = E(_d0);switch(_d2[0]){case 1:var _d3 = _d2[1];var _d4 = E(_d1);switch(_d4[0]){case 1:var _d5 = _d4[1];var _d6 = _bU(0,_d3,_d5);var _d7 = [1,E(_d6)];var _d8 = _d7;break;case 2:var _d9 = _d4[1];var _da = _9v(_d3,_d9);switch(_da[0]){case 1:var _db = _aT(_d9,_d3);var _dc = [2,E(_db)];var _dd = _dc;break;case 2:var _dd = [3];break;case 3:var _de = _aT(_d3,_d9);var _df = [1,E(_de)];var _dd = _df;break;}var _d8 = _dd;break;case 3:var _d8 = E(_d2);break;}var _dg = _d8;break;case 2:var _dh = _d2[1];var _di = E(_d1);switch(_di[0]){case 1:var _dj = _di[1];var _dk = [2,E(_dh)];var _dl = [1,E(_dj)];_d0=_dl;_d1=_dk;continue;var _dm = die("Unreachable!");break;case 2:var _dn = _di[1];var _do = _bU(0,_dh,_dn);var _dp = [2,E(_do)];var _dm = _dp;break;case 3:var _dm = E(_d2);break;}var _dg = _dm;break;case 3:var _dg = E(_d1);break;}return _dg;}};var _dq = [3];var _dr = [1,E(_aJ)];var _ds = function(_dt){var _du = E(_dt);switch(_du[0]){case 1:var _dv = _du[1];var _dw = [2,E(_dv)];break;case 2:var _dx = _du[1];var _dw = [1,E(_dx)];break;case 3:var _dw = [3];break;}return _dw;};var _dy = [1];var _dz = function(_dA){var _dB = E(_dA);return _dB[0]==1?[1,E(_dB)]:[3];};var _dC = function(_dD,_dE,_dF){while(1){var _dG = E(_dE);if(_dG[0]==1){var _dH = E(_dF);var _dI = [1,_dD,_dH];var _dJ = _dI;}else{var _dK = _dG[1];var _dL = _dG[2];var _dM = _9v(_dF,_dK);if(_dM[0]==1){var _dN = _dD<<1>>>0;_dD=_dN;_dE=_dL;_dF=_dF;continue;var _dO = die("Unreachable!");var _dP = _dO;}else{var _dQ = _aT(_dF,_dK);var _dR = _dD<<1>>>0;var _dS = _dR+1>>>0;_dD=_dS;_dE=_dL;_dF=_dQ;continue;var _dT = die("Unreachable!");var _dP = _dT;}var _dJ = _dP;}return _dJ;}};var _dU = function(_dV,_dW){var _dX = E(_dW);if(_dX){var _dY = 32-_dX|0;var _dZ = function(_e0,_e1){var _e2 = E(_e1);if(_e2[0]==1){var _e3 = _e2[1];var _e4 = _e2[2];var _e5 = _e3>>>_dY;var _e6 = _dZ(_e5,_e4);var _e7 = _e3<<_dX>>>0;var _e8 = (_e7|_e0)>>>0;var _e9 = [1,E(_e8),E(_e6)];var _ea = _e9;}else{var _eb = _e0==0;var _ea = _eb?[2]:[1,E(_e0),E(_aI)];}return _ea;};var _ec = _dZ(0,_dV);var _ed = _ec;}else{var _ed = E(_dV);}return _ed;};var _ee = function(_ef,_eg){var _eh = E(_eg);if(_eh[0]==1){var _ei = [1,E(_ef),E(_eh)];}else{var _ej = _ef==0;var _ei = _ej?[2]:[1,E(_ef),E(_aI)];}return _ei;};var _ek = function(_el,_em){var _en = E(_em);var _eo = T(function(){var _ep = [2,_en,_dy];var _eq = function(_er){var _es = E(_er);if(_es){var _et = T(function(){var _eu = _es-1|0;var _ev = _eq(_eu);return _ev;});var _ew = T(function(){return _dU(_en,_es);});var _ex = [2,_ew,_et];}else{var _ex = E(_ep);}return _ex;};return _eq(31);});var _ey = function(_ez){var _eA = E(_ez);if(_eA[0]==1){var _eB = _eA[1];var _eC = _eA[2];var _eD = _ey(_eC);var _eE = _eD[1];var _eF = _eD[2];var _eG = E(_eF);if(_eG[0]==1){var _eH = [1,E(_eB),E(_eG)];var _eI = _dC(0,_eo,_eH);var _eJ = _eI[1];var _eK = _eI[2];var _eL = T(function(){return _ee(_eJ,_eE);});var _eM = [1,_eL,_eK];var _eN = _eM;}else{var _eO = _eB==0;if(_eO){var _eP = _dC(0,_eo,_aI);var _eQ = _eP[1];var _eR = _eP[2];var _eS = T(function(){return _ee(_eQ,_eE);});var _eT = [1,_eS,_eR];var _eU = _eT;}else{var _eV = [1,E(_eB),E(_aI)];var _eW = _dC(0,_eo,_eV);var _eX = _eW[1];var _eY = _eW[2];var _eZ = T(function(){return _ee(_eX,_eE);});var _f0 = [1,_eZ,_eY];var _eU = _f0;}var _eN = _eU;}var _f1 = _eN;}else{var _f1 = [1,_aI,_aI];}return _f1;};var _f2 = _ey(_el);var _f3 = _f2[1];var _f4 = _f2[2];var _f5 = T(function(){return _dz(_f4);});var _f6 = T(function(){return _dz(_f3);});var _f7 = [1,_f6,_f5];return _f7;};var _f8 = function(_f9,_fa){var _fb = E(_f9);if(_fb[0]==3){var _fc = E(_fa);var _fd = [1,_dq,_dq];var _fe = _fd;}else{var _ff = E(_fa);if(_ff[0]==3){var _fg = [1,_dr,_dr];}else{var _fh = E(_fb);if(_fh[0]==1){var _fi = _fh[1];var _fj = E(_ff);if(_fj[0]==1){var _fk = _fj[1];var _fl = _ek(_fi,_fk);}else{var _fm = _fj[1];var _fn = _ek(_fi,_fm);var _fo = _fn[1];var _fp = _fn[2];var _fq = T(function(){return _ds(_fo);});var _fr = [1,_fq,_fp];var _fl = _fr;}var _fs = _fl;}else{var _ft = _fh[1];var _fu = E(_ff);if(_fu[0]==1){var _fv = _fu[1];var _fw = _ek(_ft,_fv);var _fx = _fw[1];var _fy = _fw[2];var _fz = T(function(){return _ds(_fy);});var _fA = T(function(){return _ds(_fx);});var _fB = [1,_fA,_fz];var _fC = _fB;}else{var _fD = _fu[1];var _fE = _ek(_ft,_fD);var _fF = _fE[1];var _fG = _fE[2];var _fH = T(function(){return _ds(_fG);});var _fI = [1,_fF,_fH];var _fC = _fI;}var _fs = _fC;}var _fg = _fs;}var _fe = _fg;}return _fe;};var _fJ = function(_fK,_fL){while(1){var _fM = _fL>=32;if(_fM){var _fN = E(_fK);var _fO = _fL-32|0;var _fP = [1,E(0),E(_fN)];_fK=_fP;_fL=_fO;continue;var _fQ = die("Unreachable!");var _fR = _fQ;}else{var _fR = _dU(_fK,_fL);}return _fR;}};var _fS = function(_fT,_fU){var _fV = E(_fT);switch(_fV[0]){case 1:var _fW = _fV[1];var _fX = _fJ(_fW,_fU);var _fY = [1,E(_fX)];var _fZ = _fY;break;case 2:var _g0 = _fV[1];var _g1 = _fJ(_g0,_fU);var _g2 = [2,E(_g1)];var _fZ = _g2;break;case 3:var _fZ = [3];break;}return _fZ;};var _g3 = I(0);var _g4 = function(_g5,_g6,_g7){var _g8 = _ao(_g7,_g3);if(_g8){var _g9 = E(_9u);}else{var _ga = _f8(_g6,_g7);var _gb = _ga[1];var _gc = _ga[2];var _gd = _fS(_gc,1);var _ge = _9M(_gd,_g7);switch(_ge[0]){case 1:var _gf = _ae(_gb,_g5);break;case 2:var _gg = _aE(_gb);var _gh = _gg>>>0;var _gi = (_gh&1)>>>0;var _gj = _gi&4294967295;if(_gj){var _gk = _cZ(_gb,_8n);var _gl = _ae(_gk,_g5);var _gm = _gl;}else{var _gm = _ae(_gb,_g5);}var _gf = _gm;break;case 3:var _gn = _cZ(_gb,_8n);var _go = _ae(_gn,_g5);var _gf = _go;break;}var _g9 = _gf;}return _g9;};var _gp = T(function(){var _gq = die("Unsupported PrimOp: newByteArray#");var _gr = _gq[1];var _gs = _gq[2];var _gt = die("Unsupported PrimOp: writeInt8Array#");var _gu = function(_gv,_gw,_gx,_gy){while(1){var _gz = E(_gx);if(_gz==256){var _gA = E(_gy);}else{var _gB = _gz<_gv;if(_gB){var _gC = die("Unsupported PrimOp: writeInt8Array#");var _gD = _gz+1|0;_gv=_gv;_gw=_gw;_gx=_gD;_gy=_gC;continue;var _gE = die("Unreachable!");var _gF = _gE;}else{var _gG = _gw-1|0;var _gH = imul(2,_gv)|0;_gv=_gH;_gw=_gG;_gx=_gz;_gy=_gy;continue;var _gI = die("Unreachable!");var _gF = _gI;}var _gA = _gF;}return _gA;}};var _gJ = _gu(2,8,1,_gt);var _gK = [1,_gJ,_gs];var _gL = _gK[2];var _gM = [1,_gL];return _gM;});var _gN = function(_gO,_gP,_gQ){while(1){var _gR = E(_gQ);if(_gR[0]==1){var _gS = _gR[1];var _gT = _gR[2];var _gU = _gO+32|0;_gO=_gU;_gP=_gS;_gQ=_gT;continue;var _gV = die("Unreachable!");var _gW = _gV;}else{var _gX = E(_gp);var _gY = _gX[1];var _gZ = _gP>>>24;var _h0 = _gZ!=0;if(_h0){var _h1 = _gZ&4294967295;var _h2 = die("Unsupported PrimOp: indexInt8Array#");var _h3 = 32-_h2|0;var _h4 = _gO+_h3|0;var _h5 = _h4;}else{var _h6 = _gP>>>16;var _h7 = _h6!=0;if(_h7){var _h8 = _h6&4294967295;var _h9 = die("Unsupported PrimOp: indexInt8Array#");var _ha = 24-_h9|0;var _hb = _gO+_ha|0;var _hc = _hb;}else{var _hd = _gP>>>8;var _he = _hd!=0;if(_he){var _hf = _hd&4294967295;var _hg = die("Unsupported PrimOp: indexInt8Array#");var _hh = 16-_hg|0;var _hi = _gO+_hh|0;var _hj = _hi;}else{var _hk = _gP&4294967295;var _hl = die("Unsupported PrimOp: indexInt8Array#");var _hm = 8-_hl|0;var _hn = _gO+_hm|0;var _hj = _hn;}var _hc = _hj;}var _h5 = _hc;}var _gW = _h5;}return _gW;}};var _ho = function(_hp){var _hq = E(_hp);if(_hq[0]==1){var _hr = _hq[1];var _hs = E(_hr);if(_hs[0]==1){var _ht = _hs[1];var _hu = _hs[2];var _hv = E(_hu);if(_hv[0]==1){var _hw = _hv[1];var _hx = _hv[2];var _hy = _gN(32,_hw,_hx);}else{var _hz = E(_gp);var _hA = _hz[1];var _hB = _ht>>>24;var _hC = _hB!=0;if(_hC){var _hD = _hB&4294967295;var _hE = die("Unsupported PrimOp: indexInt8Array#");var _hF = 32-_hE|0;var _hG = 0+_hF|0;var _hH = _hG;}else{var _hI = _ht>>>16;var _hJ = _hI!=0;if(_hJ){var _hK = _hI&4294967295;var _hL = die("Unsupported PrimOp: indexInt8Array#");var _hM = 24-_hL|0;var _hN = 0+_hM|0;var _hO = _hN;}else{var _hP = _ht>>>8;var _hQ = _hP!=0;if(_hQ){var _hR = _hP&4294967295;var _hS = die("Unsupported PrimOp: indexInt8Array#");var _hT = 16-_hS|0;var _hU = 0+_hT|0;var _hV = _hU;}else{var _hW = _ht&4294967295;var _hX = die("Unsupported PrimOp: indexInt8Array#");var _hY = 8-_hX|0;var _hZ = 0+_hY|0;var _hV = _hZ;}var _hO = _hV;}var _hH = _hO;}var _hy = _hH;}var _i0 = _hy;}else{var _i0 = 0;}var _i1 = _i0;}else{var _i1 = (-1);}return _i1;};var _i2 = function(_i3,_i4,_i5){while(1){var _i6 = E(_i5);if(_i6[0]==1){var _i7 = _i6[1];var _i8 = _i6[2];var _i9 = _i3+32|0;_i3=_i9;_i4=_i7;_i5=_i8;continue;var _ia = die("Unreachable!");var _ib = _ia;}else{var _ic = E(_gp);var _id = _ic[1];var _ie = _i4>>>24;var _if = _ie!=0;if(_if){var _ig = _ie&4294967295;var _ih = die("Unsupported PrimOp: indexInt8Array#");var _ii = 32-_ih|0;var _ij = _i3+_ii|0;var _ik = _ij;}else{var _il = _i4>>>16;var _im = _il!=0;if(_im){var _in = _il&4294967295;var _io = die("Unsupported PrimOp: indexInt8Array#");var _ip = 24-_io|0;var _iq = _i3+_ip|0;var _ir = _iq;}else{var _is = _i4>>>8;var _it = _is!=0;if(_it){var _iu = _is&4294967295;var _iv = die("Unsupported PrimOp: indexInt8Array#");var _iw = 16-_iv|0;var _ix = _i3+_iw|0;var _iy = _ix;}else{var _iz = _i4&4294967295;var _iA = die("Unsupported PrimOp: indexInt8Array#");var _iB = 8-_iA|0;var _iC = _i3+_iB|0;var _iy = _iC;}var _ir = _iy;}var _ik = _ir;}var _iD = _ik;var _iE = [1,_iD,1];var _ib = _iE;}return _ib;}};var _iF = function(_iG,_iH,_iI){while(1){var _iJ = E(_iI);if(_iJ[0]==1){var _iK = _iJ[1];var _iL = _iJ[2];var _iM = _iH==0;if(_iM){var _iN = _iG+32|0;_iG=_iN;_iH=_iK;_iI=_iL;continue;var _iO = die("Unreachable!");var _iP = _iO;}else{var _iQ = _iG+32|0;var _iR = _i2(_iQ,_iK,_iL);var _iP = _iR;}var _iS = _iP;}else{var _iT = _iH-1>>>0;var _iU = (_iH&_iT)>>>0;var _iV = _iU&4294967295;var _iW = E(_gp);var _iX = _iW[1];var _iY = _iH>>>24;var _iZ = _iY!=0;if(_iZ){var _j0 = _iY&4294967295;var _j1 = die("Unsupported PrimOp: indexInt8Array#");var _j2 = 32-_j1|0;var _j3 = _iG+_j2|0;var _j4 = _j3;}else{var _j5 = _iH>>>16;var _j6 = _j5!=0;if(_j6){var _j7 = _j5&4294967295;var _j8 = die("Unsupported PrimOp: indexInt8Array#");var _j9 = 24-_j8|0;var _ja = _iG+_j9|0;var _jb = _ja;}else{var _jc = _iH>>>8;var _jd = _jc!=0;if(_jd){var _je = _jc&4294967295;var _jf = die("Unsupported PrimOp: indexInt8Array#");var _jg = 16-_jf|0;var _jh = _iG+_jg|0;var _ji = _jh;}else{var _jj = _iH&4294967295;var _jk = die("Unsupported PrimOp: indexInt8Array#");var _jl = 8-_jk|0;var _jm = _iG+_jl|0;var _ji = _jm;}var _jb = _ji;}var _j4 = _jb;}var _jn = _j4;var _jo = [1,_jn,_iV];var _iS = _jo;}return _iS;}};var _jp = function(_jq){var _jr = E(_jq);if(_jr[0]==1){var _js = _jr[1];var _jt = E(_js);if(_jt[0]==1){var _ju = _jt[1];var _jv = _jt[2];var _jw = E(_jv);if(_jw[0]==1){var _jx = _jw[1];var _jy = _jw[2];var _jz = _ju==0;var _jA = _jz?_iF(32,_jx,_jy):_i2(32,_jx,_jy);}else{var _jB = _ju-1>>>0;var _jC = (_ju&_jB)>>>0;var _jD = _jC&4294967295;var _jE = E(_gp);var _jF = _jE[1];var _jG = _ju>>>24;var _jH = _jG!=0;if(_jH){var _jI = _jG&4294967295;var _jJ = die("Unsupported PrimOp: indexInt8Array#");var _jK = 32-_jJ|0;var _jL = 0+_jK|0;var _jM = _jL;}else{var _jN = _ju>>>16;var _jO = _jN!=0;if(_jO){var _jP = _jN&4294967295;var _jQ = die("Unsupported PrimOp: indexInt8Array#");var _jR = 24-_jQ|0;var _jS = 0+_jR|0;var _jT = _jS;}else{var _jU = _ju>>>8;var _jV = _jU!=0;if(_jV){var _jW = _jU&4294967295;var _jX = die("Unsupported PrimOp: indexInt8Array#");var _jY = 16-_jX|0;var _jZ = 0+_jY|0;var _k0 = _jZ;}else{var _k1 = _ju&4294967295;var _k2 = die("Unsupported PrimOp: indexInt8Array#");var _k3 = 8-_k2|0;var _k4 = 0+_k3|0;var _k0 = _k4;}var _jT = _k0;}var _jM = _jT;}var _k5 = _jM;var _k6 = [1,_k5,_jD];var _jA = _k6;}var _k7 = _jA;}else{var _k7 = [1,0,1];}var _k8 = _k7;}else{var _k8 = [1,(-1),1];}return _k8;};var _k9 = function(_ka){var _kb = _ka==0;if(_kb){var _kc = [3];}else{var _kd = [1,E(_ka),E(_aI)];var _kc = [1,E(_kd)];}return _kc;};var _ke = function(_kf){var _kg = _kf>=0;if(_kg){var _kh = _kf>>>0;var _ki = _k9(_kh);var _kj = _ki;}else{var _kk = -_kf;var _kl = _kk>>>0;var _km = _k9(_kl);var _kn = _ds(_km);var _kj = _kn;}return _kj;};var _ko = T(function(){return _ke(1);});var _kp = function(_kq,_kr){var _ks = E(_kq);if(_ks[0]==1){var _kt = _ks[1];var _ku = _ks[2];var _kv = E(_kr);if(_kv[0]==1){var _kw = _kv[1];var _kx = _kv[2];var _ky = _kp(_ku,_kx);var _kz = (_kt&_kw)>>>0;var _kA = [1,E(_kz),E(_ky)];var _kB = _kA;}else{var _kB = [2];}var _kC = _kB;}else{var _kD = E(_kr);var _kC = _kD[0]==1?[2]:[2];}return _kC;};var _kE = function(_kF,_kG){var _kH = E(_kF);if(_kH[0]==1){var _kI = _kH[1];var _kJ = _kH[2];var _kK = E(_kG);if(_kK[0]==1){var _kL = _kK[1];var _kM = _kK[2];var _kN = _kE(_kJ,_kM);var _kO = (_kI&_kL)>>>0;var _kP = [1,E(_kO),E(_kN)];var _kQ = _kP;}else{var _kQ = [2];}var _kR = _kQ;}else{var _kR = E(_kG);}return _kR;};var _kS = function(_kT){var _kU = E(_kT);if(_kU[0]==1){var _kV = _kU[1];var _kW = _kU[2];var _kX = _kS(_kW);var _kY = ~_kV;var _kZ = [1,E(_kY),E(_kX)];var _l0 = _kZ;}else{var _l0 = [2];}return _l0;};var _l1 = function(_l2,_l3){var _l4 = E(_l2);if(_l4[0]==1){var _l5 = _l4[1];var _l6 = _l4[2];var _l7 = E(_l3);if(_l7[0]==1){var _l8 = _l7[1];var _l9 = _l7[2];var _la = _l1(_l6,_l9);var _lb = (_l5|_l8)>>>0;var _lc = [1,E(_lb),E(_la)];var _ld = _lc;}else{var _ld = E(_l4);}var _le = _ld;}else{var _le = E(_l3);}return _le;};var _lf = function(_lg){var _lh = E(_lg);if(_lh[0]==1){var _li = _lh[1];var _lj = _lh[2];var _lk = _li==0;if(_lk){var _ll = _lf(_lj);var _lm = _ll[0]==1?[1,E(_li),E(_ll)]:[2];}else{var _ln = _lf(_lj);var _lo = [1,E(_li),E(_ln)];var _lm = _lo;}var _lp = _lm;}else{var _lp = [2];}return _lp;};var _lq = function(_lr,_ls){while(1){var _lt = E(_lr);if(_lt[0]==3){var _lu = E(_ls);var _lv = [3];var _lw = _lv;}else{var _lx = E(_ls);if(_lx[0]==3){var _ly = [3];}else{var _lz = E(_lt);if(_lz[0]==1){var _lA = _lz[1];var _lB = E(_lx);if(_lB[0]==1){var _lC = _lB[1];var _lD = _kp(_lA,_lC);var _lE = _lf(_lD);var _lF = _lE[0]==1?[1,E(_lE)]:[3];var _lG = _lF;}else{var _lH = _lB[1];var _lI = _aK(_lH,1,_aI);var _lJ = _kS(_lI);var _lK = _kE(_lJ,_lA);var _lL = _lf(_lK);var _lM = _lL[0]==1?[1,E(_lL)]:[3];var _lG = _lM;}var _lN = _lG;}else{var _lO = _lz[1];var _lP = E(_lx);if(_lP[0]==1){var _lQ = _lP[1];var _lR = [2,E(_lO)];var _lS = [1,E(_lQ)];_lr=_lS;_ls=_lR;continue;var _lT = die("Unreachable!");}else{var _lU = _lP[1];var _lV = _aK(_lU,1,_aI);var _lW = _aK(_lO,1,_aI);var _lX = _l1(_lW,_lV);var _lY = _bx(_lX);var _lZ = _lf(_lY);var _m0 = _lZ[0]==1?[2,E(_lZ)]:[3];var _lT = _m0;}var _lN = _lT;}var _ly = _lN;}var _lw = _ly;}return _lw;}};var _m1 = function(_m2,_m3){var _m4 = _9M(_m2,_m3);return _m4[0]==3?true:false;};var _m5 = function(_m6,_m7){var _m8 = _9M(_m6,_m7);return _m8[0]==1?true:false;};var _m9 = function(_ma,_mb){var _mc = _ds(_mb);var _md = _cZ(_ma,_mc);return _md;};var _me = function(_mf,_mg){var _mh = _fS(_ko,_mg);var _mi = _cZ(_mh,_mh);var _mj = _m9(_mi,_ko);var _mk = _lq(_mf,_mj);var _ml = _m5(_mh,_mk);if(_ml){var _mm = 2;}else{var _mn = _m1(_mh,_mk);var _mm = _mn?0:1;}return _mm;};var _mo = function(_mp,_mq){var _mr = _9M(_mp,_mq);return _mr[0]==3?false:true;};var _ms = [2,E(_bw)];var _mt = function(_mu){return _m9(_ms,_mu);};var _mv = function(_mw,_mx){var _my = E(_mw);var _mz = E(_mx);if(_mz){var _mA = 32-_mz|0;var _mB = _dU(_my,_mA);if(_mB[0]==1){var _mC = _mB[2];var _mD = E(_mC);var _mE = _mD[0]==1?[1,E(_mD)]:[3];}else{var _mE = [3];}var _mF = _mE;}else{var _mF = [1,E(_my)];}return _mF;};var _mG = function(_mH,_mI){while(1){var _mJ = E(_mH);if(_mJ[0]==1){var _mK = _mJ[2];var _mL = _mI>=32;if(_mL){var _mM = _mI-32|0;_mH=_mK;_mI=_mM;continue;var _mN = die("Unreachable!");var _mO = _mN;}else{var _mO = _mv(_mJ,_mI);}var _mP = _mO;}else{var _mP = [3];}return _mP;}};var _mQ = function(_mR,_mS){var _mT = E(_mR);switch(_mT[0]){case 1:var _mU = _mT[1];var _mV = _mG(_mU,_mS);break;case 2:var _mW = _mt(_mT);var _mX = _mQ(_mW,_mS);var _mY = _mt(_mX);var _mV = _mY;break;case 3:var _mV = [3];break;}return _mV;};var _mZ = function(_n0,_n1,_n2,_n3){var _n4 = _jp(_n3);var _n5 = _n4[1];var _n6 = _n4[2];var _n7 = E(_n6);if(_n7){var _n8 = _ho(_n2);var _n9 = _n8-_n5|0;var _na = function(_nb){var _nc = function(_nd,_ne){var _nf = T(function(){var _ng = _fS(_ne,1);var _nh = _nb-_n1|0;var _ni = _nh+1|0;var _nj = _g4(_ni,_nd,_ng);var _nk = [1,_nj];return _nk;});var _nl = _n1>=0;if(_nl){var _nm = _fS(_ne,_n1);var _nn = _mo(_nm,_nd);if(_nn){var _no = E(_nf);var _np = _no[1];var _nq = E(_np);var _nr = _nq;}else{var _ns = _nb-_n1|0;var _nt = _g4(_ns,_nd,_ne);var _nr = _nt;}var _nu = _nr;}else{var _nv = -_n1;var _nw = _mQ(_ne,_nv);var _nx = _mo(_nw,_nd);if(_nx){var _ny = E(_nf);var _nz = _ny[1];var _nA = E(_nz);var _nB = _nA;}else{var _nC = _nb-_n1|0;var _nD = _g4(_nC,_nd,_ne);var _nB = _nD;}var _nu = _nB;}return _nu;};var _nE = _nb<_n1;if(_nE){var _nF = T(function(){var _nG = _n1-_nb|0;var _nH = _nG>=0;if(_nH){var _nI = _fS(_n2,_nG);}else{var _nJ = -_nG;var _nK = _mQ(_n2,_nJ);var _nI = _nK;}return _nI;});var _nL = _nc(_nF,_n3);}else{var _nM = _nb==_n1;if(_nM){var _nN = _nc(_n2,_n3);}else{var _nO = T(function(){var _nP = _nb-_n1|0;var _nQ = _nP>=0;if(_nQ){var _nR = _fS(_n3,_nP);}else{var _nS = -_nP;var _nT = _mQ(_n3,_nS);var _nR = _nT;}return _nR;});var _nN = _nc(_n2,_nO);}var _nL = _nN;}return _nL;};var _nU = _n0<=_n9;var _nV = _nU?_na(_n9):_na(_n0);var _nW = _nV;}else{var _nX = _ho(_n2);var _nY = _n5+_n0|0;var _nZ = _nY-1|0;var _o0 = _nX>=_nZ;if(_o0){var _o1 = _nX<_n1;if(_o1){var _o2 = -_n5;var _o3 = _ae(_n2,_o2);var _o4 = _o3;}else{var _o5 = _nX+1|0;var _o6 = _o5-_n1|0;var _o7 = -_o6;var _o8 = _o7>=0;if(_o8){var _o9 = _fS(_n2,_o7);}else{var _oa = -_o7;var _ob = _mQ(_n2,_oa);var _o9 = _ob;}var _oc = _o9;var _od = _nX-_n1|0;var _oe = _me(_n2,_od);switch(_oe){case 0:var _of = _nX-_n5|0;var _og = _of+1|0;var _oh = _og-_n1|0;var _oi = _ae(_oc,_oh);var _oj = _oi;break;case 2:var _ok = _nX-_n5|0;var _ol = _ok+1|0;var _om = _ol-_n1|0;var _on = _cZ(_oc,_8n);var _oo = _ae(_on,_om);var _oj = _oo;break;default:var _op = _aE(_oc);var _oq = _op>>>0;var _or = (_oq&1)>>>0;var _os = _or&4294967295;if(_os){var _ot = _nX-_n5|0;var _ou = _ot+1|0;var _ov = _ou-_n1|0;var _ow = _cZ(_oc,_8n);var _ox = _ae(_ow,_ov);var _oy = _ox;}else{var _oz = _nX-_n5|0;var _oA = _oz+1|0;var _oB = _oA-_n1|0;var _oC = _ae(_oc,_oB);var _oy = _oC;}var _oj = _oy;}var _o4 = _oj;}var _oD = _o4;}else{var _oE = _n0-_n1|0;var _oF = _n5+_oE|0;var _oG = _oF<=0;if(_oG){var _oH = _n0-_n1|0;var _oI = _oH-_oF|0;var _oJ = _ae(_n2,_oI);var _oK = _oJ;}else{var _oL = _oF<=_nX;if(_oL){var _oM = -_oF;var _oN = _oM>=0;if(_oN){var _oO = _fS(_n2,_oM);}else{var _oP = -_oM;var _oQ = _mQ(_n2,_oP);var _oO = _oQ;}var _oR = _oO;var _oS = _oF-1|0;var _oT = _me(_n2,_oS);switch(_oT){case 0:var _oU = _n0-_n1|0;var _oV = _ae(_oR,_oU);var _oW = _oV;break;case 1:var _oX = _aE(_oR);var _oY = _oX>>>0;var _oZ = (_oY&1)>>>0;var _p0 = _oZ&4294967295;if(_p0){var _p1 = _n0-_n1|0;var _p2 = _cZ(_oR,_8n);var _p3 = _ae(_p2,_p1);var _p4 = _p3;}else{var _p5 = _n0-_n1|0;var _p6 = _ae(_oR,_p5);var _p4 = _p6;}var _oW = _p4;break;default:var _p7 = _n0-_n1|0;var _p8 = _cZ(_oR,_8n);var _p9 = _ae(_p8,_p7);var _oW = _p9;}var _pa = _oW;}else{var _pb = _nX+1|0;var _pc = _oF>_pb;if(_pc){var _pd = 0;}else{var _pe = _jp(_n2);var _pf = _pe[2];var _pg = E(_pf);if(_pg){var _ph = _n0-_n1|0;var _pi = _ae(_8n,_ph);var _pj = _pi;}else{var _pj = 0;}var _pd = _pj;}var _pa = _pd;}var _oK = _pa;}var _oD = _oK;}var _nW = _oD;}return _nW;};var _pk = function(_pl,_pm){var _pn = _ao(_pm,_8m);if(_pn){var _po = _ao(_pl,_8m);if(_po){var _pp = E(_8c);}else{var _pq = _m5(_pl,_8m);var _pp = _pq?E(_8f):E(_8i);}var _pr = _pp;}else{var _ps = _ao(_pl,_8m);if(_ps){var _pt = E(_8l);}else{var _pu = _m5(_pl,_8m);if(_pu){var _pv = _ds(_pl);var _pw = _mZ((-1021),53,_pv,_pm);var _px = -_pw;var _py = [1,_px];var _pz = _py;}else{var _pA = _mZ((-1021),53,_pl,_pm);var _pB = [1,_pA];var _pz = _pB;}var _pt = _pz;}var _pr = _pt;}return _pr;};var _pC = function(_pD){var _pE = E(_pD);var _pF = _pE[1];var _pG = _pE[2];var _pH = _pk(_pF,_pG);return _pH;};var _pI = function(_pJ){var _pK = E(_pJ);var _pL = _pK[1];var _pM = 1/_pL;var _pN = [1,_pM];return _pN;};var _pO = function(_pP){var _pQ = E(_pP);var _pR = _pQ[1];var _pS = _pR>=0;if(_pS){var _pT = E(_pQ);}else{var _pU = -_pR;var _pV = [1,_pU];var _pT = _pV;}return _pT;};var _pW = function(_pX){var _pY = E(_pX);if(_pY[0]==1){var _pZ = _pY[1];var _q0 = _pY[2];var _q1 = _pW(_q0);var _q2 = (_pZ&65535)>>>0;var _q3 = _q2&4294967295;var _q4 = _q3;var _q5 = Math.pow(2,16);var _q6 = _pZ>>>16;var _q7 = _q6&4294967295;var _q8 = _q7;var _q9 = _q8*_q5;var _qa = Math.pow(2,32);var _qb = _q1*_qa;var _qc = _qb+_q9;var _qd = _qc+_q4;var _qe = _qd;}else{var _qe = 0;}return _qe;};var _qf = function(_qg){var _qh = E(_qg);switch(_qh[0]){case 1:var _qi = _qh[1];var _qj = _pW(_qi);break;case 2:var _qk = _qh[1];var _ql = _pW(_qk);var _qm = -_ql;var _qj = _qm;break;case 3:var _qj = 0;break;}return _qj;};var _qn = function(_qo){var _qp = _qf(_qo);var _qq = [1,_qp];return _qq;};var _qr = [1,1];var _qs = [1,(-1)];var _qt = function(_qu){var _qv = E(_qu);var _qw = _qv[1];var _qx = _qw==0;if(_qx){var _qy = E(_8l);}else{var _qz = _qw>0;var _qy = _qz?E(_qr):E(_qs);}return _qy;};var _qA = function(_qB,_qC){var _qD = E(_qB);var _qE = _qD[1];var _qF = E(_qC);var _qG = _qF[1];var _qH = _qE-_qG;var _qI = [1,_qH];return _qI;};var _qJ = function(_qK){var _qL = E(_qK);var _qM = _qL[1];var _qN = -_qM;var _qO = [1,_qN];return _qO;};var _qP = function(_qQ,_qR){var _qS = E(_qQ);var _qT = _qS[1];var _qU = E(_qR);var _qV = _qU[1];var _qW = _qT+_qV;var _qX = [1,_qW];return _qX;};var _qY = function(_qZ,_r0){var _r1 = E(_qZ);var _r2 = _r1[1];var _r3 = E(_r0);var _r4 = _r3[1];var _r5 = _r2*_r4;var _r6 = [1,_r5];return _r6;};var _r7 = [1,_qP,_qY,_qA,_qJ,_pO,_qt,_qn];var _r8 = function(_r9,_ra){var _rb = E(_r9);var _rc = _rb[1];var _rd = E(_ra);var _re = _rd[1];var _rf = _rc/_re;var _rg = [1,_rf];return _rg;};var _rh = [1,_r7,_r8,_pI,_pC];var _ri = function(_rj){var _rk = E(_rj);var _rl = _rk[1];var _rm = Math.acos(_rl);var _rn = [1,_rm];return _rn;};var _ro = function(_rp){var _rq = E(_rp);var _rr = _rq[1];var _rs = Math.asin(_rr);var _rt = [1,_rs];return _rt;};var _ru = function(_rv){var _rw = E(_rv);var _rx = _rw[1];var _ry = Math.atan(_rx);var _rz = [1,_ry];return _rz;};var _rA = function(_rB){var _rC = E(_rB);var _rD = _rC[1];var _rE = Math.cos(_rD);var _rF = [1,_rE];return _rF;};var _rG = function(_rH){var _rI = E(_rH);var _rJ = _rI[1];var _rK = cosh(_rJ);var _rL = [1,_rK];return _rL;};var _rM = function(_rN){var _rO = E(_rN);var _rP = _rO[1];var _rQ = Math.exp(_rP);var _rR = [1,_rQ];return _rR;};var _rS = function(_rT){var _rU = E(_rT);var _rV = _rU[1];var _rW = Math.log(_rV);var _rX = [1,_rW];return _rX;};var _rY = function(_rZ,_s0){var _s1 = E(_rZ);var _s2 = _s1[1];var _s3 = E(_s0);var _s4 = _s3[1];var _s5 = Math.pow(_s2,_s4);var _s6 = [1,_s5];return _s6;};var _s7 = function(_s8){var _s9 = E(_s8);var _sa = _s9[1];var _sb = Math.sin(_sa);var _sc = [1,_sb];return _sc;};var _sd = function(_se){var _sf = E(_se);var _sg = _sf[1];var _sh = sinh(_sg);var _si = [1,_sh];return _si;};var _sj = function(_sk){var _sl = E(_sk);var _sm = _sl[1];var _sn = Math.sqrt(_sm);var _so = [1,_sn];return _so;};var _sp = function(_sq){var _sr = E(_sq);var _ss = _sr[1];var _st = Math.tan(_ss);var _su = [1,_st];return _su;};var _sv = function(_sw){var _sx = E(_sw);var _sy = _sx[1];var _sz = tanh(_sy);var _sA = [1,_sz];return _sA;};var _sB = [1,_rh,_8b,_rM,_sj,_rS,_rY,_80,_s7,_sp,_rA,_ro,_ru,_ri,_sd,_sv,_rG,_7G,_7Q,_7t];var _sC = function(_sD){var _sE = E(_sD);var _sF = _sE[2];var _sG = E(_sF);return _sG;};var _sH = function(_sI){var _sJ = E(_sI);var _sK = _sJ[2];var _sL = E(_sK);return _sL;};var _sM = function(_sN,_sO,_sP,_sQ){var _sR = T(function(){var _sS = T(function(){var _sT = T(function(){var _sU = E(_sN);var _sV = _sU[1];var _sW = function(_sX){var _sY = jsFill(_sV,_sX);var _sZ = _sY[1];var _t0 = [1,_sZ,_0];return _t0;};var _t1 = E(_sW);return _t1;});var _t2 = T(function(){var _t3 = E(_sN);var _t4 = _t3[1];var _t5 = function(_t6){var _t7 = jsClosePath(_t4,_t6);var _t8 = _t7[1];var _t9 = [1,_t8,_0];return _t9;};var _ta = E(_t5);return _ta;});return A(_3M,[_3G,_t2,_sT]);});var _tb = T(function(){var _tc = E(_sN);var _td = _tc[1];var _te = E(_sO);var _tf = _te[1];var _tg = E(_sP);var _th = _tg[1];var _ti = E(_sQ);var _tj = _ti[1];var _tk = T(function(){return _sC(_sB);});var _tl = [1,2];var _tm = A(_sH,[_r7,_tl,_tk]);var _tn = _tm[1];var _to = function(_tp){var _tq = jsArc(_td,_tf,_th,_tj,0,_tn,1,_tp);var _tr = _tq[1];var _ts = [1,_tr,_0];return _ts;};var _tt = E(_to);return _tt;});return A(_3M,[_3G,_tb,_sS]);});var _tu = T(function(){var _tv = E(_sN);var _tw = _tv[1];var _tx = function(_ty){var _tz = jsBeginPath(_tw,_ty);var _tA = _tz[1];var _tB = [1,_tA,_0];return _tB;};var _tC = E(_tx);return _tC;});return A(_3M,[_3G,_tu,_sR]);};var _tD = function(_tE,_tF,_tG,_tH,_tI,_tJ){var _tK = E(_tE);var _tL = _tK[1];var _tM = E(_tF);var _tN = _tM[1];var _tO = E(_tG);var _tP = _tO[1];var _tQ = E(_tH);var _tR = _tQ[1];var _tS = E(_tI);var _tT = _tS[1];var _tU = jsFillRect(_tL,_tN,_tP,_tR,_tT,_tJ);var _tV = _tU[1];var _tW = [1,_tV,_0];return _tW;};var _tX = function(_tY,_tZ,_u0,_4e,_h,_i){return _tD(_tY,_tZ,_u0,_4e,_h,_i);};var _u1 = function(_u2,_u3){var _u4 = E(_u2);var _u5 = _u4[1];var _u6 = jsGetContext2D(_u5,_u3);var _u7 = _u6[1];var _u8 = _u6[2];var _u9 = [1,_u8];var _ua = [1,_u7,_u9];return _ua;};var _ub = function(_h,_i){return _u1(_h,_i);};var _uc = function(_ud){return A(_6k,[_ud,_ub]);};var _ue = function(_uf,_ug,_uh){var _ui = E(_uf);var _uj = _ui[1];var _uk = E(_ug);var _ul = _uk[1];var _um = jsSetFillColor(_uj,_ul,_uh);var _un = _um[1];var _uo = [1,_un,_0];return _uo;};var _up = function(_4e,_h,_i){return _ue(_4e,_h,_i);};var _uq = function(_ur){var _us = function(_h,_i){return _up(_ur,_h,_i);};return A(_7f,[_us,toJSStr]);};var _ut = function(_uu){var _uv = E(_uu);var _uw = _uv[2];var _ux = E(_uw);return _ux;};var _uy = T(function(){return A(unCStr,["red"]);});var _uz = [1,5];var _uA = function(_uB){var _uC = E(_uB);var _uD = _uC[1];var _uE = E(_uD);return _uE;};var _uF = T(function(){return A(unCStr,["black"]);});var _uG = [1,15];var _uH = [1,40];var _uI = function(_uJ){var _uK = E(_uJ);var _uL = _uK[3];var _uM = E(_uL);return _uM;};var _uN = [1,400];var _uO = T(function(){return A(_uI,[_r7,_uN,_uG]);});var _uP = function(_uQ){var _uR = E(_uQ);var _uS = _uR[1];var _uT = [1,_uS,_uO,_uH,_uG];return _uT;};var _uU = function(_uV){var _uW = E(_uV);var _uX = _uW[1];var _uY = E(_uX);return _uY;};var _uZ = function(_v0){var _v1 = function(_v2){var _v3 = T(function(){var _v4 = T(function(){var _v5 = T(function(){var _v6 = T(function(){return A(_7f,[_uP,_uA]);});return A(_4f,[_v6,_v0]);});var _v7 = T(function(){var _v8 = T(function(){var _v9 = T(function(){var _va = T(function(){return A(_7f,[_uU,_ut]);});return A(_4f,[_va,_v0]);});var _vb = T(function(){var _vc = E(_v9);var _vd = _vc[2];var _ve = E(_vd);return _ve;});var _vf = T(function(){var _vg = E(_v9);var _vh = _vg[1];var _vi = E(_vh);return _vi;});var _vj = [1,_vf,_vb];var _vk = T(function(){var _vl = E(_vj);var _vm = _vl[2];var _vn = E(_vm);return _vn;});var _vo = T(function(){var _vp = E(_vj);var _vq = _vp[1];var _vr = E(_vq);return _vr;});return A(_sM,[_v2,_vo,_vk,_uz]);});var _vs = T(function(){return A(_uq,[_v2,_uy]);});return A(_3M,[_3G,_vs,_v8]);});var _vt = T(function(){var _vu = T(function(){var _vv = E(_v5);var _vw = _vv[4];var _vx = E(_vw);return _vx;});var _vy = T(function(){var _vz = E(_v5);var _vA = _vz[3];var _vB = E(_vA);return _vB;});var _vC = T(function(){var _vD = E(_v5);var _vE = _vD[2];var _vF = E(_vE);return _vF;});var _vG = T(function(){var _vH = E(_v5);var _vI = _vH[1];var _vJ = E(_vI);return _vJ;});return A(_tX,[_v2,_vG,_vC,_vy,_vu]);});return A(_3M,[_3G,_vt,_v7]);});var _vK = T(function(){return A(_uq,[_v2,_uF]);});return A(_3M,[_3G,_vK,_v4]);});var _vL = T(function(){return A(_7s,[_v2]);});return A(_3M,[_3G,_vL,_v3]);};var _vM = T(function(){return A(_uc,[_6B]);});return A(_3H,[_3G,_vM,_v1]);};var _vN = function(_vO,_vP){var _vQ = function(_vR){var _vS = function(_vT){var _vU = T(function(){var _vV = T(function(){return A(_7d,[_vR,_vT]);});var _vW = T(function(){var _vX = E(_vV);var _vY = _vX[2];var _vZ = E(_vY);return _vZ;});var _w0 = T(function(){var _w1 = E(_vV);var _w2 = _w1[1];var _w3 = E(_w2);return _w3;});var _w4 = [1,_w0,_vW];var _w5 = T(function(){var _w6 = T(function(){var _w7 = E(_w4);var _w8 = _w7[2];var _w9 = E(_w8);return _w9;});return A(_6R,[_vO,_w6]);});var _wa = T(function(){var _wb = T(function(){var _wc = E(_w4);var _wd = _wc[1];var _we = E(_wd);return _we;});return _uZ(_wb);});return A(_3M,[_3G,_wa,_w5]);});var _wf = T(function(){return A(_6R,[_vP,_1d]);});return A(_3M,[_3G,_wf,_vU]);};var _wg = T(function(){return A(_6I,[_vP]);});return A(_3H,[_3G,_wg,_vS]);};var _wh = T(function(){return A(_6I,[_vO]);});return A(_3H,[_3G,_wh,_vQ]);};var _wi = function(_wj){var _wk = function(_wl){var _wm = T(function(){var _wn = T(function(){var _wo = T(function(){return _vN(_wj,_wl);});var _wp = [1,20];return A(_4d,[_wp,_wo]);});var _wq = T(function(){var _wr = function(_ws){return _72(_wl,_ws);};return A(_6v,[_6B,_wr]);});return A(_3M,[_3G,_wq,_wn]);});var _wt = T(function(){var _wu = function(_ws){return _6T(_wl,_ws);};return A(_6o,[_6B,_wu]);});return A(_3M,[_3G,_wt,_wm]);};var _wv = T(function(){return A(_3Z,[_1d]);});return A(_3H,[_3G,_wv,_wk]);};var _ww = function(_wx){var _wy = E(_wx);var _wz = _wy[1];var _wA = E(_wz);return _wA;};var _wB = function(_wC){var _wD = E(_wC);var _wE = _wD[5];var _wF = E(_wE);return _wF;};var _wG = function(_wH){var _wI = E(_wH);var _wJ = _wI[2];var _wK = E(_wJ);return _wK;};var _wL = function(_wM){return [1,_wM,_wM];};var _wN = function(_wO){var _wP = E(_wO);var _wQ = _wP[2];var _wR = E(_wQ);return _wR;};var _wS = function(_wT,_wU,_wV){var _wW = T(function(){return A(_wG,[_wT,_wL]);});var _wX = T(function(){return A(_wB,[_wT,_wU,_wV]);});var _wY = _ww(_wT);var _wZ = A(_wN,[_wY,_wX,_wW]);return _wZ;};var _x0 = T(function(){return A(_wS,[_x1]);});var _x2 = function(_x3){var _x4 = E(_x3);var _x5 = _x4[3];var _x6 = E(_x5);return _x6;};var _x7 = function(_x8){var _x9 = E(_x8);var _xa = _x9[4];var _xb = E(_xa);return _xb;};var _xc = function(_xd,_xe,_xf){var _xg = T(function(){return A(_x2,[_xd,_xe]);});var _xh = T(function(){return A(_x7,[_xd,_xf]);});var _xi = _ww(_xd);var _xj = A(_wN,[_xi,_xh,_xg]);return _xj;};var _xk = T(function(){return A(_xc,[_x1]);});var _xl = function(_xm){var _xn = function(_xo){var _xp = T(function(){return A(_wG,[_x1,_xm]);});var _xq = T(function(){return A(_xm,[_xo]);});return [1,_xq,_xp];};var _xr = function(_xs){return E(_xs);};return A(_4f,[_xr,_xn]);};var _xt = function(_xu){var _xv = function(_xw){var _xx = E(_xw);var _xy = _xx[1];var _xz = _xx[2];var _xA = T(function(){return A(_xu,[_xy]);});var _xB = T(function(){var _xC = E(_xA);var _xD = _xC[2];var _xE = E(_xD);return _xE;});var _xF = T(function(){var _xG = E(_xA);var _xH = _xG[1];var _xI = E(_xH);return _xI;});var _xJ = [1,_xF,_xB];var _xK = T(function(){var _xL = T(function(){var _xM = E(_xJ);var _xN = _xM[2];var _xO = E(_xN);return _xO;});return A(_x2,[_x1,_xL]);});var _xP = T(function(){var _xQ = E(_xJ);var _xR = _xQ[1];var _xS = E(_xR);return _xS;});var _xT = [1,_xP,_xz];var _xU = [1,_xT,_xK];return _xU;};var _xV = function(_xW){return E(_xW);};return A(_4f,[_xV,_xv]);};var _xX = function(_xY){var _xZ = T(function(){var _y0 = E(_xY);var _y1 = _y0[1];var _y2 = E(_y1);return _y2;});var _y3 = T(function(){var _y4 = E(_xY);var _y5 = _y4[2];var _y6 = E(_y5);return _y6;});return [1,_y3,_xZ];};var _y7 = function(_y8,_y9){var _ya = _ww(_y8);var _yb = _ya[2];var _yc = T(function(){var _yd = T(function(){return A(_wG,[_y8,_xX]);});var _ye = T(function(){return A(_x2,[_y8,_y9]);});return A(_yb,[_ye,_yd]);});var _yf = T(function(){return A(_wG,[_y8,_xX]);});var _yg = A(_yb,[_yf,_yc]);return _yg;};var _yh = T(function(){return A(_y7,[_x1]);});var _yi = function(_yj,_yk){var _yl = function(_ym){var _yn = T(function(){return A(_yk,[_ym]);});var _yo = T(function(){var _yp = E(_yn);var _yq = _yp[2];var _yr = E(_yq);return _yr;});var _ys = T(function(){var _yt = E(_yn);var _yu = _yt[1];var _yv = E(_yu);return _yv;});var _yw = [1,_ys,_yo];var _yx = T(function(){var _yy = T(function(){var _yz = E(_yw);var _yA = _yz[1];var _yB = E(_yA);return _yB;});return A(_yj,[_yy]);});var _yC = T(function(){var _yD = E(_yx);var _yE = _yD[2];var _yF = E(_yE);return _yF;});var _yG = T(function(){var _yH = E(_yx);var _yI = _yH[1];var _yJ = E(_yI);return _yJ;});var _yK = [1,_yG,_yC];var _yL = T(function(){var _yM = T(function(){var _yN = E(_yw);var _yO = _yN[2];var _yP = E(_yO);return _yP;});var _yQ = T(function(){var _yR = E(_yK);var _yS = _yR[2];var _yT = E(_yS);return _yT;});return A(_wN,[_yU,_yQ,_yM]);});var _yV = T(function(){var _yW = E(_yK);var _yX = _yW[1];var _yY = E(_yX);return _yY;});return [1,_yV,_yL];};var _yZ = function(_z0){return E(_z0);};return A(_4f,[_yZ,_yl]);};var _z1 = function(_z2){var _z3 = E(_z2);var _z4 = _z3[1];var _z5 = E(_z4);return _z5;};var _z6 = T(function(){var _z7 = function(_z8){var _z9 = T(function(){return _z1(_yU);});return [1,_z8,_z9];};var _za = function(_zb){return E(_zb);};return A(_4f,[_za,_z7]);});var _yU = T(function(){return [1,_z6,_yi];});var _x1 = T(function(){return [1,_yU,_xl,_xt,_yh,_xk,_x0];});var _zc = function(_zd,_ze,_zf){var _zg = _ww(_zd);var _zh = A(_wN,[_zg,_zf,_ze]);return _zh;};var _zi = T(function(){return A(_zc,[_x1]);});var _zj = function(_zk){var _zl = E(_zk);var _zm = _zl[2];var _zn = E(_zm);return _zn;};var _zo = function(_zp){return E(_zp);};var _zq = function(_zr){return A(_wG,[_zr,_zo]);};var _zs = function(_zt){var _zu = function(_zv){var _zw = T(function(){var _zx = [1,_zv,_zy];var _zz = A(_zt,[_zx]);var _zA = _zz[1];var _zB = _zz[2];var _zC = E(_zA);var _zD = _zC[1];var _zE = _zC[2];var _zF = [1,_zD,_zE,_zB];return _zF;});var _zy = T(function(){var _zG = E(_zw);var _zH = _zG[2];var _zI = E(_zH);return _zI;});var _zJ = T(function(){var _zK = E(_zw);var _zL = _zK[3];var _zM = E(_zL);return _zM;});var _zN = T(function(){var _zO = E(_zw);var _zP = _zO[1];var _zQ = E(_zP);return _zQ;});var _zR = [1,_zN,_zy,_zJ];var _zS = T(function(){var _zT = T(function(){var _zU = E(_zR);var _zV = _zU[3];var _zW = E(_zV);return _zW;});return A(_zj,[_zX,_zT]);});var _zY = T(function(){var _zZ = E(_zR);var _A0 = _zZ[1];var _A1 = E(_A0);return _A1;});return [1,_zY,_zS];};var _A2 = function(_A3){return E(_A3);};return A(_4f,[_A2,_zu]);};var _zX = T(function(){return [1,_x1,_zs];});var _A4 = function(_A5,_A6,_A7){return A(_wN,[_A5,_A7,_A6]);};var _A8 = function(_A9){var _Aa = function(_Ab){var _Ac = function(_Ad){var _Ae = T(function(){return _Aa(_Ad);});var _Af = [1,_Ad,_Ab];return [1,_Af,_Ae];};var _Ag = function(_Ah){return E(_Ah);};return A(_4f,[_Ag,_Ac]);};var _Ai = function(_Aj){var _Ak = T(function(){return _Aa(_Aj);});var _Al = [1,_Aj,_A9];return [1,_Al,_Ak];};var _Am = function(_An){return E(_An);};return A(_4f,[_Am,_Ai]);};var _Ao = function(_Ap){var _Aq = E(_Ap);var _Ar = _Aq[2];var _As = E(_Ar);return _As;};var _At = function(_Au){var _Av = T(function(){return A(_wG,[_x1,_Ao]);});var _Aw = T(function(){return _A8(_Au);});return A(_A4,[_yU,_Aw,_Av]);};var _Ax = function(_Ay){var _Az = E(_Ay);var _AA = _Az[1];var _AB = E(_AA);var _AC = _AB[1];var _AD = _AB[2];var _AE = T(function(){var _AF = [1,2];return A(_sH,[_r7,_AF,_uz]);});var _AG = T(function(){var _AH = [1,2];return A(_sH,[_r7,_AH,_uz]);});var _AI = T(function(){return A(_uI,[_r7,_AD,_uz]);});var _AJ = T(function(){return A(_uI,[_r7,_AC,_uz]);});var _AK = [1,_AJ,_AI,_AG,_AE];return _AK;};var _AL = function(_AM){var _AN = E(_AM);var _AO = _AN[1];var _AP = E(_AO);return _AP;};var _AQ = function(_AR,_AS){var _AT = E(_AS);if(_AT[0]==1){var _AU = [1];}else{var _AV = _AT[1];var _AW = _AT[2];var _AX = T(function(){return _AQ(_AR,_AW);});var _AY = T(function(){return A(_AR,[_AV]);});var _AU = [2,_AY,_AX];}return _AU;};var _AZ = function(_B0,_B1){var _B2 = E(_B0);var _B3 = _B2[1];var _B4 = E(_B1);var _B5 = _B4[1];var _B6 = _B3==_B5;var _B7 = _B6?false:true;return _B7;};var _B8 = function(_B9,_Ba){var _Bb = E(_B9);var _Bc = _Bb[1];var _Bd = E(_Ba);var _Be = _Bd[1];var _Bf = _Bc==_Be;return _Bf;};var _Bg = [1,_B8,_AZ];var _Bh = function(_Bi,_Bj){var _Bk = E(_Bi);var _Bl = _Bk[1];var _Bm = E(_Bj);var _Bn = _Bm[1];var _Bo = _Bl<_Bn;return _Bo;};var _Bp = function(_Bq,_Br){var _Bs = E(_Bq);var _Bt = _Bs[1];var _Bu = E(_Br);var _Bv = _Bu[1];var _Bw = _Bt<=_Bv;return _Bw;};var _Bx = function(_By,_Bz){var _BA = E(_By);var _BB = _BA[1];var _BC = E(_Bz);var _BD = _BC[1];var _BE = _BB>_BD;return _BE;};var _BF = function(_BG,_BH){var _BI = E(_BG);var _BJ = _BI[1];var _BK = E(_BH);var _BL = _BK[1];var _BM = _BJ>=_BL;return _BM;};var _BN = function(_BO,_BP){var _BQ = E(_BO);var _BR = _BQ[1];var _BS = E(_BP);var _BT = _BS[1];var _BU = _BR<_BT;if(_BU){var _BV = [1];}else{var _BW = _BR==_BT;var _BV = _BW?[2]:[3];}return _BV;};var _BX = function(_BY,_BZ){var _C0 = E(_BY);var _C1 = _C0[1];var _C2 = E(_BZ);var _C3 = _C2[1];var _C4 = _C1<=_C3;var _C5 = _C4?E(_C2):E(_C0);return _C5;};var _C6 = function(_C7,_C8){var _C9 = E(_C7);var _Ca = _C9[1];var _Cb = E(_C8);var _Cc = _Cb[1];var _Cd = _Ca<=_Cc;var _Ce = _Cd?E(_C9):E(_Cb);return _Ce;};var _Cf = [1,_Bg,_BN,_Bh,_BF,_Bx,_Bp,_BX,_C6];var _Cg = function(_Ch){var _Ci = E(_Ch);var _Cj = _Ci[6];var _Ck = E(_Cj);return _Ck;};var _Cl = function(_Cm){var _Cn = E(_Cm);var _Co = _Cn[4];var _Cp = E(_Co);return _Cp;};var _Cq = function(_Cr,_Cs){while(1){var _Ct = E(_Cs);if(_Ct[0]==1){var _Cu = [1];}else{var _Cv = _Ct[1];var _Cw = _Ct[2];var _Cx = A(_Cr,[_Cv]);if(_Cx){var _Cy = (function(_Cr,_Cw){return T(function(){return _Cq(_Cr,_Cw);})})(_Cr,_Cw);var _Cz = [2,_Cv,_Cy];}else{_Cr=_Cr;_Cs=_Cw;continue;var _Cz = die("Unreachable!");}var _Cu = _Cz;}return _Cu;}};var _CA = function(_CB){var _CC = E(_CB);var _CD = _CC[1];var _CE = E(_CD);return _CE;};var _CF = [4];var _CG = [1];var _CH = [2];var _CI = [3];var _CJ = function(_CK,_CL){var _CM = E(_CK);var _CN = _CM[1];var _CO = E(_CN);var _CP = _CO[1];var _CQ = _CO[2];var _CR = E(_CL);var _CS = _CR[1];var _CT = _CR[2];var _CU = _CR[3];var _CV = _CR[4];var _CW = T(function(){var _CX = T(function(){return A(_CA,[_r7,_CT,_CV]);});return A(_Cl,[_Cf,_CQ,_CX]);});var _CY = [1,_CW,_CI];var _CZ = [2,_CY,_1d];var _D0 = T(function(){return A(_Cg,[_Cf,_CQ,_CT]);});var _D1 = [1,_D0,_CF];var _D2 = [2,_D1,_CZ];var _D3 = T(function(){var _D4 = T(function(){return A(_CA,[_r7,_CS,_CU]);});return A(_Cl,[_Cf,_CP,_D4]);});var _D5 = [1,_D3,_CG];var _D6 = [2,_D5,_D2];var _D7 = T(function(){return A(_Cg,[_Cf,_CP,_CS]);});var _D8 = [1,_D7,_CH];var _D9 = [2,_D8,_D6];var _Da = T(function(){var _Db = T(function(){return A(_Cq,[_AL]);});var _Dc = T(function(){return A(_AQ,[_Ao]);});return A(_7f,[_Dc,_Db]);});var _Dd = A(_4f,[_Da,_D9]);return _Dd;};var _De = function(_Df,_Dg){var _Dh = T(function(){var _Di = T(function(){var _Dj = E(_Dg);var _Dk = _Dj[3];var _Dl = E(_Dk);return _Dl;});var _Dm = T(function(){var _Dn = E(_Dg);var _Do = _Dn[1];var _Dp = E(_Do);return _Dp;});return A(_CA,[_r7,_Dm,_Di]);});var _Dq = T(function(){var _Dr = E(_Df);var _Ds = _Dr[1];var _Dt = E(_Ds);return _Dt;});var _Du = A(_Cl,[_Cf,_Dq,_Dh]);if(_Du){var _Dv = false;}else{var _Dw = T(function(){var _Dx = T(function(){var _Dy = E(_Df);var _Dz = _Dy[3];var _DA = E(_Dz);return _DA;});var _DB = T(function(){var _DC = E(_Df);var _DD = _DC[1];var _DE = E(_DD);return _DE;});return A(_CA,[_r7,_DB,_Dx]);});var _DF = T(function(){var _DG = E(_Dg);var _DH = _DG[1];var _DI = E(_DH);return _DI;});var _DJ = A(_Cl,[_Cf,_DF,_Dw]);if(_DJ){var _DK = false;}else{var _DL = T(function(){var _DM = T(function(){var _DN = E(_Dg);var _DO = _DN[4];var _DP = E(_DO);return _DP;});var _DQ = T(function(){var _DR = E(_Dg);var _DS = _DR[2];var _DT = E(_DS);return _DT;});return A(_CA,[_r7,_DQ,_DM]);});var _DU = T(function(){var _DV = E(_Df);var _DW = _DV[2];var _DX = E(_DW);return _DX;});var _DY = A(_Cl,[_Cf,_DU,_DL]);if(_DY){var _DZ = false;}else{var _E0 = T(function(){var _E1 = T(function(){var _E2 = E(_Df);var _E3 = _E2[4];var _E4 = E(_E3);return _E4;});var _E5 = T(function(){var _E6 = E(_Df);var _E7 = _E6[2];var _E8 = E(_E7);return _E8;});return A(_CA,[_r7,_E5,_E1]);});var _E9 = T(function(){var _Ea = E(_Dg);var _Eb = _Ea[2];var _Ec = E(_Eb);return _Ec;});var _Ed = A(_Cl,[_Cf,_E9,_E0]);var _DZ = _Ed?false:true;}var _DK = _DZ;}var _Dv = _DK;}return _Dv;};var _Ee = function(_Ef,_Eg){var _Eh = T(function(){return _Ax(_Eg);});var _Ei = T(function(){var _Ej = E(_Ef);var _Ek = _Ej[1];var _El = [1,_Ek,_uO,_uH,_uG];return _El;});var _Em = _De(_Ei,_Eh);if(_Em){var _En = T(function(){var _Eo = E(_Ef);var _Ep = _Eo[1];var _Eq = [1,_Ep,_uO,_uH,_uG];return _Eq;});var _Er = _CJ(_Eg,_En);}else{var _Er = [1];}return _Er;};var _Es = T(function(){return A(_zc,[_x1]);});var _Et = function(_Eu,_Ev){var _Ew = function(_Ex){var _Ey = T(function(){return A(_Eu,[_Ev,_Ex]);});var _Ez = T(function(){return _Et(_Eu,_Ey);});return [1,_Ey,_Ez];};var _EA = function(_EB){return E(_EB);};return A(_4f,[_EA,_Ew]);};var _EC = function(_ED){var _EE = E(_ED);var _EF = _EE[2];var _EG = E(_EF);return _EG;};var _EH = function(_EI,_EJ,_EK,_EL){var _EM = E(_EK);var _EN = _EM[1];var _EO = _EM[2];var _EP = E(_EL);var _EQ = _EP[1];var _ER = _EP[2];var _ES = T(function(){return A(_EC,[_EJ,_EO,_ER]);});var _ET = T(function(){return A(_EC,[_EI,_EN,_EQ]);});var _EU = [1,_ET,_ES];return _EU;};var _EV = function(_EW){var _EX = E(_EW);var _EY = _EX[3];var _EZ = E(_EY);return _EZ;};var _F0 = function(_F1,_F2,_F3){var _F4 = E(_F3);var _F5 = _F4[1];var _F6 = _F4[2];var _F7 = T(function(){return A(_EV,[_F2,_F6]);});var _F8 = T(function(){return A(_EV,[_F1,_F5]);});var _F9 = [1,_F8,_F7];return _F9;};var _Fa = function(_Fb){var _Fc = E(_Fb);var _Fd = _Fc[1];var _Fe = E(_Fd);return _Fe;};var _Ff = function(_Fg,_Fh){var _Fi = T(function(){return _Fa(_Fh);});var _Fj = T(function(){return _Fa(_Fg);});return [1,_Fj,_Fi];};var _Fk = function(_Fl,_Fm){var _Fn = function(_Fo){return _F0(_Fl,_Fm,_Fo);};var _Fp = function(_Fq,_Fo){return _EH(_Fl,_Fm,_Fq,_Fo);};var _Fr = T(function(){return _Ff(_Fl,_Fm);});return [1,_Fr,_Fp,_Fn];};var _Fs = [1,0];var _Ft = [1,_Fs,_qP,_qJ];var _Fu = function(_ws){return [1,_ws];};var _Fv = function(_Fw,_Fx,_Fy){var _Fz = function(_FA,_FB){while(1){var _FC = E(_FB);if(_FC[0]==1){var _FD = E(_FA);}else{var _FE = _FC[1];var _FF = _FC[2];var _FG = A(_Fw,[_FA,_FE]);_FA=_FG;_FB=_FF;continue;var _FH = die("Unreachable!");var _FD = _FH;}return _FD;}};return _Fz(_Fx,_Fy);};var _FI = function(_FJ,_FK){var _FL = function(_FM){var _FN = T(function(){return A(_Fv,[_FJ,_FK,_FM]);});var _FO = T(function(){return _FI(_FJ,_FN);});return [1,_FN,_FO];};var _FP = function(_FQ){return E(_FQ);};return A(_4f,[_FP,_FL]);};var _FR = [1,3];var _FS = function(_FT){var _FU = E(_FT);var _FV = _FU[4];var _FW = E(_FV);return _FW;};var _FX = [1,3];var _FY = T(function(){return A(_FS,[_r7,_FX]);});var _FZ = [1,_FR,_FY];var _G0 = function(_G1){var _G2 = E(_G1);var _G3 = _G2[5];var _G4 = E(_G3);return _G4;};var _G5 = function(_G6,_G7){var _G8 = E(_G6);var _G9 = _G8[1];var _Ga = _G8[2];var _Gb = E(_G7);switch(_Gb[0]){case 1:var _Gc = T(function(){return A(_G0,[_r7,_G9]);});var _Gd = [1,_Gc,_Ga];break;case 2:var _Ge = T(function(){var _Gf = T(function(){return A(_G0,[_r7,_G9]);});return A(_FS,[_r7,_Gf]);});var _Gd = [1,_Ge,_Ga];break;case 3:var _Gg = T(function(){return A(_G0,[_r7,_Ga]);});var _Gd = [1,_G9,_Gg];break;case 4:var _Gh = T(function(){var _Gi = T(function(){return A(_G0,[_r7,_Ga]);});return A(_FS,[_r7,_Gi]);});var _Gd = [1,_G9,_Gh];break;}return _Gd;};var _Gj = T(function(){return A(_FI,[_G5,_FZ]);});var _Gk = function(_Gl){var _Gm = E(_Gl);var _Gn = _Gm[2];var _Go = E(_Gn);return _Go;};var _Gp = [1,600];var _Gq = [1,2];var _Gr = T(function(){return A(_Gk,[_rh,_Gp,_Gq]);});var _Gs = [1,50];var _Gt = T(function(){return A(_uI,[_r7,_uN,_Gs]);});var _Gu = [1,_Gr,_Gt];var _Gv = [1,_Gu];var _Gw = T(function(){var _Gx = T(function(){var _Gy = T(function(){var _Gz = T(function(){return A(_zq,[_x1]);});var _GA = T(function(){return A(_wG,[_x1,_Fu]);});return A(_Es,[_GA,_Gz]);});var _GB = T(function(){var _GC = T(function(){var _GD = T(function(){var _GE = function(_GF){var _GG = E(_GF);var _GH = _GG[1];var _GI = _GG[2];var _GJ = E(_GI);var _GK = E(_GH);return _GK;};return A(_wG,[_x1,_GE]);});var _GL = T(function(){var _GM = T(function(){var _GN = T(function(){var _GO = T(function(){var _GP = E(_Gv);var _GQ = _GP[1];var _GR = E(_GQ);return _GR;});var _GS = T(function(){var _GT = A(_Fk,[_Ft,_Ft]);var _GU = _EC(_GT);return _GU;});return A(_Et,[_GS,_GO]);});var _GV = T(function(){var _GW = function(_GX){return E(_GX);};return A(_wG,[_x1,_GW]);});return A(_Es,[_GV,_GN]);});return A(_x2,[_x1,_GM]);});return A(_Es,[_GL,_GD]);});var _GY = T(function(){var _GZ = function(_H0){return [1,_H0,_0];};return A(_wG,[_x1,_GZ]);});return A(_Es,[_GY,_GC]);});return A(_Es,[_GB,_Gy]);});var _H1 = T(function(){var _H2 = T(function(){var _H3 = T(function(){var _H4 = function(_H5){var _H6 = E(_H5);var _H7 = _H6[1];var _H8 = _H6[2];var _H9 = E(_H8);var _Ha = E(_H7);return _Ha;};return A(_wG,[_x1,_H4]);});var _Hb = T(function(){var _Hc = T(function(){var _Hd = T(function(){var _He = function(_Hf){return E(_Hf);};return A(_wG,[_x1,_He]);});return A(_Es,[_Hd,_Gj]);});return A(_x2,[_x1,_Hc]);});return A(_Es,[_Hb,_H3]);});var _Hg = T(function(){var _Hh = function(_Hi){return [1,_Hi,_0];};return A(_wG,[_x1,_Hh]);});return A(_Es,[_Hg,_H2]);});return A(_Es,[_H1,_Gx]);});var _Hj = T(function(){var _Hk = function(_Hl){return E(_Hl);};return A(_wG,[_x1,_Hk]);});var _Hm = T(function(){return A(_Es,[_Hj,_Gw]);});var _Hn = function(_Ho){var _Hp = E(_Ho);var _Hq = _Hp[3];var _Hr = E(_Hq);return _Hr;};var _Hs = function(_Ht){var _Hu = E(_Ht);var _Hv = _Hu[5];var _Hw = E(_Hv);return _Hw;};var _Hx = function(_Hy){var _Hz = E(_Hy);var _HA = _Hz[1];var _HB = E(_HA);var _HC = _HB[1];var _HD = _HB[2];var _HE = T(function(){return A(_Hn,[_Cf,_HD,_uz]);});var _HF = [1,_HE,_CI];var _HG = [2,_HF,_1d];var _HH = T(function(){var _HI = T(function(){return A(_uI,[_r7,_Gp,_uz]);});return A(_Hs,[_Cf,_HC,_HI]);});var _HJ = [1,_HH,_CH];var _HK = [2,_HJ,_HG];var _HL = T(function(){return A(_Hn,[_Cf,_HC,_uz]);});var _HM = [1,_HL,_CG];var _HN = [2,_HM,_HK];var _HO = T(function(){var _HP = T(function(){return A(_Cq,[_AL]);});var _HQ = T(function(){return A(_AQ,[_Ao]);});return A(_7f,[_HQ,_HP]);});var _HR = A(_4f,[_HO,_HN]);return _HR;};var _HS = T(function(){return A(_zc,[_x1]);});var _HT = function(_HU){var _HV = E(_HU);var _HW = _HV[7];var _HX = E(_HW);return _HX;};var _HY = function(_HZ){var _I0 = E(_HZ);var _I1 = _I0[8];var _I2 = E(_I1);return _I2;};var _I3 = function(_I4,_I5,_I6,_I7){var _I8 = E(_I6);var _I9 = _I8[1];var _Ia = _I8[2];var _Ib = function(_Ic,_Id){var _Ie = T(function(){var _If = T(function(){return A(_CA,[_I4,_Ic,_Id]);});var _Ig = T(function(){return A(_HT,[_I5,_I9]);});return A(_4f,[_Ig,_If]);});var _Ih = T(function(){return A(_HY,[_I5,_Ia]);});return A(_4f,[_Ih,_Ie]);};var _Ii = _Et(_Ib,_I7);return _Ii;};var _Ij = function(_ws){return [1,_ws];};var _Ik = [1,2];var _Il = T(function(){return A(_uI,[_r7,_Gp,_uH]);});var _Im = T(function(){return A(_Gk,[_rh,_Il,_Ik]);});var _In = [1,_Im];var _Io = T(function(){return A(_zc,[_x1]);});var _Ip = function(_Iq){var _Ir = E(_Iq);var _Is = _Ir[1];var _It = E(_Is);return _It;};var _Iu = false;var _Iv = function(_Iw){var _Ix = E(_Iw);return _Ix?false:true;};var _Iy = function(_Iz,_IA){var _IB = T(function(){return A(_Ip,[_IC,_Iz,_IA]);});return A(_Iv,[_IB]);};var _ID = function(_IE,_IF){var _IG = E(_IE);var _IH = _IG[1];var _II = E(_IF);var _IJ = _II[1];var _IK = _IH==_IJ;return _IK;};var _IL = function(_IM,_IN){var _IO = E(_IM);var _IP = _IO[1];var _IQ = E(_IN);var _IR = _IQ[1];var _IS = _IP!=_IR;return _IS;};var _IT = [1,_ID,_IL];var _IU = function(_IV,_IW){var _IX = E(_IV);if(_IX[0]==1){var _IY = _IX[1];var _IZ = E(_IW);if(_IZ[0]==1){var _J0 = _IZ[1];var _J1 = A(_Ip,[_IT,_IY,_J0]);}else{var _J1 = false;}var _J2 = _J1;}else{var _J3 = _IX[1];var _J4 = E(_IW);if(_J4[0]==1){var _J5 = false;}else{var _J6 = _J4[1];var _J5 = A(_Ip,[_IT,_J3,_J6]);}var _J2 = _J5;}return _J2;};var _IC = T(function(){return [1,_IU,_Iy];});var _J7 = function(_J8){var _J9 = function(_Ja,_Jb){var _Jc = [1,_J8];var _Jd = A(_Ip,[_IC,_Jb,_Jc]);if(_Jd){var _Je = false;}else{var _Jf = [2,_J8];var _Jg = A(_Ip,[_IC,_Jb,_Jf]);var _Je = _Jg?true:E(_Ja);}return _Je;};return A(_FI,[_J9,_Iu]);};var _Jh = [1,37];var _Ji = [1,5];var _Jj = [1,39];var _Jk = T(function(){var _Jl = T(function(){var _Jm = T(function(){var _Jn = T(function(){return A(_zq,[_x1]);});var _Jo = T(function(){var _Jp = function(_Jq){var _Jr = E(_Jq);var _Js = _Jr[1];var _Jt = _Jr[2];var _Ju = E(_Js);if(_Ju){var _Jv = A(_FS,[_r7,_Ji]);}else{var _Jw = E(_Jt);var _Jv = _Jw?E(_Ji):[1,0];}return _Jv;};return A(_wG,[_x1,_Jp]);});return A(_Io,[_Jo,_Jn]);});var _Jx = T(function(){var _Jy = T(function(){var _Jz = T(function(){var _JA = function(_JB){var _JC = E(_JB);var _JD = _JC[1];var _JE = _JC[2];var _JF = [1,_JE,_JD];return _JF;};return A(_wG,[_x1,_JA]);});var _JG = T(function(){var _JH = T(function(){var _JI = T(function(){return _J7(_Jj);});var _JJ = T(function(){var _JK = function(_JL){return E(_JL);};return A(_wG,[_x1,_JK]);});return A(_Io,[_JJ,_JI]);});return A(_x2,[_x1,_JH]);});return A(_Io,[_JG,_Jz]);});var _JM = T(function(){var _JN = function(_JO){return E(_JO);};return A(_wG,[_x1,_JN]);});return A(_Io,[_JM,_Jy]);});return A(_Io,[_Jx,_Jm]);});var _JP = T(function(){var _JQ = T(function(){var _JR = T(function(){var _JS = function(_JT){var _JU = E(_JT);var _JV = _JU[1];var _JW = _JU[2];var _JX = [1,_JW,_JV];return _JX;};return A(_wG,[_x1,_JS]);});var _JY = T(function(){var _JZ = T(function(){var _K0 = T(function(){return _J7(_Jh);});var _K1 = T(function(){var _K2 = function(_K3){return E(_K3);};return A(_wG,[_x1,_K2]);});return A(_Io,[_K1,_K0]);});return A(_x2,[_x1,_JZ]);});return A(_Io,[_JY,_JR]);});var _K4 = T(function(){var _K5 = function(_K6){return [1,_K6,_K6];};return A(_wG,[_x1,_K5]);});return A(_Io,[_K4,_JQ]);});return A(_Io,[_JP,_Jl]);});var _K7 = T(function(){var _K8 = function(_K9){return E(_K9);};return A(_wG,[_x1,_K8]);});var _Ka = T(function(){return A(_Io,[_K7,_Jk]);});var _Kb = T(function(){var _Kc = T(function(){var _Kd = T(function(){var _Ke = T(function(){return A(_zq,[_x1]);});var _Kf = T(function(){return A(_wG,[_x1,_Ij]);});return A(_HS,[_Kf,_Ke]);});var _Kg = T(function(){var _Kh = T(function(){var _Ki = T(function(){var _Kj = function(_Kk){var _Kl = E(_Kk);var _Km = _Kl[1];var _Kn = _Kl[2];var _Ko = E(_Kn);var _Kp = E(_Km);return _Kp;};return A(_wG,[_x1,_Kj]);});var _Kq = T(function(){var _Kr = T(function(){var _Ks = T(function(){var _Kt = T(function(){var _Ku = E(_In);var _Kv = _Ku[1];var _Kw = E(_Kv);return _Kw;});var _Kx = T(function(){return A(_uI,[_r7,_Gp,_uH]);});var _Ky = [1,0];var _Kz = [1,_Ky,_Kx];return A(_I3,[_r7,_Cf,_Kz,_Kt]);});var _KA = T(function(){var _KB = function(_KC){return E(_KC);};return A(_wG,[_x1,_KB]);});return A(_HS,[_KA,_Ks]);});return A(_x2,[_x1,_Kr]);});return A(_HS,[_Kq,_Ki]);});var _KD = T(function(){var _KE = function(_KF){return [1,_KF,_0];};return A(_wG,[_x1,_KE]);});return A(_HS,[_KD,_Kh]);});return A(_HS,[_Kg,_Kd]);});var _KG = T(function(){var _KH = T(function(){var _KI = T(function(){var _KJ = function(_KK){var _KL = E(_KK);var _KM = _KL[1];var _KN = _KL[2];var _KO = E(_KN);var _KP = E(_KM);return _KP;};return A(_wG,[_x1,_KJ]);});var _KQ = T(function(){var _KR = T(function(){var _KS = T(function(){var _KT = function(_KU){return E(_KU);};return A(_wG,[_x1,_KT]);});return A(_HS,[_KS,_Ka]);});return A(_x2,[_x1,_KR]);});return A(_HS,[_KQ,_KI]);});var _KV = T(function(){var _KW = function(_KX){return [1,_KX,_0];};return A(_wG,[_x1,_KW]);});return A(_HS,[_KV,_KH]);});return A(_HS,[_KG,_Kc]);});var _KY = T(function(){var _KZ = function(_L0){return E(_L0);};return A(_wG,[_x1,_KZ]);});var _L1 = T(function(){return A(_HS,[_KY,_Kb]);});var _L2 = T(function(){var _L3 = T(function(){var _L4 = T(function(){var _L5 = T(function(){return A(_zq,[_x1]);});var _L6 = T(function(){var _L7 = function(_L8){var _L9 = E(_L8);var _La = _L9[1];var _Lb = _L9[2];var _Lc = [1,_La,_Lb];return _Lc;};return A(_wG,[_x1,_L7]);});return A(_zi,[_L6,_L5]);});var _Ld = T(function(){var _Le = T(function(){var _Lf = T(function(){var _Lg = function(_Lh){var _Li = E(_Lh);var _Lj = _Li[1];var _Lk = _Li[2];var _Ll = [1,_Lk,_Lj];return _Ll;};return A(_wG,[_x1,_Lg]);});var _Lm = T(function(){var _Ln = T(function(){var _Lo = T(function(){var _Lp = T(function(){var _Lq = T(function(){var _Lr = function(_Ls){return E(_Ls);};return A(_wG,[_x1,_Lr]);});var _Lt = T(function(){var _Lu = T(function(){var _Lv = T(function(){var _Lw = T(function(){var _Lx = T(function(){var _Ly = function(_Lz){var _LA = E(_Lz);var _LB = _LA[1];var _LC = _LA[2];var _LD = [1,_LC,_LB];return _LD;};return A(_wG,[_x1,_Ly]);});var _LE = T(function(){var _LF = T(function(){var _LG = T(function(){return A(_At,[_Gv]);});var _LH = T(function(){var _LI = function(_LJ){return E(_LJ);};return A(_wG,[_x1,_LI]);});return A(_zi,[_LH,_LG]);});return A(_x2,[_x1,_LF]);});return A(_zi,[_LE,_Lx]);});var _LK = T(function(){var _LL = function(_LM){return [1,_LM,_LM];};return A(_wG,[_x1,_LL]);});return A(_zi,[_LK,_Lw]);});var _LN = T(function(){var _LO = T(function(){var _LP = T(function(){var _LQ = function(_LR){var _LS = E(_LR);var _LT = _LS[1];var _LU = _LS[2];var _LV = E(_LU);var _LW = E(_LT);return _LW;};return A(_wG,[_x1,_LQ]);});var _LX = T(function(){var _LY = T(function(){var _LZ = T(function(){var _M0 = function(_M1){return E(_M1);};return A(_wG,[_x1,_M0]);});return A(_zi,[_LZ,_Hm]);});return A(_x2,[_x1,_LY]);});return A(_zi,[_LX,_LP]);});var _M2 = T(function(){var _M3 = function(_M4){return [1,_M4,_0];};return A(_wG,[_x1,_M3]);});return A(_zi,[_M2,_LO]);});return A(_zi,[_LN,_Lv]);});var _M5 = T(function(){var _M6 = function(_M7){var _M8 = E(_M7);var _M9 = _M8[1];var _Ma = _M8[2];var _Mb = T(function(){return _Ee(_M9,_Ma);});var _Mc = T(function(){return _Hx(_Ma);});var _Md = A(_1o,[_Mc,_Mb]);return _Md;};return A(_wG,[_x1,_M6]);});return A(_zi,[_M5,_Lu]);});return A(_zi,[_Lt,_Lq]);});var _Me = T(function(){var _Mf = function(_Mg){return E(_Mg);};return A(_wG,[_x1,_Mf]);});return A(_zi,[_Me,_Lp]);});return A(_zj,[_zX,_Lo]);});return A(_x2,[_x1,_Ln]);});return A(_zi,[_Lm,_Lf]);});var _Mh = T(function(){var _Mi = function(_Mj){return [1,_Mj,_Mj];};return A(_wG,[_x1,_Mi]);});return A(_zi,[_Mh,_Le]);});return A(_zi,[_Ld,_L4]);});var _Mk = T(function(){var _Ml = T(function(){var _Mm = T(function(){var _Mn = function(_Mo){var _Mp = E(_Mo);var _Mq = _Mp[1];var _Mr = _Mp[2];var _Ms = E(_Mr);var _Mt = E(_Mq);return _Mt;};return A(_wG,[_x1,_Mn]);});var _Mu = T(function(){var _Mv = T(function(){var _Mw = T(function(){var _Mx = function(_My){return E(_My);};return A(_wG,[_x1,_Mx]);});return A(_zi,[_Mw,_L1]);});return A(_x2,[_x1,_Mv]);});return A(_zi,[_Mu,_Mm]);});var _Mz = T(function(){var _MA = function(_MB){return [1,_MB,_0];};return A(_wG,[_x1,_MA]);});return A(_zi,[_Mz,_Ml]);});return A(_zi,[_Mk,_L3]);});var _MC = T(function(){var _MD = function(_ME){return E(_ME);};return A(_wG,[_x1,_MD]);});var _MF = T(function(){return A(_zi,[_MC,_L2]);});var _MG = T(function(){return A(_3Z,[_MF]);});var _MH = T(function(){return A(_3H,[_3G,_MG,_wi]);});var _MI = T(function(){return A(_g,[_MH]);});
E(E(_MI)(0));
