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

// Implementation of stable names.
// Unlike native GHC, the garbage collector isn't going to move data around
// in a way that we can detect, so each object could serve as its own stable
// name if it weren't for the fact we can't turn a JS reference into an
// integer.
// So instead, each object has a unique integer attached to it, which serves
// as its stable name.

var __next_stable_name = 1;

function makeStableName(x, _world) {
    if(!x.stableName) {
        x.stableName = __next_stable_name;
        __next_stable_name += 1;
    }
    return [1,_world,x.stableName];
}

function eqStableName(x, y) {
    return (x == y) ? 1 : 0;
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

function jsMoveTo(context, x, y, _) {
	context.moveTo(x,y);
	return [1,0];
}

function jsLineTo(context, x, y, _) {
	context.lineTo(x,y);
	return [1,0];
}

function jsFillRect(context, x, y, width, height, _) {
	context.fillRect(x,y,width,height);
	return [1,0];
}

function jsFillText(context, text, x, y, _) {
	context.fillText(text,x,y);
	return [1,0];
}

function jsSetFillColor(context, red, green, blue, alpha, _) {
	context.fillStyle = "rgba(" + Math.round(red * 255.0) + "," + Math.round(green * 255.0) + "," + Math.round(blue * 255.0) + "," + alpha + ")";
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

function jsAlertDouble(d, _) {
	alert(d);
	return [1,0];
}

var _0 = [1];var _1 = function(_2,_3){var _4 = E(_2);var _5 = [1,_4];var _6 = _5[1];var _7 = jsSetOnLoad(_6,_3);var _8 = _7[1];var _9 = [1,_8,_0];return _9;};var _a = function(_b,_c){return _1(_b,_c);};var _d = function(_e,_f,_g){var _h = E(_e);var _i = _h[1];var _j = E(_f);var _k = [1,_j];var _l = _k[1];var _m = jsSetInterval(_i,_l,_g);var _n = _m[1];var _o = [1,_n,_0];return _o;};var _p = function(_q,_b,_c){return _d(_q,_b,_c);};var _r = "keydown";var _s = [1,_r];var _t = function(_u,_v){var _w = E(_u);if(_w[0]==1){var _x = E(_v);}else{var _y = _w[1];var _z = _w[2];var _A = T(function(){return _t(_z,_v);});var _x = [2,_y,_A];}return _x;};var _B = T(function(){return unCStr(" could be found!");});var _C = function(_D){var _E = T(function(){return _t(_D,_B);});var _F = unAppCStr("No element with ID ",_E);var _G = err(_F);return _G;};var _H = function(_I,_J,_K){var _L = [1,_J];var _M = _L[1];var _N = toJSStr(_I);var _O = _N[1];var _P = jsFind(_O,_K);var _Q = _P[1];var _R = _P[2];var _S = [1,_R];var _T = _S[1];var _U = E(_T);if(_U[0]==1){var _V = _C(_I);}else{var _W = _U[1];var _X = E(_W);var _Y = _X[1];var _Z = E(_s);var _10 = _Z[1];var _11 = E(_J);var _12 = jsSetCB(_Y,_10,_M,_Q);var _13 = _12[1];var _14 = _12[2];var _15 = T(function(){var _16 = E(_14);return _16?true:false;});var _17 = [1,_13,_15];var _V = _17;}return _V;};var _18 = function(_q,_b,_c){return _H(_q,_b,_c);};var _19 = "keyup";var _1a = [1,_19];var _1b = function(_1c,_1d,_1e){var _1f = [1,_1d];var _1g = _1f[1];var _1h = toJSStr(_1c);var _1i = _1h[1];var _1j = jsFind(_1i,_1e);var _1k = _1j[1];var _1l = _1j[2];var _1m = [1,_1l];var _1n = _1m[1];var _1o = E(_1n);if(_1o[0]==1){var _1p = _C(_1c);}else{var _1q = _1o[1];var _1r = E(_1q);var _1s = _1r[1];var _1t = E(_1a);var _1u = _1t[1];var _1v = E(_1d);var _1w = jsSetCB(_1s,_1u,_1g,_1k);var _1x = _1w[1];var _1y = _1w[2];var _1z = T(function(){var _1A = E(_1y);return _1A?true:false;});var _1B = [1,_1x,_1z];var _1p = _1B;}return _1p;};var _1C = function(_q,_b,_c){return _1b(_q,_b,_c);};var _1D = [3];var _1E = [1,1];var _1F = [1,'-'];var _1G = function(_1H,_1I){while(1){var _1J = _1H<10;if(_1J){var _1K = 48+_1H|0;var _1L = String.fromCharCode(_1K);var _1M = [1,_1L];var _1N = [2,_1M,_1I];var _1O = _1N;}else{var _1P = _1H%10;var _1Q = 48+_1P|0;var _1R = String.fromCharCode(_1Q);var _1S = [1,_1R];var _1T = [2,_1S,_1I];var _1U = quot(_1H,10);_1H=_1U;_1I=_1T;continue;var _1V = die("Unreachable!");var _1O = _1V;}return _1O;}};var _1W = function(_1X,_1Y){var _1Z = _1X<0;if(_1Z){var _20 = E(_1X);if(_20==(-2147483648)){var _21 = T(function(){var _22 = T(function(){return _1G(8,_1Y);});return _1G(214748364,_22);});var _23 = [2,_1F,_21];}else{var _24 = T(function(){var _25 = -_20;var _26 = _1G(_25,_1Y);return _26;});var _23 = [2,_1F,_24];}var _27 = _23;}else{var _27 = _1G(_1X,_1Y);}return _27;};var _28 = [1,')'];var _29 = [1,'('];var _2a = function(_2b,_2c,_2d){var _2e = _2c<0;if(_2e){var _2f = _2b>6;if(_2f){var _2g = T(function(){var _2h = [2,_28,_2d];return _1W(_2c,_2h);});var _2i = [2,_29,_2g];}else{var _2i = _1W(_2c,_2d);}var _2j = _2i;}else{var _2j = _1W(_2c,_2d);}return _2j;};var _2k = [1];var _2l = function(_2m,_2n){var _2o = E(_2m);var _2p = _2o[1];var _2q = jsClear(_2p,_2n);var _2r = _2q[1];var _2s = [1,_2r,_0];return _2s;};var _2t = function(_b,_c){return _2l(_b,_c);};var _2u = function(_2v,_2w,_2x,_2y,_2z){var _2A = jsBeginPath(_2v,_2z);var _2B = _2A[1];var _2C = jsArc(_2v,_2w,_2x,_2y,0,6.283185307179586,1,_2B);var _2D = _2C[1];var _2E = jsClosePath(_2v,_2D);var _2F = _2E[1];var _2G = jsFill(_2v,_2F);var _2H = _2G[1];var _2I = [1,_2H,_0];return _2I;};var _2J = function(_2K,_2L,_2M,_2N,_2O){var _2P = E(_2K);var _2Q = _2P[1];var _2R = E(_2L);var _2S = _2R[1];var _2T = E(_2M);var _2U = _2T[1];var _2V = E(_2N);var _2W = _2V[1];var _2X = _2u(_2Q,_2S,_2U,_2W,_2O);return _2X;};var _2Y = function(_2Z,_30,_q,_b,_c){return _2J(_2Z,_30,_q,_b,_c);};var _31 = function(_32,_33,_34,_35,_36,_37,_38){var _39 = jsBeginPath(_32,_38);var _3a = _39[1];var _3b = _33+_37;var _3c = jsMoveTo(_32,_3b,_34,_3a);var _3d = _3c[1];var _3e = _33+_35;var _3f = _3e-_37;var _3g = jsLineTo(_32,_3f,_34,_3d);var _3h = _3g[1];var _3i = _34+_37;var _3j = _3e-_37;var _3k = jsArc(_32,_3j,_3i,_37,4.71238898038469,6.283185307179586,0,_3h);var _3l = _3k[1];var _3m = _34+_36;var _3n = _3m-_37;var _3o = jsLineTo(_32,_3e,_3n,_3l);var _3p = _3o[1];var _3q = _3m-_37;var _3r = _3e-_37;var _3s = jsArc(_32,_3r,_3q,_37,0,1.5707963267948966,0,_3p);var _3t = _3s[1];var _3u = _33+_37;var _3v = jsLineTo(_32,_3u,_3m,_3t);var _3w = _3v[1];var _3x = _3m-_37;var _3y = _33+_37;var _3z = jsArc(_32,_3y,_3x,_37,1.5707963267948966,3.141592653589793,0,_3w);var _3A = _3z[1];var _3B = _34+_37;var _3C = jsLineTo(_32,_33,_3B,_3A);var _3D = _3C[1];var _3E = _34+_37;var _3F = _33+_37;var _3G = jsArc(_32,_3F,_3E,_37,3.141592653589793,4.71238898038469,0,_3D);var _3H = _3G[1];var _3I = jsClosePath(_32,_3H);var _3J = _3I[1];var _3K = jsFill(_32,_3J);var _3L = _3K[1];var _3M = [1,_3L,_0];return _3M;};var _3N = function(_3O,_3P,_3Q,_3R,_3S,_3T,_3U){var _3V = E(_3O);var _3W = _3V[1];var _3X = E(_3P);var _3Y = _3X[1];var _3Z = E(_3Q);var _40 = _3Z[1];var _41 = E(_3R);var _42 = _41[1];var _43 = E(_3S);var _44 = _43[1];var _45 = E(_3T);var _46 = _45[1];var _47 = _31(_3W,_3Y,_40,_42,_44,_46,_3U);return _47;};var _48 = function(_49,_4a,_2Z,_30,_q,_b,_c){return _3N(_49,_4a,_2Z,_30,_q,_b,_c);};var _4b = function(_4c,_4d,_4e,_4f,_4g){var _4h = E(_4c);var _4i = _4h[1];var _4j = E(_4d);var _4k = _4j[1];var _4l = E(_4e);var _4m = _4l[1];var _4n = E(_4f);var _4o = _4n[1];var _4p = jsFillText(_4i,_4k,_4m,_4o,_4g);var _4q = _4p[1];var _4r = [1,_4q,_0];return _4r;};var _4s = function(_2Z,_30,_q,_b,_c){return _4b(_2Z,_30,_q,_b,_c);};var _4t = function(_4u,_4v){var _4w = T(function(){return toJSStr(_4v);});return A(_4s,[_4u,_4w]);};var _4x = function(_4y,_4z,_4A,_4B,_4C,_4D){var _4E = jsSetFillColor(_4y,_4z,_4A,_4B,_4C,_4D);var _4F = _4E[1];var _4G = [1,_4F,_0];return _4G;};var _4H = function(_4I,_4J,_4K){var _4L = E(_4I);var _4M = _4L[1];var _4N = E(_4J);var _4O = _4N[1];var _4P = _4N[2];var _4Q = _4N[3];var _4R = _4N[4];var _4S = E(_4O);var _4T = _4S[1];var _4U = E(_4P);var _4V = _4U[1];var _4W = E(_4Q);var _4X = _4W[1];var _4Y = E(_4R);var _4Z = _4Y[1];var _50 = _4x(_4M,_4T,_4V,_4X,_4Z,_4K);return _50;};var _51 = function(_q,_b,_c){return _4H(_q,_b,_c);};var _52 = T(function(){return unCStr("Prelude.(!!): negative index\n");});var _53 = T(function(){return err(_52);});var _54 = T(function(){return unCStr("Prelude.(!!): index too large\n");});var _55 = T(function(){return err(_54);});var _56 = function(_57,_58){while(1){var _59 = E(_57);if(_59[0]==1){var _5a = E(_55);}else{var _5b = _59[1];var _5c = _59[2];var _5d = E(_58);if(_5d){var _5e = _5d-1|0;_57=_5c;_58=_5e;continue;var _5f = die("Unreachable!");var _5g = _5f;}else{var _5g = E(_5b);}var _5a = _5g;}return _5a;}};var _5h = [1,5];var _5i = [1,20];var _5j = [1,60];var _5k = [1,0];var _5l = [1,0.5];var _5m = [1,_5k,_5l,_5k,_1E];var _5n = [2,_5m,_2k];var _5o = T(function(){return _56(_5n,0);});var _5p = [1,_5k,_5k,_5l,_1E];var _5q = [2,_5p,_2k];var _5r = [1,_5k,_5k,_1E,_1E];var _5s = [2,_5r,_5q];var _5t = T(function(){return _56(_5s,0);});var _5u = function(_5v,_5w,_5x,_5y,_5z,_5A){var _5B = E(_5z);if(_5B[0]==1){var _5C = _5B[1];var _5D = T(function(){var _5E = E(_5C);var _5F = _5E[1];var _5G = _5F-1|0;var _5H = _5G<0;if(_5H){var _5I = E(_53);}else{var _5J = E(_5w);var _5I = _5J[0]==1?_56(_5s,_5G):_56(_5n,_5G);}return _5I;});var _5K = A(_51,[_5v,_5D,_5A]);var _5L = _5K[1];var _5M = A(_48,[_5v,_5x,_5y,_5j,_5i,_5h,_5L]);var _5N = _5M;}else{var _5O = _5B[1];var _5P = T(function(){var _5Q = E(_5w);if(_5Q[0]==1){var _5R = E(_5t);var _5S = _5R[1];var _5T = _5R[2];var _5U = _5R[3];var _5V = [1,_5S,_5T,_5U,_5O];var _5W = _5V;}else{var _5X = E(_5o);var _5Y = _5X[1];var _5Z = _5X[2];var _60 = _5X[3];var _61 = [1,_5Y,_5Z,_60,_5O];var _5W = _61;}return _5W;});var _62 = A(_51,[_5v,_5P,_5A]);var _63 = _62[1];var _64 = A(_48,[_5v,_5x,_5y,_5j,_5i,_5h,_63]);var _5N = _64;}return _5N;};var _65 = [1,_1E,_5k,_5k,_1E];var _66 = [1,_5k,_5l,_5k,_1E];var _67 = [1,3];var _68 = function(_69,_6a){var _6b = toJSStr(_69);var _6c = _6b[1];var _6d = jsFind(_6c,_6a);var _6e = _6d[1];var _6f = _6d[2];var _6g = [1,_6f];var _6h = _6g[1];var _6i = E(_6h);if(_6i[0]==1){var _6j = _C(_69);}else{var _6k = _6i[1];var _6l = E(_6k);var _6m = _6l[1];var _6n = jsGetContext2D(_6m,_6e);var _6o = _6n[1];var _6p = _6n[2];var _6q = [1,_6p];var _6r = [1,_6o,_6q];var _6j = _6r;}return _6j;};var _6s = function(_b,_c){return _68(_b,_c);};var _6t = T(function(){return unCStr("canvas4");});var _6u = T(function(){return A(_6s,[_6t]);});var _6v = [1,200];var _6w = [1,_5k,_5k,_5k,_1E];var _6x = [1,10];var _6y = [1,_5k,_5k,_5k,_1E];var _6z = [1,_5k,_5k,_5k,_1E];var _6A = [1,15];var _6B = [1,7];var _6C = [1,50];var _6D = [1,385];var _6E = function(_6F,_6G){var _6H = E(_6F);if(_6H[0]==1){var _6I = _6H[1];var _6J = _6H[2];var _6K = _6H[3];var _6L = _6H[4];var _6M = _6H[5];var _6N = A(_6u,[_6G]);var _6O = _6N[1];var _6P = _6N[2];var _6Q = A(_2t,[_6P,_6O]);var _6R = _6Q[1];var _6S = A(_51,[_6P,_6z,_6R]);var _6T = _6S[1];var _6U = T(function(){var _6V = E(_6I);var _6W = _6V[1];var _6X = E(_6W);return _6X;});var _6Y = A(_48,[_6P,_6U,_6D,_6C,_6A,_6B,_6T]);var _6Z = _6Y[1];var _70 = function(_71,_72){while(1){var _73 = E(_71);if(_73[0]==1){var _74 = [1,_72,_0];}else{var _75 = _73[1];var _76 = _73[2];var _77 = E(_75);var _78 = _77[1];var _79 = _77[2];var _7a = _77[3];var _7b = E(_79);var _7c = _7b[1];var _7d = _7b[2];var _7e = _5u(_6P,_78,_7c,_7d,_7a,_72);var _7f = _7e[1];_71=_76;_72=_7f;continue;var _7g = die("Unreachable!");var _74 = _7g;}return _74;}};var _7h = _70(_6L,_6Z);var _7i = _7h[1];var _7j = function(_7k,_7l){while(1){var _7m = E(_7k);if(_7m[0]==1){var _7n = [1,_7l,_0];}else{var _7o = _7m[1];var _7p = _7m[2];var _7q = E(_7o);var _7r = _7q[1];var _7s = E(_7r);var _7t = _7s[1];var _7u = _7s[2];var _7v = A(_51,[_6P,_66,_7l]);var _7w = _7v[1];var _7x = A(_2Y,[_6P,_7t,_7u,_67,_7w]);var _7y = _7x[1];_7k=_7p;_7l=_7y;continue;var _7z = die("Unreachable!");var _7n = _7z;}return _7n;}};var _7A = _7j(_6M,_7i);var _7B = _7A[1];var _7C = A(_51,[_6P,_65,_7B]);var _7D = _7C[1];var _7E = T(function(){var _7F = E(_6K);var _7G = _7F[1];var _7H = E(_7G);return _7H;});var _7I = T(function(){var _7J = E(_7E);var _7K = _7J[2];var _7L = E(_7K);return _7L;});var _7M = T(function(){var _7N = E(_7E);var _7O = _7N[1];var _7P = E(_7O);return _7P;});var _7Q = A(_2Y,[_6P,_7M,_7I,_5h,_7D]);var _7R = _7Q[1];var _7S = A(_51,[_6P,_6y,_7R]);var _7T = _7S[1];var _7U = T(function(){var _7V = T(function(){var _7W = E(_6J);var _7X = _7W[1];var _7Y = E(_7X);var _7Z = _7Y[1];var _80 = _2a(0,_7Z,_2k);return _80;});return unAppCStr("Ammo: ",_7V);});var _81 = A(_4t,[_6P,_7U,_5k,_6x,_7T]);var _82 = _81;}else{var _83 = _6H[1];var _84 = A(_6u,[_6G]);var _85 = _84[1];var _86 = _84[2];var _87 = A(_2t,[_86,_85]);var _88 = _87[1];var _89 = A(_51,[_86,_6w,_88]);var _8a = _89[1];var _8b = A(_4t,[_86,_83,_6v,_6v,_8a]);var _82 = _8b;}return _82;};var _8c = T(function(){return unCStr("base");});var _8d = T(function(){return unCStr("Control.Exception.Base");});var _8e = T(function(){return unCStr("PatternMatchFail");});var _8f = [1,1.605959309876327e19,1.3945565038419476e19,_8c,_8d,_8e];var _8g = [1,1.605959309876327e19,1.3945565038419476e19,_8f,_2k];var _8h = function(_8i){return E(_8g);};var _8j = T(function(){return unCStr("Maybe.fromJust: Nothing");});var _8k = T(function(){return err(_8j);});var _8l = function(_8m,_8n,_8o){var _8p = T(function(){var _8q = A(_8m,[_8o]);var _8r = _8q[1];var _8s = _8q[2];var _8t = T(function(){var _8u = E(_8p);if(_8u[0]==1){var _8v = E(_8k);}else{var _8w = _8u[1];var _8v = E(_8w);}return _8v;});var _8x = A(_8n,[_8t]);var _8y = _8x[1];var _8z = _8x[2];var _8A = hs_eqWord64(_8r,_8y,realWorld);var _8B = _8A[2];var _8C = E(_8B);if(_8C){var _8D = hs_eqWord64(_8s,_8z,realWorld);var _8E = _8D[2];var _8F = E(_8E);var _8G = _8F?[2,_8o]:[1];var _8H = _8G;}else{var _8H = [1];}return _8H;});return E(_8p);};var _8I = function(_8J){var _8K = E(_8J);var _8L = _8K[1];var _8M = E(_8L);return _8M;};var _8N = function(_8O){var _8P = E(_8O);var _8Q = _8P[1];var _8R = _8P[2];var _8S = _8I(_8Q);var _8T = _8l(_8S,_8h,_8R);return _8T;};var _8U = function(_8V){var _8W = E(_8V);var _8X = _8W[1];var _8Y = E(_8X);return _8Y;};var _8Z = [1,','];var _90 = [1,']'];var _91 = [1,'['];var _92 = function(_93,_94){var _95 = E(_93);if(_95[0]==1){var _96 = unAppCStr("[]",_94);}else{var _97 = _95[1];var _98 = _95[2];var _99 = T(function(){var _9a = E(_97);var _9b = _9a[1];var _9c = T(function(){var _9d = [2,_90,_94];var _9e = function(_9f){var _9g = E(_9f);if(_9g[0]==1){var _9h = E(_9d);}else{var _9i = _9g[1];var _9j = _9g[2];var _9k = T(function(){var _9l = E(_9i);var _9m = _9l[1];var _9n = T(function(){return _9e(_9j);});var _9o = _t(_9m,_9n);return _9o;});var _9h = [2,_8Z,_9k];}return _9h;};return _9e(_98);});var _9p = _t(_9b,_9c);return _9p;});var _96 = [2,_91,_99];}return _96;};var _9q = function(_9r,_9s,_9t){var _9u = E(_9s);var _9v = _9u[1];var _9w = _t(_9v,_9t);return _9w;};var _9x = [1,_9q,_8U,_92];var _9y = T(function(){return [1,_8h,_9x,_9z,_8N];});var _9z = function(_9A){return [1,_9y,_9A];};var _9B = T(function(){return unCStr("Non-exhaustive patterns in");});var _9C = function(_9D,_9E){var _9F = T(function(){return A(_9E,[_9D]);});return die(_9F);};var _9G = [1,' '];var _9H = [1,'\n'];var _9I = [2,_9H,_2k];var _9J = function(_9K){var _9L = E(_9K);var _9M = _9L[1];var _9N = E(_9M);var _9O = _9N=='|'?false:true;return _9O;};var _9P = function(_9Q,_9R){var _9S = E(_9R);if(_9S[0]==1){var _9T = [1,_2k,_2k];}else{var _9U = _9S[1];var _9V = _9S[2];var _9W = A(_9Q,[_9U]);if(_9W){var _9X = T(function(){var _9Y = _9P(_9Q,_9V);var _9Z = _9Y[1];var _a0 = _9Y[2];var _a1 = [1,_9Z,_a0];return _a1;});var _a2 = T(function(){var _a3 = E(_9X);var _a4 = _a3[2];var _a5 = E(_a4);return _a5;});var _a6 = T(function(){var _a7 = E(_9X);var _a8 = _a7[1];var _a9 = E(_a8);return _a9;});var _aa = [2,_9U,_a6];var _ab = [1,_aa,_a2];}else{var _ab = [1,_2k,_9S];}var _9T = _ab;}return _9T;};var _ac = function(_ad,_ae){var _af = unCStr(_ad);var _ag = _9P(_9J,_af);var _ah = _ag[1];var _ai = _ag[2];var _aj = function(_ak,_al){var _am = T(function(){var _an = T(function(){var _ao = T(function(){return _t(_al,_9I);});return _t(_ae,_ao);});return unAppCStr(": ",_an);});return _t(_ak,_am);};var _ap = E(_ai);if(_ap[0]==1){var _aq = _aj(_ah,_2k);}else{var _ar = _ap[1];var _as = _ap[2];var _at = E(_ar);var _au = _at[1];var _av = E(_au);if(_av=='|'){var _aw = [2,_9G,_as];var _ax = _aj(_ah,_aw);}else{var _ax = _aj(_ah,_2k);}var _aq = _ax;}return _aq;};var _ay = function(_az){var _aA = T(function(){return _ac(_az,_9B);});var _aB = [1,_aA];return _9C(_aB,_9z);};var _aC = T(function(){return _ay("BreakoutImproved.hs:(125,3)-(127,30)|case");});var _aD = T(function(){return unCStr("Error!");});var _aE = T(function(){return toJSStr(_aD);});var _aF = function(_aG,_aH){var _aI = rMV(_aG,_aH);var _aJ = _aI[1];var _aK = _aI[2];var _aL = function(_aM,_aN){var _aO = E(_aM);if(_aO[0]==1){var _aP = E(_aE);var _aQ = _aP[1];var _aR = jsAlert(_aQ,_aJ);var _aS = _aR[1];var _aT = wMV(_aG,_aN,_aS);var _aU = [1,_aT,_0];var _aV = _aU;}else{var _aW = _aO[1];var _aX = E(_aW);if(_aX[0]==1){var _aY = E(_aC);}else{var _aZ = _aX[1];var _b0 = _6E(_aZ,_aJ);var _b1 = _b0[1];var _b2 = wMV(_aG,_aN,_b1);var _b3 = [1,_b2,_0];var _aY = _b3;}var _aV = _aY;}return _aV;};var _b4 = E(_aK);if(_b4[0]==1){var _b5 = _b4[1];var _b6 = A(_b5,[_1E,_1D]);var _b7 = _b6[1];var _b8 = _b6[2];var _b9 = _aL(_b7,_b8);var _ba = _b9;}else{var _bb = _b4[1];var _bc = A(_bb,[_1E,_1D]);var _bd = _bc[1];var _be = _bc[2];var _bf = _aL(_bd,_be);var _ba = _bf;}return _ba;};var _bg = function(_bh,_bi,_bj){var _bk = rMV(_bh,_bj);var _bl = _bk[1];var _bm = _bk[2];var _bn = T(function(){var _bo = E(_bm);if(_bo[0]==1){var _bp = _bo[1];var _bq = [1,_bi];var _br = A(_bp,[_5k,_bq]);var _bs = _br[2];var _bt = E(_bs);var _bu = _bt;}else{var _bv = _bo[1];var _bw = [1,_bi];var _bx = A(_bv,[_5k,_bw]);var _by = _bx[2];var _bz = E(_by);var _bu = _bz;}return _bu;});var _bA = wMV(_bh,_bn,_bl);var _bB = [1,_bA,_0];return _bB;};var _bC = function(_bD,_bE,_bF){var _bG = rMV(_bD,_bF);var _bH = _bG[1];var _bI = _bG[2];var _bJ = T(function(){var _bK = E(_bI);if(_bK[0]==1){var _bL = _bK[1];var _bM = [2,_bE];var _bN = A(_bL,[_5k,_bM]);var _bO = _bN[2];var _bP = E(_bO);var _bQ = _bP;}else{var _bR = _bK[1];var _bS = [2,_bE];var _bT = A(_bR,[_5k,_bS]);var _bU = _bT[2];var _bV = E(_bU);var _bQ = _bV;}return _bQ;});var _bW = wMV(_bD,_bJ,_bH);var _bX = [1,_bW,_0];return _bX;};var _bY = function(_bZ){var _c0 = E(_bZ);var _c1 = _c0[1];var _c2 = E(_c1);return _c2;};var _c3 = function(_c4){var _c5 = E(_c4);var _c6 = _c5[3];var _c7 = E(_c6);return _c7;};var _c8 = function(_c9,_ca,_cb){var _cc = function(_cd,_ce){var _cf = function(_cg,_ch){var _ci = E(_cg);if(_ci[0]==1){var _cj = _ci[1];var _ck = T(function(){return A(_ca,[_cj]);});var _cl = _cm(_ck);}else{var _cn = _ci[1];var _co = T(function(){return _c8(_c9,_ca,_ch);});var _cp = [2,_cn];var _cq = [1,_cp,_co];var _cl = A(_c3,[_c9,_cq]);}return _cl;};var _cr = function(_cs){var _ct = E(_cs);var _cu = _ct[1];var _cv = _ct[2];var _cw = _cf(_cu,_cv);return _cw;};var _cm = function(_cx){var _cy = T(function(){var _cz = E(_cx);if(_cz[0]==1){var _cA = _cz[1];var _cB = A(_cA,[_cd,_ce]);}else{var _cC = _cz[1];var _cD = T(function(){return A(_cC,[_cd,_ce]);});var _cB = A(_c3,[_c9,_cD]);}return _cB;});return A(_bY,[_c9,_cy,_cr]);};return _cm(_cb);};return [1,_cc];};var _cE = function(_cF,_cG){return E(_cG);};var _cH = function(_cI,_cJ){return _cE(_cI,_cJ);};var _cK = function(_cL,_cM){return A(_cM,[_cL]);};var _cN = function(_cO){return err(_cO);};var _cP = function(_cQ){return E(_cQ);};var _cR = [1,_cK,_cH,_cP,_cN];var _cS = function(_cT,_cU,_cV){var _cW = function(_cX,_cY){var _cZ = T(function(){var _d0 = E(_cV);if(_d0[0]==1){var _d1 = _d0[1];var _d2 = A(_d1,[_cX,_cY]);}else{var _d3 = _d0[1];var _d4 = T(function(){return A(_d3,[_cX,_cY]);});var _d2 = A(_c3,[_cT,_d4]);}return _d2;});var _d5 = function(_d6){var _d7 = E(_d6);var _d8 = _d7[1];var _d9 = _d7[2];var _da = E(_d8);if(_da[0]==1){var _db = E(_cZ);}else{var _dc = T(function(){return _cS(_cT,_d9,_cV);});var _dd = [1,_da,_dc];var _db = A(_c3,[_cT,_dd]);}return _db;};var _de = T(function(){var _df = E(_cU);if(_df[0]==1){var _dg = _df[1];var _dh = A(_dg,[_cX,_cY]);}else{var _di = _df[1];var _dj = T(function(){return A(_di,[_cX,_cY]);});var _dh = A(_c3,[_cT,_dj]);}return _dh;});return A(_bY,[_cT,_de,_d5]);};return [1,_cW];};var _dk = function(_dl,_dm,_dn){var _do = function(_dp,_dq,_dr){var _ds = function(_dt,_du){var _dv = T(function(){var _dw = E(_dt);var _dx = _dw[1];var _dy = _dp+_dx;var _dz = [1,_dy];return _dz;});var _dA = function(_dB){var _dC = E(_dB);var _dD = _dC[1];var _dE = _dC[2];var _dF = E(_dD);if(_dF[0]==1){var _dG = _dF[1];var _dH = T(function(){var _dI = E(_dt);var _dJ = _dI[1];var _dK = _dp+_dJ;var _dL = _do(_dK,_dq,_dE);return _dL;});var _dM = [1,_dG];var _dN = [1,_dM,_dH];var _dO = A(_c3,[_dl,_dN]);}else{var _dP = _dF[1];var _dQ = function(_dR){var _dS = E(_dR);var _dT = _dS[1];var _dU = _dS[2];var _dV = T(function(){return _do(0,_dU,_dE);});var _dW = [1,_dT,_dV];var _dX = A(_c3,[_dl,_dW]);return _dX;};var _dY = T(function(){var _dZ = E(_dq);if(_dZ[0]==1){var _e0 = _dZ[1];var _e1 = A(_e0,[_dv,_dP]);}else{var _e2 = _dZ[1];var _e3 = T(function(){return A(_e2,[_dv,_dP]);});var _e1 = A(_c3,[_dl,_e3]);}return _e1;});var _dO = A(_bY,[_dl,_dY,_dQ]);}return _dO;};var _e4 = T(function(){var _e5 = E(_dr);if(_e5[0]==1){var _e6 = _e5[1];var _e7 = A(_e6,[_dt,_du]);}else{var _e8 = _e5[1];var _e9 = T(function(){return A(_e8,[_dt,_du]);});var _e7 = A(_c3,[_dl,_e9]);}return _e7;});return A(_bY,[_dl,_e4,_dA]);};var _ea = E(_dq);if(_ea[0]==1){var _eb = [1,_ds];}else{var _ec = _ea[1];var _ed = E(_dr);if(_ed[0]==1){var _ee = [1,_ds];}else{var _ef = _ed[1];var _eg = function(_eh,_ei){var _ej = A(_ef,[_eh,_ei]);var _ek = _ej[1];var _el = _ej[2];var _em = E(_ek);if(_em[0]==1){var _en = _em[1];var _eo = T(function(){var _ep = E(_eh);var _eq = _ep[1];var _er = _dp+_eq;var _es = _do(_er,_ea,_el);return _es;});var _et = [1,_en];var _eu = [1,_et,_eo];}else{var _ev = _em[1];var _ew = T(function(){var _ex = T(function(){var _ey = E(_eh);var _ez = _ey[1];var _eA = _dp+_ez;var _eB = [1,_eA];return _eB;});return A(_ec,[_ex,_ev]);});var _eC = T(function(){var _eD = E(_ew);var _eE = _eD[2];var _eF = _do(0,_eE,_el);return _eF;});var _eG = T(function(){var _eH = E(_ew);var _eI = _eH[1];var _eJ = E(_eI);return _eJ;});var _eu = [1,_eG,_eC];}return _eu;};var _ee = [2,_eg];}var _eb = _ee;}return _eb;};return _do(0,_dm,_dn);};var _eK = function(_eL,_eM,_eN){var _eO = function(_eP,_eQ,_eR){var _eS = function(_eT,_eU){var _eV = T(function(){var _eW = E(_eR);if(_eW[0]==1){var _eX = _eW[1];var _eY = T(function(){var _eZ = E(_eT);var _f0 = _eZ[1];var _f1 = _eP+_f0;var _f2 = [1,_f1];return _f2;});var _f3 = A(_eX,[_eY,_eU]);}else{var _f4 = _eW[1];var _f5 = T(function(){var _f6 = T(function(){var _f7 = E(_eT);var _f8 = _f7[1];var _f9 = _eP+_f8;var _fa = [1,_f9];return _fa;});return A(_f4,[_f6,_eU]);});var _f3 = A(_c3,[_eL,_f5]);}return _f3;});var _fb = function(_fc){var _fd = E(_fc);var _fe = _fd[1];var _ff = _fd[2];var _fg = E(_fe);if(_fg[0]==1){var _fh = _fg[1];var _fi = T(function(){var _fj = E(_eT);var _fk = _fj[1];var _fl = _eP+_fk;var _fm = _eO(_fl,_ff,_eR);return _fm;});var _fn = [1,_fh];var _fo = [1,_fn,_fi];var _fp = A(_c3,[_eL,_fo]);}else{var _fq = _fg[1];var _fr = function(_fs){var _ft = E(_fs);var _fu = _ft[1];var _fv = _ft[2];var _fw = T(function(){return _eO(0,_ff,_fv);});var _fx = T(function(){var _fy = E(_fu);if(_fy[0]==1){var _fz = _fy[1];var _fA = [1,_fz];}else{var _fB = _fy[1];var _fC = T(function(){return A(_fq,[_fB]);});var _fA = [2,_fC];}return _fA;});var _fD = [1,_fx,_fw];var _fE = A(_c3,[_eL,_fD]);return _fE;};var _fp = A(_bY,[_eL,_eV,_fr]);}return _fp;};var _fF = T(function(){var _fG = E(_eQ);if(_fG[0]==1){var _fH = _fG[1];var _fI = A(_fH,[_eT,_eU]);}else{var _fJ = _fG[1];var _fK = T(function(){return A(_fJ,[_eT,_eU]);});var _fI = A(_c3,[_eL,_fK]);}return _fI;});return A(_bY,[_eL,_fF,_fb]);};var _fL = E(_eQ);if(_fL[0]==1){var _fM = [1,_eS];}else{var _fN = _fL[1];var _fO = E(_eR);if(_fO[0]==1){var _fP = [1,_eS];}else{var _fQ = _fO[1];var _fR = function(_fS,_fT){var _fU = A(_fN,[_fS,_fT]);var _fV = _fU[1];var _fW = _fU[2];var _fX = E(_fV);if(_fX[0]==1){var _fY = _fX[1];var _fZ = T(function(){var _g0 = E(_fS);var _g1 = _g0[1];var _g2 = _eP+_g1;var _g3 = _eO(_g2,_fW,_fO);return _g3;});var _g4 = [1,_fY];var _g5 = [1,_g4,_fZ];}else{var _g6 = _fX[1];var _g7 = T(function(){var _g8 = T(function(){var _g9 = E(_fS);var _ga = _g9[1];var _gb = _eP+_ga;var _gc = [1,_gb];return _gc;});return A(_fQ,[_g8,_fT]);});var _gd = T(function(){var _ge = T(function(){var _gf = E(_g7);var _gg = _gf[2];var _gh = E(_gg);return _gh;});return _eO(0,_fW,_ge);});var _gi = T(function(){var _gj = E(_g7);var _gk = _gj[1];var _gl = E(_gk);if(_gl[0]==1){var _gm = _gl[1];var _gn = [1,_gm];}else{var _go = _gl[1];var _gp = T(function(){return A(_g6,[_go]);});var _gn = [2,_gp];}return _gn;});var _g5 = [1,_gi,_gd];}return _g5;};var _fP = [2,_fR];}var _fM = _fP;}return _fM;};return _eO(0,_eM,_eN);};var _gq = function(_gr){var _gs = E(_gr);if(_gs[0]==1){var _gt = _gs[1];var _gu = [1,_gt];}else{var _gv = _gs[1];var _gw = function(_gx){return [1,_gv,_gx];};var _gu = [2,_gw];}return _gu;};var _gy = function(_gz){var _gA = E(_gz);var _gB = _gA[2];var _gC = E(_gB);return _gC;};var _gD = function(_gE,_gF){var _gG = T(function(){return _gy(_gF);});var _gH = [2,_gG];return [1,_gH,_gI];};var _gI = T(function(){return [2,_gD];});var _gJ = function(_gK,_gL,_gM){var _gN = E(_gM);if(_gN[0]==1){var _gO = _gN[1];var _gP = function(_gQ){var _gR = T(function(){var _gS = E(_gQ);var _gT = _gS[2];var _gU = _gJ(_gK,_gL,_gT);return _gU;});var _gV = T(function(){var _gW = E(_gQ);var _gX = _gW[1];var _gY = E(_gX);return _gY;});var _gZ = [1,_gV,_gR];return A(_c3,[_gK,_gZ]);};var _h0 = function(_h1){var _h2 = T(function(){return A(_gO,[_h1]);});var _h3 = function(_h4){var _h5 = T(function(){var _h6 = T(function(){return A(_gL,[_h4]);});return A(_h2,[_h6]);});return A(_bY,[_gK,_h5,_gP]);};return E(_h3);};var _h7 = [1,_h0];}else{var _h8 = _gN[1];var _h9 = function(_ha){var _hb = T(function(){return A(_h8,[_ha]);});var _hc = function(_hd){var _he = T(function(){var _hf = T(function(){return A(_gL,[_hd]);});return A(_hb,[_hf]);});var _hg = T(function(){var _hh = E(_he);var _hi = _hh[2];var _hj = _gJ(_gK,_gL,_hi);return _hj;});var _hk = T(function(){var _hl = E(_he);var _hm = _hl[1];var _hn = E(_hm);return _hn;});return [1,_hk,_hg];};return E(_hc);};var _h7 = [2,_h9];}return _h7;};var _ho = function(_hp,_hq,_hr){var _hs = E(_hr);if(_hs[0]==1){var _ht = _hs[1];var _hu = function(_hv){var _hw = T(function(){var _hx = E(_hv);var _hy = _hx[2];var _hz = _ho(_hp,_hq,_hy);return _hz;});var _hA = T(function(){var _hB = T(function(){var _hC = E(_hv);var _hD = _hC[1];var _hE = E(_hD);return _hE;});return A(_hq,[_hB]);});var _hF = [1,_hA,_hw];return A(_c3,[_hp,_hF]);};var _hG = function(_hH){var _hI = T(function(){return A(_ht,[_hH]);});var _hJ = function(_hK){var _hL = T(function(){return A(_hI,[_hK]);});return A(_bY,[_hp,_hL,_hu]);};return E(_hJ);};var _hM = [1,_hG];}else{var _hN = _hs[1];var _hO = function(_hP){var _hQ = T(function(){return A(_hN,[_hP]);});var _hR = function(_hS){var _hT = T(function(){return A(_hQ,[_hS]);});var _hU = T(function(){var _hV = E(_hT);var _hW = _hV[2];var _hX = _ho(_hp,_hq,_hW);return _hX;});var _hY = T(function(){var _hZ = T(function(){var _i0 = E(_hT);var _i1 = _i0[1];var _i2 = E(_i1);return _i2;});return A(_hq,[_hZ]);});return [1,_hY,_hU];};return E(_hR);};var _hM = [2,_hO];}return _hM;};var _i3 = function(_i4){var _i5 = E(_i4);var _i6 = _i5[1];var _i7 = E(_i6);return _i7;};var _i8 = function(_i9,_ia){var _ib = _gJ(_i9,_i3,_ia);var _ic = _ho(_i9,_gq,_ib);var _id = _eK(_i9,_ic,_gI);return _id;};var _ie = function(_if,_ig){var _ih = [2,_ig];var _ii = function(_ij,_ik){var _il = E(_ig);var _im = T(function(){var _in = T(function(){return A(_if,[_ij,_il,_ik]);});return _ie(_if,_in);});var _io = [1,_ih,_im];return _io;};return [2,_ii];};var _ip = function(_iq,_ir,_is){return A(_is,[_ir]);};var _it = T(function(){return _ie(_ip,_2k);});var _iu = function(_iv,_iw){var _ix = [2,_iw];return [1,_ix,_iy];};var _iy = T(function(){return [2,_iu];});var _iz = T(function(){return _dk(_cR,_it,_iy);});var _iA = T(function(){return _i8(_cR,_iz);});var _iB = function(_iC,_iD){var _iE = T(function(){var _iF = E(_iD);var _iG = _iF[1];var _iH = _iF[2];var _iI = E(_iH);var _iJ = _iI[1];var _iK = _iI[2];var _iL = [1,_iJ,_iK,_iG];return _iL;});var _iM = [2,_iE];return [1,_iM,_iN];};var _iN = T(function(){return [2,_iB];});var _iO = T(function(){return _dk(_cR,_iN,_iA);});var _iP = function(_iQ,_iR){var _iS = T(function(){var _iT = E(_iR);var _iU = _iT[1];var _iV = _iT[2];var _iW = _iT[3];var _iX = [1,_iU,_iV];var _iY = [1,_iW,_iX];return _iY;});var _iZ = [2,_iS];return [1,_iZ,_j0];};var _j0 = T(function(){return [2,_iP];});var _j1 = T(function(){return _dk(_cR,_iO,_j0);});var _j2 = function(_j3,_j4,_j5){var _j6 = function(_j7,_j8,_j9,_ja){var _jb = function(_jc){var _jd = T(function(){var _je = E(_jc);var _jf = _je[1];var _jg = _j7+_jf;var _jh = [1,_jg];return _jh;});var _ji = function(_jj,_jk){var _jl = T(function(){var _jm = E(_jc);var _jn = _jm[1];var _jo = _j8+_jn;var _jp = _j6(0,_jo,_jk,_ja);return _jp;});var _jq = [1,_jj,_jl];return A(_c3,[_j3,_jq]);};var _jr = function(_js){var _jt = E(_js);var _ju = _jt[1];var _jv = _jt[2];var _jw = _ji(_ju,_jv);return _jw;};var _jx = T(function(){var _jy = E(_jc);var _jz = _jy[1];var _jA = _j8+_jz;var _jB = [1,_jA];return _jB;});var _jC = function(_jD,_jE){var _jF = T(function(){var _jG = E(_jc);var _jH = _jG[1];var _jI = _j7+_jH;var _jJ = _j6(_jI,0,_j9,_jE);return _jJ;});var _jK = [1,_jD,_jF];return A(_c3,[_j3,_jK]);};var _jL = function(_jM){var _jN = E(_jM);var _jO = _jN[1];var _jP = _jN[2];var _jQ = _jC(_jO,_jP);return _jQ;};var _jR = function(_jS){var _jT = E(_jS);if(_jT[0]==1){var _jU = _jT[1];var _jV = T(function(){var _jW = E(_j9);if(_jW[0]==1){var _jX = _jW[1];var _jY = A(_jX,[_jd,_jU]);}else{var _jZ = _jW[1];var _k0 = T(function(){return A(_jZ,[_jd,_jU]);});var _jY = A(_c3,[_j3,_k0]);}return _jY;});var _k1 = A(_bY,[_j3,_jV,_jr]);}else{var _k2 = _jT[1];var _k3 = T(function(){var _k4 = E(_ja);if(_k4[0]==1){var _k5 = _k4[1];var _k6 = A(_k5,[_jx,_k2]);}else{var _k7 = _k4[1];var _k8 = T(function(){return A(_k7,[_jx,_k2]);});var _k6 = A(_c3,[_j3,_k8]);}return _k6;});var _k1 = A(_bY,[_j3,_k3,_jL]);}return _k1;};return E(_jR);};return [1,_jb];};return _j6(0,0,_j4,_j5);};var _k9 = function(_ka,_kb){var _kc = [2,_kb];return [1,_kc,_kd];};var _kd = T(function(){return [2,_k9];});var _ke = function(_kf,_kg){var _kh = T(function(){var _ki = E(_kg);var _kj = [1];return _kj;});var _kk = [2,_kh];return [1,_kk,_kl];};var _kl = T(function(){return [2,_ke];});var _km = T(function(){return _dk(_cR,_kd,_kl);});var _kn = function(_ko){var _kp = E(_ko);var _kq = _kp[1];var _kr = E(_kq);return _kr;};var _ks = function(_kt){var _ku = E(_kt);var _kv = _ku[2];var _kw = E(_kv);return _kw;};var _kx = T(function(){return unCStr("Feedback loop broken by inhibition");});var _ky = T(function(){return err(_kx);});var _kz = function(_kA,_kB){var _kC = T(function(){return _kn(_kA);});var _kD = function(_kE){var _kF = T(function(){var _kG = T(function(){var _kH = E(_kE);var _kI = _kH[2];var _kJ = E(_kI);return _kJ;});return _kz(_kA,_kG);});var _kK = T(function(){var _kL = E(_kE);var _kM = _kL[1];var _kN = E(_kM);if(_kN[0]==1){var _kO = _kN[1];var _kP = [1,_kO];}else{var _kQ = _kN[1];var _kR = T(function(){var _kS = E(_kQ);var _kT = _kS[1];var _kU = E(_kT);return _kU;});var _kP = [2,_kR];}return _kP;});var _kV = [1,_kK,_kF];return A(_c3,[_kC,_kV]);};var _kW = function(_kX,_kY){var _kZ = T(function(){var _l0 = T(function(){var _l1 = E(_kB);if(_l1[0]==1){var _l2 = _l1[1];var _l3 = A(_l2,[_kX]);}else{var _l4 = _l1[1];var _l5 = T(function(){return A(_l4,[_kX]);});var _l6 = function(_l7){var _l8 = T(function(){return A(_l5,[_l7]);});return A(_c3,[_kC,_l8]);};var _l3 = E(_l6);}return _l3;});var _l9 = function(_la){var _lb = T(function(){var _lc = E(_la);var _ld = _lc[1];var _le = E(_ld);if(_le[0]==1){var _lf = E(_ky);}else{var _lg = _le[1];var _lf = _gy(_lg);}return _lf;});var _lh = [1,_kY,_lb];return A(_l0,[_lh]);};return A(_ks,[_kA,_l9]);});return A(_bY,[_kC,_kZ,_kD]);};return [1,_kW];};var _li = function(_lj){var _lk = T(function(){return A(_lj,[_lk]);});return E(_lk);};var _ll = [1,_cR,_li];var _lm = function(_ln,_lo){var _lp = function(_lq,_lr){var _ls = E(_lo);var _lt = T(function(){return A(_ln,[_lq,_ls,_lr]);});var _lu = T(function(){return _lm(_ln,_lt);});var _lv = [2,_lt];var _lw = [1,_lv,_lu];return _lw;};return [2,_lp];};var _lx = function(_ly,_lz,_lA){var _lB = function(_lC,_lD){while(1){var _lE = E(_lD);if(_lE[0]==1){var _lF = E(_lC);}else{var _lG = _lE[1];var _lH = _lE[2];var _lI = A(_ly,[_lC,_lG]);_lC=_lI;_lD=_lH;continue;var _lJ = die("Unreachable!");var _lF = _lJ;}return _lF;}};return _lB(_lz,_lA);};var _lK = function(_lL,_lM){var _lN = function(_lO,_lP){return _lx(_lL,_lO,_lP);};var _lQ = function(_lR,_lS,_lT){return _lN(_lS,_lT);};return _lm(_lQ,_lM);};var _lU = function(_lV,_lW){var _lX = E(_lV);var _lY = _lX[1];var _lZ = _lX[2];var _m0 = E(_lW);var _m1 = _m0[1];var _m2 = E(_m1);var _m3 = _m2[1];var _m4 = _m2[2];var _m5 = T(function(){var _m6 = E(_m3);var _m7 = _m6[1];var _m8 = E(_lY);var _m9 = _m8[1];var _ma = E(_m4);var _mb = _ma[1];var _mc = E(_lZ);var _md = _mc[1];var _me = _mb*_md;var _mf = _m7*_m9;var _mg = _mf+_me;var _mh = 2*_mg;var _mi = [1,_mh];return _mi;});var _mj = T(function(){var _mk = E(_lZ);var _ml = _mk[1];var _mm = E(_m5);var _mn = _mm[1];var _mo = E(_m4);var _mp = _mo[1];var _mq = _mn*_mp;var _mr = _ml-_mq;var _ms = [1,_mr];return _ms;});var _mt = T(function(){var _mu = E(_lY);var _mv = _mu[1];var _mw = E(_m5);var _mx = _mw[1];var _my = E(_m3);var _mz = _my[1];var _mA = _mx*_mz;var _mB = _mv-_mA;var _mC = [1,_mB];return _mC;});var _mD = [1,_mt,_mj];return _mD;};var _mE = [1,(-3)];var _mF = [1,_67,_mE];var _mG = T(function(){return _lK(_lU,_mF);});var _mH = function(_mI,_mJ,_mK){var _mL = E(_mJ);var _mM = _mL[1];var _mN = _mL[2];var _mO = E(_mK);var _mP = _mO[1];var _mQ = _mO[2];var _mR = T(function(){var _mS = E(_mN);var _mT = _mS[1];var _mU = E(_mI);var _mV = _mU[1];var _mW = E(_mQ);var _mX = _mW[1];var _mY = _mV*_mX;var _mZ = _mT+_mY;var _n0 = [1,_mZ];return _n0;});var _n1 = T(function(){var _n2 = E(_mM);var _n3 = _n2[1];var _n4 = E(_mI);var _n5 = _n4[1];var _n6 = E(_mP);var _n7 = _n6[1];var _n8 = _n5*_n7;var _n9 = _n3+_n8;var _na = [1,_n9];return _na;});var _nb = [1,_n1,_mR];return _nb;};var _nc = [1,350];var _nd = [1,300];var _ne = [1,_nd,_nc];var _nf = T(function(){return _lm(_mH,_ne);});var _ng = function(_nh){var _ni = E(_nh);if(_ni[0]==1){var _nj = _ni[1];var _nk = [1,_nj];}else{var _nl = _ni[1];var _nm = function(_lT){return [1,_nl,_lT];};var _nk = [2,_nm];}return _nk;};var _nn = T(function(){return _ho(_cR,_ng,_nf);});var _no = T(function(){return _dk(_cR,_nn,_mG);});var _np = T(function(){return _eK(_cR,_no,_mG);});var _nq = function(_nr,_ns){while(1){var r=(function(_nt,_nu){var _nv = E(_nu);if(_nv[0]==1){var _nw = _nv[3];var _nx = _nv[4];var _ny = _nv[5];var _nz = T(function(){return _nq(_nt,_ny);});var _nA = [2,_nw,_nz];_nr=_nA;_ns=_nx;return null;var _nB = die("Unreachable!");}else{var _nB = E(_nt);}return _nB;})(_nr,_ns);if(null!==r)return r;}};var _nC = function(_nD,_nE){var _nF = T(function(){var _nG = E(_nE);var _nH = _nG[1];var _nI = _nG[2];var _nJ = _nG[3];var _nK = T(function(){var _nL = T(function(){return _nq(_2k,_nH);});return _t(_nJ,_nL);});var _nM = _t(_nI,_nK);return _nM;});var _nN = [2,_nF];return [1,_nN,_nO];};var _nO = T(function(){return [2,_nC];});var _nP = T(function(){return _dk(_cR,_np,_nO);});var _nQ = T(function(){return _i8(_cR,_nP);});var _nR = function(_nS,_nT){var _nU = T(function(){var _nV = E(_nT);var _nW = _nV[1];var _nX = _nV[2];var _nY = E(_nX);var _nZ = _nY[1];var _o0 = _nY[2];var _o1 = _nY[3];var _o2 = _nY[4];var _o3 = [1,_nZ,_o0,_o1,_o2,_nW];return _o3;});var _o4 = [2,_nU];return [1,_o4,_o5];};var _o5 = T(function(){return [2,_nR];});var _o6 = T(function(){return _dk(_cR,_o5,_nQ);});var _o7 = function(_o8,_o9){var _oa = T(function(){var _ob = E(_o9);var _oc = _ob[1];var _od = _ob[2];var _oe = _ob[3];var _of = _ob[4];var _og = _ob[5];var _oh = _ob[6];var _oi = [1,_oc,_od,_og,_oh];var _oj = [1,_od,_oe,_of];var _ok = [1,_oj,_oi];return _ok;});var _ol = [2,_oa];return [1,_ol,_om];};var _om = T(function(){return [2,_o7];});var _on = T(function(){return _dk(_cR,_o6,_om);});var _oo = function(_op){var _oq = [2,_op];var _or = function(_os,_ot){var _ou = E(_op);var _ov = T(function(){return _oo(_ot);});var _ow = [1,_oq,_ov];return _ow;};return [2,_or];};var _ox = [1,_ne,_mF];var _oy = T(function(){return _oo(_ox);});var _oz = function(_oA,_oB){var _oC = [2,_oB];return [1,_oC,_oD];};var _oD = T(function(){return [2,_oz];});var _oE = T(function(){return _dk(_cR,_oy,_oD);});var _oF = T(function(){return _i8(_cR,_oE);});var _oG = function(_oH,_oI){var _oJ = T(function(){var _oK = E(_oI);var _oL = _oK[1];var _oM = _oK[2];var _oN = E(_oM);var _oO = _oN[1];var _oP = _oN[2];var _oQ = _oN[3];var _oR = _oN[4];var _oS = _oN[5];var _oT = [1,_oO,_oP,_oQ,_oR,_oS,_oL];return _oT;});var _oU = [2,_oJ];return [1,_oU,_oV];};var _oV = T(function(){return [2,_oG];});var _oW = T(function(){return _dk(_cR,_oV,_oF);});var _oX = function(_oY,_oZ){var _p0 = T(function(){var _p1 = E(_oZ);var _p2 = _p1[5];var _p3 = [1,_p2,_p1];return _p3;});var _p4 = [2,_p0];return [1,_p4,_p5];};var _p5 = T(function(){return [2,_oX];});var _p6 = T(function(){return _dk(_cR,_oW,_p5);});var _p7 = function(_p8){var _p9 = [1,_p8];var _pa = T(function(){return [2,_pb];});var _pb = function(_pc,_pd){return E(_pe);};var _pe = T(function(){return [1,_p9,_pa];});return E(_pa);};var _pf = [2];var _pg = T(function(){return _p7(_pf);});var _ph = [3];var _pi = [1,_ph];var _pj = function(_pk,_pl){var _pm = T(function(){var _pn = E(_pl);var _po = _pn[1];var _pp = E(_po);var _pq = _pp[2];var _pr = E(_pq);var _ps = _pr[1];var _pt = _ps>400;var _pu = _pt?E(_pi):[2,_pn];return _pu;});return [1,_pm,_pv];};var _pv = T(function(){return [2,_pj];});var _pw = T(function(){return _cS(_cR,_pv,_pg);});var _px = function(_py,_pz){var _pA = [2,_pz];return [1,_pA,_pB];};var _pB = T(function(){return [2,_px];});var _pC = T(function(){return _dk(_cR,_pw,_pB);});var _pD = T(function(){return _i8(_cR,_pC);});var _pE = function(_pF,_pG){var _pH = T(function(){var _pI = E(_pG);var _pJ = _pI[2];var _pK = E(_pJ);return _pK;});var _pL = [2,_pH];return [1,_pL,_pM];};var _pM = T(function(){return [2,_pE];});var _pN = T(function(){return _dk(_cR,_pM,_pD);});var _pO = function(_pP,_pQ){var _pR = T(function(){var _pS = E(_pQ);var _pT = _pS[6];var _pU = [1,_pT,_pS];return _pU;});var _pV = [2,_pR];return [1,_pV,_pW];};var _pW = T(function(){return [2,_pO];});var _pX = T(function(){return _dk(_cR,_pN,_pW);});var _pY = T(function(){return unCStr("Prelude.undefined");});var _pZ = T(function(){return err(_pY);});var _q0 = function(_q1,_q2){var _q3 = E(_q2);if(_q3[0]==1){var _q4 = [1];}else{var _q5 = _q3[1];var _q6 = _q3[2];var _q7 = T(function(){return _q0(_q1,_q6);});var _q8 = T(function(){return A(_q1,[_q5]);});var _q4 = [2,_q8,_q7];}return _q4;};var _q9 = function(_qa){var _qb = E(_qa);var _qc = _qb[1];var _qd = E(_qc);return _qd;};var _qe = function(_qf){var _qg = T(function(){return _q9(_qf);});var _qh = [1,_qg];var _qi = T(function(){return [2,_qj];});var _qj = function(_qk,_ql){return E(_qm);};var _qm = T(function(){return [1,_qh,_qi];});return E(_qi);};var _qn = function(_qo,_qp){var _qq = T(function(){return _qe(_qo);});var _qr = T(function(){return _q9(_qo);});var _qs = [1,_qr];var _qt = [1,_qs,_qq];var _qu = T(function(){return _qn(_qo,_qp);});var _qv = function(_qw,_qx){var _qy = A(_qp,[_qx]);if(_qy){var _qz = [2,_qx];var _qA = [1,_qz,_qu];}else{var _qA = E(_qt);}return _qA;};return [2,_qv];};var _qB = function(_qC){var _qD = E(_qC);return _qD[0]==1?true:false;};var _qE = function(_qF,_qG){var _qH = E(_qG);if(_qH[0]==3){var _qI = E(_qF);}else{var _qJ = E(_qF);if(_qJ[0]==3){var _qK = E(_qH);}else{var _qL = E(_qH);var _qK = _qL[0]==1?[1]:E(_qJ);}var _qI = _qK;}return _qI;};var _qM = function(_qN){var _qO = E(_qN);if(_qO[0]==1){var _qP = [3];}else{var _qQ = _qO[1];var _qR = _qO[2];var _qS = _qM(_qR);if(_qS[0]==3){var _qT = E(_qQ);}else{var _qU = E(_qQ);if(_qU[0]==3){var _qV = E(_qS);}else{var _qW = E(_qS);var _qV = _qW[0]==1?[1]:E(_qU);}var _qT = _qV;}var _qP = _qT;}return _qP;};var _qX = [1,_ph,_qE,_qM];var _qY = T(function(){return _qn(_qX,_qB);});var _qZ = function(_r0){var _r1 = [2,_r0];var _r2 = T(function(){return [2,_r3];});var _r3 = function(_r4,_r5){return E(_r6);};var _r6 = T(function(){return [1,_r1,_r2];});return E(_r2);};var _r7 = [1,0];var _r8 = T(function(){return _qZ(_r7);});var _r9 = T(function(){return _dk(_cR,_r8,_qY);});var _ra = T(function(){return _qZ(_r7);});var _rb = [1,1];var _rc = T(function(){return _qZ(_rb);});var _rd = [1,_ph];var _re = T(function(){return [1,_rd,_rf];});var _rg = function(_rh,_ri){return E(_re);};var _rf = T(function(){return [2,_rg];});var _rj = function(_rk,_rl){var _rm = [2,_rl];return [1,_rm,_rf];};var _rn = [2,_rj];var _ro = T(function(){return _dk(_cR,_rn,_rc);});var _rp = T(function(){return _cS(_cR,_ro,_ra);});var _rq = T(function(){return _cS(_cR,_r9,_rp);});var _rr = T(function(){return _qZ(_r7);});var _rs = function(_rt){var _ru = E(_rt);var _rv = _ru[1];var _rw = E(_rv);var _rx = _rw[0]==1?E(_rr):E(_rq);return _rx;};var _ry = function(_rz,_rA){var _rB = _rA<=0;if(_rB){var _rC = _qe(_rz);}else{var _rD = function(_rE,_rF){var _rG = T(function(){var _rH = E(_rE);var _rI = _rH[1];var _rJ = _rA-_rI;var _rK = _ry(_rz,_rJ);return _rK;});var _rL = [2,_rF];return [1,_rL,_rG];};var _rC = [2,_rD];}return _rC;};var _rM = T(function(){return _ry(_qX,30);});var _rN = function(_rO){var _rP = E(_rO);if(_rP[0]==1){var _rQ = _rP[1];var _rR = [1,_rQ];}else{var _rS = _rP[1];var _rT = [2,_rS];var _rR = [2,_rT];}return _rR;};var _rU = T(function(){return _qZ(_1E);});var _rV = function(_rW,_rX){var _rY = E(_rW);var _rZ = _rY[1];var _s0 = E(_rX);var _s1 = _s0[1];var _s2 = _rZ-_s1;var _s3 = [1,_s2];return _s3;};var _s4 = function(_s5){var _s6 = E(_s5);if(_s6[0]==1){var _s7 = _s6[1];var _s8 = [1,_s7];}else{var _s9 = _s6[1];var _sa = function(_lT){return _rV(_s9,_lT);};var _s8 = [2,_sa];}return _s8;};var _sb = T(function(){return _ho(_cR,_s4,_rU);});var _sc = [1,30];var _sd = T(function(){return _qZ(_sc);});var _se = [1,0];var _sf = function(_sg){var _sh = function(_si,_sj){var _sk = E(_sg);var _sl = _sk[1];var _sm = E(_si);var _sn = _sm[1];var _so = _sl+_sn;var _sp = [1,_so];var _sq = T(function(){return _sf(_sp);});var _sr = [2,_sp];var _ss = [1,_sr,_sq];return _ss;};return [2,_sh];};var _st = T(function(){return _sf(_se);});var _su = function(_sv,_sw){var _sx = E(_sv);var _sy = _sx[1];var _sz = E(_sw);var _sA = _sz[1];var _sB = _sy/_sA;var _sC = [1,_sB];return _sC;};var _sD = function(_sE){var _sF = E(_sE);if(_sF[0]==1){var _sG = _sF[1];var _sH = [1,_sG];}else{var _sI = _sF[1];var _sJ = function(_lT){return _su(_sI,_lT);};var _sH = [2,_sJ];}return _sH;};var _sK = T(function(){return _ho(_cR,_sD,_st);});var _sL = T(function(){return _eK(_cR,_sK,_sd);});var _sM = T(function(){return _eK(_cR,_sb,_sL);});var _sN = T(function(){return _ho(_cR,_rN,_sM);});var _sO = T(function(){return _dk(_cR,_sN,_rM);});var _sP = T(function(){return _ay("BreakoutImproved.hs:(316,3)-(317,73)|function update");});var _sQ = function(_sR,_sS){var _sT = E(_sR);var _sU = _sT[1];var _sV = _sT[2];var _sW = _sT[3];var _sX = E(_sS);if(_sX[0]==1){var _sY = [1,_sU,_sV,_sW];}else{var _sZ = E(_sW);if(_sZ[0]==1){var _t0 = _sZ[1];var _t1 = T(function(){var _t2 = E(_t0);var _t3 = _t2[1];var _t4 = _t3-1|0;var _t5 = [1,_t4];return _t5;});var _t6 = [1,_t1];var _t7 = [1,_sU,_sV,_t6];}else{var _t7 = E(_sP);}var _sY = _t7;}return _sY;};var _t8 = function(_t9,_ta,_tb){return _sQ(_ta,_tb);};var _tc = T(function(){return _ay("BreakoutImproved.hs:318:3-42|function blockAlive");});var _td = function(_te){var _tf = E(_te);var _tg = _tf[3];var _th = E(_tg);if(_th[0]==1){var _ti = _th[1];var _tj = E(_ti);var _tk = _tj[1];var _tl = _tk>0;var _tm = _tl;}else{var _tm = E(_tc);}return _tm;};var _tn = T(function(){return _qn(_qX,_td);});var _to = function(_tp){var _tq = T(function(){var _tr = T(function(){var _ts = E(_tp);var _tt = _ts[2];var _tu = E(_tt);return _tu;});var _tv = T(function(){var _tw = E(_tp);var _tx = _tw[1];var _ty = E(_tx);return _ty;});var _tz = function(_tA){var _tB = E(_tA);if(_tB[0]==1){var _tC = _tB[1];var _tD = [1,_tC];}else{var _tE = _tB[1];var _tF = [1,_tv,_tr,_tE];var _tD = [2,_tF];}return _tD;};return _ho(_cR,_tz,_sO);});var _tG = T(function(){var _tH = T(function(){return _lm(_t8,_tp);});return _dk(_cR,_tn,_tH);});return _cS(_cR,_tG,_tq);};var _tI = function(_tJ){var _tK = T(function(){return _to(_tJ);});var _tL = _rs(_tJ);var _tM = _ho(_cR,_gq,_tL);var _tN = _eK(_cR,_tM,_tK);return _tN;};var _tO = [1,140];var _tP = [1,240];var _tQ = [1,520];var _tR = [2,_tQ,_2k];var _tS = [1,440];var _tT = [2,_tS,_tR];var _tU = [1,340];var _tV = [2,_tU,_tT];var _tW = [2,_tP,_tV];var _tX = [2,_tO,_tW];var _tY = [2,_5i,_tX];var _tZ = [2];var _u0 = [1];var _u1 = [1,2];var _u2 = [1,260];var _u3 = [1,_u2,_u0,_u1];var _u4 = [2,_u3,_2k];var _u5 = [1,180];var _u6 = [1,_u5,_u0,_u1];var _u7 = [2,_u6,_u4];var _u8 = [1,_tO,_u0,_rb];var _u9 = [2,_u8,_u7];var _ua = [1,100];var _ub = [1,_ua,_u0,_u1];var _uc = [2,_ub,_u9];var _ud = function(_ue){var _uf = E(_ue);if(_uf[0]==1){var _ug = [1];}else{var _uh = _uf[1];var _ui = _uf[2];var _uj = T(function(){return _ud(_ui);});var _uk = function(_ul,_um,_un,_uo){var _up = T(function(){return _uq(_uo);});var _ur = [1,_un];var _us = [1,_uh,_ul];var _ut = [1,_um,_us,_ur];return [2,_ut,_up];};var _uq = function(_uu){var _uv = E(_uu);if(_uv[0]==1){var _uw = E(_uj);}else{var _ux = _uv[1];var _uy = _uv[2];var _uz = E(_ux);var _uA = _uz[1];var _uB = _uz[2];var _uC = _uz[3];var _uD = T(function(){return _uq(_uy);});var _uE = [1,_uC];var _uF = [1,_uh,_uA];var _uG = [1,_uB,_uF,_uE];var _uH = [2,_uG,_uD];var _uw = _uH;}return _uw;};var _ug = _uk(_5j,_tZ,_rb,_uc);}return _ug;};var _uI = T(function(){return _ud(_tY);});var _uJ = T(function(){return _q0(_tI,_uI);});var _uK = function(_uL,_uM){var _uN = T(function(){return _i3(_uM);});var _uO = [2,_uN];return [1,_uO,_uP];};var _uP = T(function(){return [2,_uK];});var _uQ = function(_uR){var _uS = E(_uR);if(_uS[0]==1){var _uT = _uS[1];var _uU = [1,_uT];}else{var _uV = _uS[1];var _uW = function(_gx){return [1,_uV,_gx];};var _uU = [2,_uW];}return _uU;};var _uX = function(_uY,_uZ){var _v0 = T(function(){return _gJ(_uY,_gy,_uZ);});var _v1 = _ho(_uY,_uQ,_uP);var _v2 = _eK(_uY,_v1,_v0);return _v2;};var _v3 = function(_v4,_v5){while(1){var _v6 = E(_v4);if(_v6[0]==1){var _v7 = E(_v5);}else{var _v8 = _v6[2];var _v9 = _v5+1|0;_v4=_v8;_v5=_v9;continue;var _va = die("Unreachable!");var _v7 = _va;}return _v7;}};var _vb = function(_vc,_vd){while(1){var _ve = E(_vc);if(_ve){var _vf = E(_vd);if(_vf[0]==1){var _vg = [1];}else{var _vh = _vf[2];var _vi = _ve-1|0;_vc=_vi;_vd=_vh;continue;var _vj = die("Unreachable!");var _vg = _vj;}var _vk = _vg;}else{var _vk = E(_vd);}return _vk;}};var _vl = function(_vm,_vn,_vo,_vp){var _vq = E(_vo);if(_vq[0]==1){var _vr = E(_vn);}else{var _vs = _vq[1];var _vt = _vq[2];var _vu = E(_vp);if(_vu[0]==1){var _vv = E(_vn);}else{var _vw = _vu[1];var _vx = _vu[2];var _vy = T(function(){return _vl(_vm,_vn,_vt,_vx);});var _vv = A(_vm,[_vs,_vw,_vy]);}var _vr = _vv;}return _vr;};var _vz = function(_vA,_vB){while(1){var r=(function(_vC,_vD){var _vE = E(_vD);if(_vE[0]==1){var _vF = [1];}else{var _vG = _vE[1];var _vH = _vE[2];var _vI = A(_vC,[_vG]);if(_vI[0]==1){_vA=_vC;_vB=_vH;return null;var _vJ = die("Unreachable!");}else{var _vK = _vI[1];var _vL = T(function(){return _vz(_vC,_vH);});var _vJ = [2,_vK,_vL];}var _vF = _vJ;}return _vF;})(_vA,_vB);if(null!==r)return r;}};var _vM = function(_vN){var _vO = E(_vN);var _vP = _vO[1];var _vQ = _vO[2];var _vR = E(_vP);if(_vR[0]==1){var _vS = [1];}else{var _vT = _vR[1];var _vU = [1,_vT,_vQ];var _vS = [2,_vU];}return _vS;};var _vV = function(_vW,_vX,_vY){var _vZ = T(function(){return A(_c3,[_vW,_2k]);});var _w0 = function(_w1,_w2){var _w3 = E(_w2);var _w4 = _w3[1];var _w5 = _w3[2];var _w6 = T(function(){return _q0(_vX,_w5);});var _w7 = function(_w8){var _w9 = E(_w8);if(_w9[0]==1){var _wa = E(_w6);}else{var _wb = _w9[1];var _wc = _w9[2];var _wd = T(function(){return _w7(_wc);});var _we = T(function(){return _gy(_wb);});var _wa = [2,_we,_wd];}return _wa;};var _wf = function(_wg){var _wh = T(function(){return _vz(_vM,_wg);});var _wi = T(function(){var _wj = T(function(){return _w7(_wh);});return _vV(_vW,_vX,_wj);});var _wk = T(function(){return _q0(_i3,_wh);});var _wl = [2,_wk];var _wm = [1,_wl,_wi];return A(_c3,[_vW,_wm]);};var _wn = T(function(){var _wo = function(_wp){var _wq = E(_wp);if(_wq[0]==1){var _wr = E(_vZ);}else{var _ws = _wq[1];var _wt = _wq[2];var _wu = T(function(){return _wo(_wt);});var _wv = function(_ww){var _wx = function(_wy){var _wz = [2,_ww,_wy];return A(_c3,[_vW,_wz]);};return A(_bY,[_vW,_wu,_wx]);};var _wA = T(function(){var _wB = E(_ws);if(_wB[0]==1){var _wC = _wB[1];var _wD = A(_wC,[_w1,_w4]);}else{var _wE = _wB[1];var _wF = T(function(){return A(_wE,[_w1,_w4]);});var _wD = A(_c3,[_vW,_wF]);}return _wD;});var _wr = A(_bY,[_vW,_wA,_wv]);}return _wr;};return _wo(_vY);});var _wG = A(_bY,[_vW,_wn,_wf]);return _wG;};return [1,_w0];};var _wH = function(_wI,_wJ,_wK){var _wL = _wK>_wJ;if(_wL){var _wM = _wK>_wI;if(_wM){var _wN = [1];}else{var _wO = [1,_wI];var _wN = [2,_wO,_2k];}var _wP = _wN;}else{var _wQ = T(function(){var _wR = _wJ-_wI|0;var _wS = _wK-_wR|0;var _wT = function(_wU){var _wV = _wU<_wS;if(_wV){var _wW = [1,_wU];var _wX = [2,_wW,_2k];}else{var _wY = T(function(){var _wZ = _wU+_wR|0;var _x0 = _wT(_wZ);return _x0;});var _x1 = [1,_wU];var _wX = [2,_x1,_wY];}return _wX;};var _x2 = _wT(_wJ);return _x2;});var _x3 = [1,_wI];var _wP = [2,_x3,_wQ];}return _wP;};var _x4 = function(_x5,_x6,_x7){var _x8 = _x7<_x6;if(_x8){var _x9 = _x7<_x5;if(_x9){var _xa = [1];}else{var _xb = [1,_x5];var _xa = [2,_xb,_2k];}var _xc = _xa;}else{var _xd = T(function(){var _xe = _x6-_x5|0;var _xf = _x7-_xe|0;var _xg = function(_xh){var _xi = _xh>_xf;if(_xi){var _xj = [1,_xh];var _xk = [2,_xj,_2k];}else{var _xl = T(function(){var _xm = _xh+_xe|0;var _xn = _xg(_xm);return _xn;});var _xo = [1,_xh];var _xk = [2,_xo,_xl];}return _xk;};var _xp = _xg(_x6);return _xp;});var _xq = [1,_x5];var _xc = [2,_xq,_xd];}return _xc;};var _xr = function(_xs,_xt){var _xu = _xt>=_xs;return _xu?_x4(_xs,_xt,2147483647):_wH(_xs,_xt,(-2147483648));};var _xv = T(function(){return _xr(0,1);});var _xw = function(_xx,_xy){while(1){var _xz = E(_xy);if(_xz[0]==1){var _xA = _xz[2];var _xB = _xz[3];var _xC = _xz[4];var _xD = _xz[5];var _xE = E(_xA);var _xF = _xE[1];var _xG = _xx<_xF;if(_xG){_xx=_xx;_xy=_xC;continue;var _xH = die("Unreachable!");}else{var _xI = _xx==_xF;if(_xI){var _xJ = [2,_xB];}else{_xx=_xx;_xy=_xD;continue;var _xJ = die("Unreachable!");}var _xH = _xJ;}var _xK = _xH;}else{var _xK = [1];}return _xK;}};var _xL = function(_xM,_xN){var _xO = E(_xM);var _xP = _xO[1];var _xQ = _xw(_xP,_xN);return _xQ;};var _xR = function(_xS,_xT,_xU){var _xV = T(function(){var _xW = T(function(){return [2,_xX];});var _xX = function(_xY,_xZ){var _y0 = T(function(){return _xL(_xT,_xZ);});var _y1 = [2,_y0];return [1,_y1,_xW];};return _dk(_xS,_xU,_xW);});var _y2 = _qZ(_xT);var _y3 = _ho(_xS,_gq,_y2);var _y4 = _eK(_xS,_y3,_xV);return _y4;};var _y5 = function(_y6,_y7){var _y8 = [2,_y7];return [1,_y8,_y9];};var _y9 = T(function(){return [2,_y5];});var _ya = function(_yb,_yc){var _yd = [1,_yc,_yc];var _ye = [2,_yd];return [1,_ye,_yf];};var _yf = T(function(){return [2,_ya];});var _yg = function(_yh){var _yi = _v3(_yh,0);var _yj = [1,_yi];return _yj;};var _yk = function(_yl,_ym){var _yn = T(function(){return _yg(_ym);});var _yo = [2,_yn];return [1,_yo,_yp];};var _yp = T(function(){return [2,_yk];});var _yq = function(_yr,_ys){var _yt = T(function(){var _yu = E(_ys);var _yv = _yu[1];var _yw = _yu[2];var _yx = [1,_yw,_yv];return _yx;});var _yy = [2,_yt];return [1,_yy,_yz];};var _yz = T(function(){return [2,_yq];});var _yA = function(_yB,_yC){var _yD = E(_yB);if(_yD[0]==1){var _yE = [1];}else{var _yF = _yD[1];var _yG = _yD[2];var _yH = E(_yC);if(_yH[0]==1){var _yI = [1];}else{var _yJ = _yH[1];var _yK = _yH[2];var _yL = T(function(){return _yA(_yG,_yK);});var _yM = [1,_yF,_yJ];var _yI = [2,_yM,_yL];}var _yE = _yI;}return _yE;};var _yN = function(_yO){var _yP = E(_yO);var _yQ = _yP[1];var _yR = _yP[2];var _yS = _yA(_yR,_yQ);return _yS;};var _yT = function(_yU,_yV){var _yW = T(function(){return _yN(_yV);});var _yX = [2,_yW];return [1,_yX,_yY];};var _yY = T(function(){return [2,_yT];});var _yZ = function(_z0,_z1){var _z2 = [2,_z1];return [1,_z2,_z3];};var _z3 = T(function(){return [2,_yZ];});var _z4 = function(_z5,_z6){var _z7 = function(_z8,_z9){var _za = E(_z6);var _zb = T(function(){return A(_z5,[_z8,_za,_z9]);});var _zc = T(function(){var _zd = T(function(){var _ze = E(_zb);var _zf = _ze[2];var _zg = E(_zf);return _zg;});return _z4(_z5,_zd);});var _zh = T(function(){var _zi = E(_zb);var _zj = _zi[1];var _zk = E(_zj);return _zk;});var _zl = [2,_zh];var _zm = [1,_zl,_zc];return _zm;};return [2,_z7];};var _zn = function(_zo,_zp){var _zq = E(_zo);var _zr = _zq[1];var _zs = _zr<0;var _zt = _zs?E(_zp):_vb(_zr,_zp);return _zt;};var _zu = function(_zv,_zw){var _zx = E(_zv);if(_zx){var _zy = E(_zw);if(_zy[0]==1){var _zz = [1];}else{var _zA = _zy[1];var _zB = _zy[2];var _zC = T(function(){var _zD = _zx-1|0;var _zE = _zu(_zD,_zB);return _zE;});var _zz = [2,_zA,_zC];}var _zF = _zz;}else{var _zF = [1];}return _zF;};var _zG = function(_zH,_zI){var _zJ = _zH>=0;return _zJ?_zu(_zH,_zI):[1];};var _zK = function(_zL,_zM){var _zN = E(_zL);var _zO = _zN[1];var _zP = _zO<=0;var _zQ = _zP?[1]:_zG(_zO,_zM);return _zQ;};var _zR = function(_zS,_zT){var _zU = T(function(){return _zn(_zT,_zS);});var _zV = T(function(){return _zK(_zT,_zS);});return [1,_zV,_zU];};var _zW = function(_zX,_zY,_zZ){return _zR(_zY,_zZ);};var _A0 = function(_A1,_A2){return _z4(_zW,_A2);};var _A3 = function(_A4){var _A5 = T(function(){return _dk(_A4,_z3,_yY);});var _A6 = function(_A7){var _A8 = T(function(){var _A9 = T(function(){var _Aa = _A0(_A4,_A7);var _Ab = _dk(_A4,_Aa,_yp);var _Ac = _i8(_A4,_Ab);return _Ac;});var _Ad = _dk(_A4,_yz,_A9);var _Ae = _dk(_A4,_Ad,_yf);return _Ae;});var _Af = _dk(_A4,_A5,_A8);var _Ag = _dk(_A4,_Af,_y9);return _Ag;};return E(_A6);};var _Ah = function(_Ai,_Aj,_Ak){var _Al = T(function(){var _Am = T(function(){var _An = T(function(){var _Ao = _v3(_Ak,0);var _Ap = _Ao<0;var _Aq = _Ap?E(_xv):_vb(_Ao,_xv);return _Aq;});return A(_A3,[_Ai,_An]);});return _uX(_Ai,_Am);});var _Ar = T(function(){var _As = function(_At,_Au,_Av){var _Aw = T(function(){return _xR(_Ai,_At,_Au);});return [2,_Aw,_Av];};return _vl(_As,_2k,_xv,_Ak);});var _Ax = function(_Ay){var _Az = E(_Ay);var _AA = _Az[1];var _AB = _Az[2];var _AC = T(function(){return A(_Aj,[_AB]);});var _AD = _xR(_Ai,_AA,_AC);return _AD;};var _AE = _vV(_Ai,_Ax,_Ar);var _AF = _dk(_Ai,_AE,_Al);return _AF;};var _AG = T(function(){return _Ah(_cR,_pZ,_uJ);});var _AH = function(_AI,_AJ){var _AK = [1,_AJ,_2k];var _AL = [2,_AK];return [1,_AL,_AM];};var _AM = T(function(){return [2,_AH];});var _AN = T(function(){return _dk(_cR,_AG,_AM);});var _AO = function(_AP,_AQ){while(1){var _AR = E(_AP);if(_AR[0]==1){var _AS = E(_AQ);}else{var _AT = _AR[1];var _AU = _AR[2];var _AV = E(_AT);var _AW = _AV[1];var _AX = _AQ+_AW|0;_AP=_AU;_AQ=_AX;continue;var _AY = die("Unreachable!");var _AS = _AY;}return _AS;}};var _AZ = function(_B0){var _B1 = T(function(){var _B2 = E(_B0);var _B3 = _B2[2];var _B4 = E(_B3);var _B5 = _B4[2];var _B6 = E(_B5);return _B6;});var _B7 = T(function(){return _i3(_B0);});return [1,_B7,_B1];};var _B8 = function(_lT){return _AZ(_lT);};var _B9 = function(_Ba){var _Bb = E(_Ba);var _Bc = _Bb[2];var _Bd = E(_Bc);var _Be = _Bd[1];var _Bf = E(_Be);return _Bf;};var _Bg = function(_lT){return _B9(_lT);};var _Bh = function(_Bi){var _Bj = T(function(){return _q0(_B8,_Bi);});var _Bk = T(function(){var _Bl = _q0(_Bg,_Bi);var _Bm = _AO(_Bl,0);var _Bn = [1,_Bm];return _Bn;});return [1,_Bk,_Bj];};var _Bo = function(_Bp,_Bq){var _Br = T(function(){return _Bh(_Bq);});var _Bs = [2,_Br];return [1,_Bs,_Bt];};var _Bt = T(function(){return [2,_Bo];});var _Bu = T(function(){return _dk(_cR,_Bt,_AN);});var _Bv = [1];var _Bw = [2];var _Bx = T(function(){return unCStr("Failure in Data.Map.balanceL");});var _By = T(function(){return err(_Bx);});var _Bz = function(_BA,_BB,_BC,_BD){var _BE = E(_BD);if(_BE[0]==1){var _BF = _BE[1];var _BG = E(_BC);if(_BG[0]==1){var _BH = _BG[1];var _BI = _BG[2];var _BJ = _BG[3];var _BK = _BG[4];var _BL = _BG[5];var _BM = imul(3,_BF)|0;var _BN = _BH>_BM;if(_BN){var _BO = E(_BK);if(_BO[0]==1){var _BP = _BO[1];var _BQ = E(_BL);if(_BQ[0]==1){var _BR = _BQ[1];var _BS = _BQ[2];var _BT = _BQ[3];var _BU = _BQ[4];var _BV = _BQ[5];var _BW = imul(2,_BP)|0;var _BX = _BR<_BW;if(_BX){var _BY = E(_BA);var _BZ = 1+_BF|0;var _C0 = _BZ+_BR|0;var _C1 = [1,_C0,E(_BY),_BB,E(_BQ),E(_BE)];var _C2 = 1+_BH|0;var _C3 = _C2+_BF|0;var _C4 = [1,_C3,E(_BI),_BJ,E(_BO),E(_C1)];var _C5 = _C4;}else{var _C6 = function(_C7){var _C8 = E(_BV);if(_C8[0]==1){var _C9 = _C8[1];var _Ca = E(_BA);var _Cb = 1+_BF|0;var _Cc = _Cb+_C9|0;var _Cd = [1,_Cc,E(_Ca),_BB,E(_C8),E(_BE)];var _Ce = 1+_BP|0;var _Cf = _Ce+_C7|0;var _Cg = [1,_Cf,E(_BI),_BJ,E(_BO),E(_BU)];var _Ch = 1+_BH|0;var _Ci = _Ch+_BF|0;var _Cj = [1,_Ci,E(_BS),_BT,E(_Cg),E(_Cd)];var _Ck = _Cj;}else{var _Cl = E(_BA);var _Cm = 1+_BF|0;var _Cn = [1,_Cm,E(_Cl),_BB,E(_Bw),E(_BE)];var _Co = 1+_BP|0;var _Cp = _Co+_C7|0;var _Cq = [1,_Cp,E(_BI),_BJ,E(_BO),E(_BU)];var _Cr = 1+_BH|0;var _Cs = _Cr+_BF|0;var _Ct = [1,_Cs,E(_BS),_BT,E(_Cq),E(_Cn)];var _Ck = _Ct;}return _Ck;};var _Cu = E(_BU);if(_Cu[0]==1){var _Cv = _Cu[1];var _Cw = _C6(_Cv);}else{var _Cw = _C6(0);}var _C5 = _Cw;}var _Cx = _C5;}else{var _Cx = E(_By);}var _Cy = _Cx;}else{var _Cy = E(_By);}var _Cz = _Cy;}else{var _CA = E(_BA);var _CB = 1+_BH|0;var _CC = _CB+_BF|0;var _CD = [1,_CC,E(_CA),_BB,E(_BG),E(_BE)];var _Cz = _CD;}var _CE = _Cz;}else{var _CF = E(_BA);var _CG = 1+_BF|0;var _CH = [1,_CG,E(_CF),_BB,E(_Bw),E(_BE)];var _CE = _CH;}var _CI = _CE;}else{var _CJ = E(_BC);if(_CJ[0]==1){var _CK = _CJ[1];var _CL = _CJ[2];var _CM = _CJ[3];var _CN = _CJ[4];var _CO = _CJ[5];var _CP = E(_CN);if(_CP[0]==1){var _CQ = _CP[1];var _CR = E(_CO);if(_CR[0]==1){var _CS = _CR[1];var _CT = _CR[2];var _CU = _CR[3];var _CV = _CR[4];var _CW = _CR[5];var _CX = imul(2,_CQ)|0;var _CY = _CS<_CX;if(_CY){var _CZ = E(_BA);var _D0 = 1+_CS|0;var _D1 = [1,_D0,E(_CZ),_BB,E(_CR),E(_Bw)];var _D2 = 1+_CK|0;var _D3 = [1,_D2,E(_CL),_CM,E(_CP),E(_D1)];var _D4 = _D3;}else{var _D5 = function(_D6){var _D7 = E(_CW);if(_D7[0]==1){var _D8 = _D7[1];var _D9 = E(_BA);var _Da = 1+_D8|0;var _Db = [1,_Da,E(_D9),_BB,E(_D7),E(_Bw)];var _Dc = 1+_CQ|0;var _Dd = _Dc+_D6|0;var _De = [1,_Dd,E(_CL),_CM,E(_CP),E(_CV)];var _Df = 1+_CK|0;var _Dg = [1,_Df,E(_CT),_CU,E(_De),E(_Db)];var _Dh = _Dg;}else{var _Di = E(_BA);var _Dj = [1,1,E(_Di),_BB,E(_Bw),E(_Bw)];var _Dk = 1+_CQ|0;var _Dl = _Dk+_D6|0;var _Dm = [1,_Dl,E(_CL),_CM,E(_CP),E(_CV)];var _Dn = 1+_CK|0;var _Do = [1,_Dn,E(_CT),_CU,E(_Dm),E(_Dj)];var _Dh = _Do;}return _Dh;};var _Dp = E(_CV);if(_Dp[0]==1){var _Dq = _Dp[1];var _Dr = _D5(_Dq);}else{var _Dr = _D5(0);}var _D4 = _Dr;}var _Ds = _D4;}else{var _Dt = E(_BA);var _Du = [1,1,E(_Dt),_BB,E(_Bw),E(_Bw)];var _Dv = [1,3,E(_CL),_CM,E(_CP),E(_Du)];var _Ds = _Dv;}var _Dw = _Ds;}else{var _Dx = E(_CO);if(_Dx[0]==1){var _Dy = _Dx[2];var _Dz = _Dx[3];var _DA = E(_BA);var _DB = [1,1,E(_DA),_BB,E(_Bw),E(_Bw)];var _DC = [1,1,E(_CL),_CM,E(_Bw),E(_Bw)];var _DD = [1,3,E(_Dy),_Dz,E(_DC),E(_DB)];var _DE = _DD;}else{var _DF = E(_BA);var _DG = [1,2,E(_DF),_BB,E(_CJ),E(_Bw)];var _DE = _DG;}var _Dw = _DE;}var _DH = _Dw;}else{var _DI = E(_BA);var _DJ = [1,1,E(_DI),_BB,E(_Bw),E(_Bw)];var _DH = _DJ;}var _CI = _DH;}return _CI;};var _DK = T(function(){return unCStr("Failure in Data.Map.balanceR");});var _DL = T(function(){return err(_DK);});var _DM = function(_DN,_DO,_DP,_DQ){var _DR = E(_DP);if(_DR[0]==1){var _DS = _DR[1];var _DT = E(_DQ);if(_DT[0]==1){var _DU = _DT[1];var _DV = _DT[2];var _DW = _DT[3];var _DX = _DT[4];var _DY = _DT[5];var _DZ = imul(3,_DS)|0;var _E0 = _DU>_DZ;if(_E0){var _E1 = E(_DX);if(_E1[0]==1){var _E2 = _E1[1];var _E3 = _E1[2];var _E4 = _E1[3];var _E5 = _E1[4];var _E6 = _E1[5];var _E7 = E(_DY);if(_E7[0]==1){var _E8 = _E7[1];var _E9 = imul(2,_E8)|0;var _Ea = _E2<_E9;if(_Ea){var _Eb = E(_DN);var _Ec = 1+_DS|0;var _Ed = _Ec+_E2|0;var _Ee = [1,_Ed,E(_Eb),_DO,E(_DR),E(_E1)];var _Ef = 1+_DS|0;var _Eg = _Ef+_DU|0;var _Eh = [1,_Eg,E(_DV),_DW,E(_Ee),E(_E7)];var _Ei = _Eh;}else{var _Ej = function(_Ek){var _El = E(_DN);var _Em = E(_E6);if(_Em[0]==1){var _En = _Em[1];var _Eo = 1+_E8|0;var _Ep = _Eo+_En|0;var _Eq = [1,_Ep,E(_DV),_DW,E(_Em),E(_E7)];var _Er = 1+_DS|0;var _Es = _Er+_Ek|0;var _Et = [1,_Es,E(_El),_DO,E(_DR),E(_E5)];var _Eu = 1+_DS|0;var _Ev = _Eu+_DU|0;var _Ew = [1,_Ev,E(_E3),_E4,E(_Et),E(_Eq)];var _Ex = _Ew;}else{var _Ey = 1+_E8|0;var _Ez = [1,_Ey,E(_DV),_DW,E(_Bw),E(_E7)];var _EA = 1+_DS|0;var _EB = _EA+_Ek|0;var _EC = [1,_EB,E(_El),_DO,E(_DR),E(_E5)];var _ED = 1+_DS|0;var _EE = _ED+_DU|0;var _EF = [1,_EE,E(_E3),_E4,E(_EC),E(_Ez)];var _Ex = _EF;}return _Ex;};var _EG = E(_E5);if(_EG[0]==1){var _EH = _EG[1];var _EI = _Ej(_EH);}else{var _EI = _Ej(0);}var _Ei = _EI;}var _EJ = _Ei;}else{var _EJ = E(_DL);}var _EK = _EJ;}else{var _EK = E(_DL);}var _EL = _EK;}else{var _EM = E(_DN);var _EN = 1+_DS|0;var _EO = _EN+_DU|0;var _EP = [1,_EO,E(_EM),_DO,E(_DR),E(_DT)];var _EL = _EP;}var _EQ = _EL;}else{var _ER = E(_DN);var _ES = 1+_DS|0;var _ET = [1,_ES,E(_ER),_DO,E(_DR),E(_Bw)];var _EQ = _ET;}var _EU = _EQ;}else{var _EV = E(_DQ);if(_EV[0]==1){var _EW = _EV[1];var _EX = _EV[2];var _EY = _EV[3];var _EZ = _EV[4];var _F0 = _EV[5];var _F1 = E(_EZ);if(_F1[0]==1){var _F2 = _F1[1];var _F3 = _F1[2];var _F4 = _F1[3];var _F5 = _F1[4];var _F6 = _F1[5];var _F7 = E(_F0);if(_F7[0]==1){var _F8 = _F7[1];var _F9 = imul(2,_F8)|0;var _Fa = _F2<_F9;if(_Fa){var _Fb = E(_DN);var _Fc = 1+_F2|0;var _Fd = [1,_Fc,E(_Fb),_DO,E(_Bw),E(_F1)];var _Fe = 1+_EW|0;var _Ff = [1,_Fe,E(_EX),_EY,E(_Fd),E(_F7)];var _Fg = _Ff;}else{var _Fh = function(_Fi){var _Fj = E(_DN);var _Fk = E(_F6);if(_Fk[0]==1){var _Fl = _Fk[1];var _Fm = 1+_F8|0;var _Fn = _Fm+_Fl|0;var _Fo = [1,_Fn,E(_EX),_EY,E(_Fk),E(_F7)];var _Fp = 1+_Fi|0;var _Fq = [1,_Fp,E(_Fj),_DO,E(_Bw),E(_F5)];var _Fr = 1+_EW|0;var _Fs = [1,_Fr,E(_F3),_F4,E(_Fq),E(_Fo)];var _Ft = _Fs;}else{var _Fu = 1+_F8|0;var _Fv = [1,_Fu,E(_EX),_EY,E(_Bw),E(_F7)];var _Fw = 1+_Fi|0;var _Fx = [1,_Fw,E(_Fj),_DO,E(_Bw),E(_F5)];var _Fy = 1+_EW|0;var _Fz = [1,_Fy,E(_F3),_F4,E(_Fx),E(_Fv)];var _Ft = _Fz;}return _Ft;};var _FA = E(_F5);if(_FA[0]==1){var _FB = _FA[1];var _FC = _Fh(_FB);}else{var _FC = _Fh(0);}var _Fg = _FC;}var _FD = _Fg;}else{var _FE = E(_DN);var _FF = [1,1,E(_EX),_EY,E(_Bw),E(_Bw)];var _FG = [1,1,E(_FE),_DO,E(_Bw),E(_Bw)];var _FH = [1,3,E(_F3),_F4,E(_FG),E(_FF)];var _FD = _FH;}var _FI = _FD;}else{var _FJ = E(_F0);if(_FJ[0]==1){var _FK = E(_DN);var _FL = [1,1,E(_FK),_DO,E(_Bw),E(_Bw)];var _FM = [1,3,E(_EX),_EY,E(_FL),E(_FJ)];var _FN = _FM;}else{var _FO = E(_DN);var _FP = [1,2,E(_FO),_DO,E(_Bw),E(_EV)];var _FN = _FP;}var _FI = _FN;}var _FQ = _FI;}else{var _FR = E(_DN);var _FS = [1,1,E(_FR),_DO,E(_Bw),E(_Bw)];var _FQ = _FS;}var _EU = _FQ;}return _EU;};var _FT = function(_FU,_FV){var _FW = E(_FU);var _FX = [1,1,E(_FW),_FV,E(_Bw),E(_Bw)];return _FX;};var _FY = function(_FZ,_G0,_G1){var _G2 = E(_G1);if(_G2[0]==1){var _G3 = _G2[2];var _G4 = _G2[3];var _G5 = _G2[4];var _G6 = _G2[5];var _G7 = _FY(_FZ,_G0,_G6);var _G8 = _DM(_G3,_G4,_G5,_G7);var _G9 = _G8;}else{var _G9 = _FT(_FZ,_G0);}return _G9;};var _Ga = function(_Gb,_Gc,_Gd){var _Ge = E(_Gd);if(_Ge[0]==1){var _Gf = _Ge[2];var _Gg = _Ge[3];var _Gh = _Ge[4];var _Gi = _Ge[5];var _Gj = _Ga(_Gb,_Gc,_Gh);var _Gk = _Bz(_Gf,_Gg,_Gj,_Gi);var _Gl = _Gk;}else{var _Gl = _FT(_Gb,_Gc);}return _Gl;};var _Gm = function(_Gn,_Go,_Gp,_Gq,_Gr,_Gs,_Gt,_Gu){var _Gv = E(_Gp);if(_Gv[0]==1){var _Gw = _Gv[1];var _Gx = _Gv[2];var _Gy = _Gv[3];var _Gz = _Gv[4];var _GA = _Gv[5];var _GB = imul(3,_Gw)|0;var _GC = _GB<_Gq;if(_GC){var _GD = _GE(_Gn,_Go,_Gw,_Gx,_Gy,_Gz,_GA,_Gt);var _GF = _Bz(_Gr,_Gs,_GD,_Gu);var _GG = _GF;}else{var _GH = imul(3,_Gq)|0;var _GI = _GH<_Gw;if(_GI){var _GJ = _Gm(_Gn,_Go,_GA,_Gq,_Gr,_Gs,_Gt,_Gu);var _GK = _DM(_Gx,_Gy,_Gz,_GJ);var _GL = _GK;}else{var _GM = E(_Gn);var _GN = [1,_Gq,E(_Gr),_Gs,E(_Gt),E(_Gu)];var _GO = _Gw+_Gq|0;var _GP = _GO+1|0;var _GQ = [1,_GP,E(_GM),_Go,E(_Gv),E(_GN)];var _GL = _GQ;}var _GG = _GL;}var _GR = _GG;}else{var _GS = [1,_Gq,E(_Gr),_Gs,E(_Gt),E(_Gu)];var _GR = _Ga(_Gn,_Go,_GS);}return _GR;};var _GE = function(_GT,_GU,_GV,_GW,_GX,_GY,_GZ,_H0){var _H1 = E(_H0);if(_H1[0]==1){var _H2 = _H1[1];var _H3 = _H1[2];var _H4 = _H1[3];var _H5 = _H1[4];var _H6 = _H1[5];var _H7 = imul(3,_GV)|0;var _H8 = _H7<_H2;if(_H8){var _H9 = _GE(_GT,_GU,_GV,_GW,_GX,_GY,_GZ,_H5);var _Ha = _Bz(_H3,_H4,_H9,_H6);var _Hb = _Ha;}else{var _Hc = imul(3,_H2)|0;var _Hd = _Hc<_GV;if(_Hd){var _He = _Gm(_GT,_GU,_GZ,_H2,_H3,_H4,_H5,_H6);var _Hf = _DM(_GW,_GX,_GY,_He);var _Hg = _Hf;}else{var _Hh = E(_GT);var _Hi = [1,_GV,E(_GW),_GX,E(_GY),E(_GZ)];var _Hj = _GV+_H2|0;var _Hk = _Hj+1|0;var _Hl = [1,_Hk,E(_Hh),_GU,E(_Hi),E(_H1)];var _Hg = _Hl;}var _Hb = _Hg;}var _Hm = _Hb;}else{var _Hn = [1,_GV,E(_GW),_GX,E(_GY),E(_GZ)];var _Hm = _FY(_GT,_GU,_Hn);}return _Hm;};var _Ho = function(_Hp,_Hq,_Hr,_Hs){var _Ht = E(_Hr);if(_Ht[0]==1){var _Hu = _Ht[1];var _Hv = _Ht[2];var _Hw = _Ht[3];var _Hx = _Ht[4];var _Hy = _Ht[5];var _Hz = E(_Hs);if(_Hz[0]==1){var _HA = _Hz[1];var _HB = _Hz[2];var _HC = _Hz[3];var _HD = _Hz[4];var _HE = _Hz[5];var _HF = imul(3,_Hu)|0;var _HG = _HF<_HA;if(_HG){var _HH = _GE(_Hp,_Hq,_Hu,_Hv,_Hw,_Hx,_Hy,_HD);var _HI = _Bz(_HB,_HC,_HH,_HE);var _HJ = _HI;}else{var _HK = imul(3,_HA)|0;var _HL = _HK<_Hu;if(_HL){var _HM = _Gm(_Hp,_Hq,_Hy,_HA,_HB,_HC,_HD,_HE);var _HN = _DM(_Hv,_Hw,_Hx,_HM);var _HO = _HN;}else{var _HP = E(_Hp);var _HQ = _Hu+_HA|0;var _HR = _HQ+1|0;var _HS = [1,_HR,E(_HP),_Hq,E(_Ht),E(_Hz)];var _HO = _HS;}var _HJ = _HO;}var _HT = _HJ;}else{var _HT = _FY(_Hp,_Hq,_Ht);}var _HU = _HT;}else{var _HU = _Ga(_Hp,_Hq,_Hs);}return _HU;};var _HV = function(_HW,_HX){while(1){var _HY = E(_HX);if(_HY[0]==1){var _HZ = _HY[2];var _I0 = _HY[3];var _I1 = _HY[4];var _I2 = _HY[5];var _I3 = E(_HZ);var _I4 = _I3[1];var _I5 = _HW<_I4;if(_I5){var _I6 = _HV(_HW,_I1);var _I7 = _Ho(_I3,_I0,_I6,_I2);var _I8 = _I7;}else{var _I9 = _HW==_I4;if(_I9){var _Ia = E(_I2);}else{_HW=_HW;_HX=_I2;continue;var _Ia = die("Unreachable!");}var _I8 = _Ia;}var _Ib = _I8;}else{var _Ib = [2];}return _Ib;}};var _Ic = function(_Id,_Ie){var _If = E(_Ie);if(_If[0]==1){var _Ig = _If[2];var _Ih = _If[3];var _Ii = _If[4];var _Ij = _If[5];var _Ik = E(_Ig);var _Il = _Ik[1];var _Im = E(_Id);var _In = _Im[1];var _Io = _In<_Il;if(_Io){var _Ip = _HV(_In,_Ii);var _Iq = _Ho(_Ik,_Ih,_Ip,_Ij);var _Ir = _Iq;}else{var _Is = _In==_Il;var _Ir = _Is?E(_Ij):_HV(_In,_Ij);}var _It = _Ir;}else{var _It = [2];}return _It;};var _Iu = function(_Iv,_Iw){while(1){var _Ix = E(_Iw);if(_Ix[0]==1){var _Iy = _Ix[2];var _Iz = _Ix[3];var _IA = _Ix[4];var _IB = _Ix[5];var _IC = E(_Iy);var _ID = _IC[1];var _IE = _ID<_Iv;if(_IE){var _IF = _Iu(_Iv,_IB);var _IG = _Ho(_IC,_Iz,_IA,_IF);var _IH = _IG;}else{var _II = _ID==_Iv;if(_II){var _IJ = E(_IA);}else{_Iv=_Iv;_Iw=_IA;continue;var _IJ = die("Unreachable!");}var _IH = _IJ;}var _IK = _IH;}else{var _IK = [2];}return _IK;}};var _IL = function(_IM,_IN){var _IO = E(_IN);if(_IO[0]==1){var _IP = _IO[2];var _IQ = _IO[3];var _IR = _IO[4];var _IS = _IO[5];var _IT = E(_IP);var _IU = _IT[1];var _IV = E(_IM);var _IW = _IV[1];var _IX = _IU<_IW;if(_IX){var _IY = _Iu(_IW,_IS);var _IZ = _Ho(_IT,_IQ,_IR,_IY);var _J0 = _IZ;}else{var _J1 = _IU==_IW;var _J0 = _J1?E(_IR):_Iu(_IW,_IR);}var _J2 = _J0;}else{var _J2 = [2];}return _J2;};var _J3 = function(_J4,_J5){while(1){var _J6 = E(_J5);if(_J6[0]==1){var _J7 = _J6[2];var _J8 = _J6[5];var _J9 = E(_J7);var _Ja = _J9[1];var _Jb = _Ja<=_J4;if(_Jb){_J4=_J4;_J5=_J8;continue;var _Jc = die("Unreachable!");}else{var _Jc = E(_J6);}var _Jd = _Jc;}else{var _Jd = [2];}return _Jd;}};var _Je = function(_Jf,_Jg){while(1){var _Jh = E(_Jg);if(_Jh[0]==1){var _Ji = _Jh[2];var _Jj = _Jh[4];var _Jk = E(_Ji);var _Jl = _Jk[1];var _Jm = _Jl>=_Jf;if(_Jm){_Jf=_Jf;_Jg=_Jj;continue;var _Jn = die("Unreachable!");}else{var _Jn = E(_Jh);}var _Jo = _Jn;}else{var _Jo = [2];}return _Jo;}};var _Jp = function(_Jq,_Jr,_Js){while(1){var _Jt = E(_Js);if(_Jt[0]==1){var _Ju = _Jt[2];var _Jv = _Jt[4];var _Jw = _Jt[5];var _Jx = E(_Ju);var _Jy = _Jx[1];var _Jz = _Jy<=_Jq;if(_Jz){_Jq=_Jq;_Jr=_Jr;_Js=_Jw;continue;var _JA = die("Unreachable!");}else{var _JB = _Jy>=_Jr;if(_JB){_Jq=_Jq;_Jr=_Jr;_Js=_Jv;continue;var _JC = die("Unreachable!");}else{var _JC = E(_Jt);}var _JA = _JC;}var _JD = _JA;}else{var _JD = [2];}return _JD;}};var _JE = function(_JF,_JG,_JH){while(1){var _JI = E(_JH);if(_JI[0]==1){var _JJ = _JI[2];var _JK = _JI[4];var _JL = _JI[5];var _JM = E(_JJ);var _JN = _JM[1];var _JO = _JN<=_JF;if(_JO){_JF=_JF;_JG=_JG;_JH=_JL;continue;var _JP = die("Unreachable!");}else{var _JQ = E(_JG);var _JR = _JQ[1];var _JS = _JN>=_JR;var _JT = _JS?_Jp(_JF,_JR,_JK):E(_JI);var _JP = _JT;}var _JU = _JP;}else{var _JU = [2];}return _JU;}};var _JV = function(_JW,_JX,_JY){var _JZ = E(_JY);if(_JZ[0]==1){var _K0 = _JZ[2];var _K1 = _JZ[3];var _K2 = _JZ[4];var _K3 = _JZ[5];var _K4 = E(_K0);var _K5 = _K4[1];var _K6 = _JW<_K5;if(_K6){var _K7 = _JV(_JW,_JX,_K2);var _K8 = _Bz(_K4,_K1,_K7,_K3);var _K9 = _K8;}else{var _Ka = _JW==_K5;if(_Ka){var _Kb = E(_JZ);}else{var _Kc = _JV(_JW,_JX,_K3);var _Kd = _DM(_K4,_K1,_K2,_Kc);var _Kb = _Kd;}var _K9 = _Kb;}var _Ke = _K9;}else{var _Kf = [1,_JW];var _Ke = [1,1,E(_Kf),_JX,E(_Bw),E(_Bw)];}return _Ke;};var _Kg = function(_Kh,_Ki,_Kj,_Kk){var _Kl = E(_Kk);if(_Kl[0]==1){var _Km = _Kl[2];var _Kn = _Kl[3];var _Ko = _Kl[4];var _Kp = _Kl[5];var _Kq = E(_Km);var _Kr = _Kq[1];var _Ks = E(_Kj);if(_Ks[0]==1){var _Kt = _Ks[2];var _Ku = _Ks[3];var _Kv = _Ks[4];var _Kw = _Ks[5];var _Kx = T(function(){var _Ky = [2,E(_Kt)];var _Kz = T(function(){var _KA = E(_Ki);if(_KA[0]==1){var _KB = E(_Kt);var _KC = _KB[1];var _KD = _Kr<=_KC;if(_KD){var _KE = _J3(_KC,_Kp);var _KF = _Kg(_Ky,_Bv,_Kw,_KE);var _KG = _KF;}else{var _KG = _Kg(_Ky,_Bv,_Kw,_Kl);}var _KH = _KG;}else{var _KI = _KA[1];var _KJ = E(_Kt);var _KK = _KJ[1];var _KL = _Kr<=_KK;if(_KL){var _KM = _JE(_KK,_KI,_Kp);var _KN = _Kg(_Ky,_KA,_Kw,_KM);var _KO = _KN;}else{var _KP = E(_KI);var _KQ = _KP[1];var _KR = _Kr>=_KQ;if(_KR){var _KS = _Jp(_KK,_KQ,_Ko);var _KT = _Kg(_Ky,_KA,_Kw,_KS);var _KU = _KT;}else{var _KU = _Kg(_Ky,_KA,_Kw,_Kl);}var _KO = _KU;}var _KH = _KO;}return _KH;});var _KV = E(_Kh);if(_KV[0]==1){var _KW = E(_Kt);var _KX = _KW[1];var _KY = _Kr>=_KX;if(_KY){var _KZ = _Je(_KX,_Ko);var _L0 = _Kg(_Bv,_Ky,_Kv,_KZ);var _L1 = _Ho(_KW,_Ku,_L0,_Kz);var _L2 = _L1;}else{var _L3 = _Kg(_Bv,_Ky,_Kv,_Kl);var _L4 = _Ho(_KW,_Ku,_L3,_Kz);var _L2 = _L4;}var _L5 = _L2;}else{var _L6 = _KV[1];var _L7 = E(_L6);var _L8 = _L7[1];var _L9 = _Kr<=_L8;if(_L9){var _La = _JE(_L8,_Kt,_Kp);var _Lb = _Kg(_KV,_Ky,_Kv,_La);var _Lc = _Ho(_Kt,_Ku,_Lb,_Kz);var _Ld = _Lc;}else{var _Le = E(_Kt);var _Lf = _Le[1];var _Lg = _Kr>=_Lf;if(_Lg){var _Lh = _Jp(_L8,_Lf,_Ko);var _Li = _Kg(_KV,_Ky,_Kv,_Lh);var _Lj = _Ho(_Le,_Ku,_Li,_Kz);var _Lk = _Lj;}else{var _Ll = _Kg(_KV,_Ky,_Kv,_Kl);var _Lm = _Ho(_Le,_Ku,_Ll,_Kz);var _Lk = _Lm;}var _Ld = _Lk;}var _L5 = _Ld;}return _L5;});var _Ln = E(_Ko);if(_Ln[0]==1){var _Lo = E(_Kx);}else{var _Lp = E(_Kp);var _Lo = _Lp[0]==1?E(_Kx):_JV(_Kr,_Kn,_Ks);}var _Lq = _Lo;}else{var _Lr = E(_Kh);if(_Lr[0]==1){var _Ls = E(_Ki);if(_Ls[0]==1){var _Lt = _Ho(_Kq,_Kn,_Ko,_Kp);}else{var _Lu = _Ls[1];var _Lv = _IL(_Lu,_Kp);var _Lw = _Ho(_Kq,_Kn,_Ko,_Lv);var _Lt = _Lw;}var _Lx = _Lt;}else{var _Ly = _Lr[1];var _Lz = E(_Ki);if(_Lz[0]==1){var _LA = _Ic(_Ly,_Ko);var _LB = _Ho(_Kq,_Kn,_LA,_Kp);var _LC = _LB;}else{var _LD = _Lz[1];var _LE = _IL(_LD,_Kp);var _LF = _Ic(_Ly,_Ko);var _LG = _Ho(_Kq,_Kn,_LF,_LE);var _LC = _LG;}var _Lx = _LC;}var _Lq = _Lx;}var _LH = _Lq;}else{var _LH = E(_Kj);}return _LH;};var _LI = function(_LJ,_LK){var _LL = T(function(){var _LM = E(_LK);var _LN = _LM[1];var _LO = _LM[2];var _LP = E(_LN);if(_LP[0]==1){var _LQ = E(_LO);var _LR = _LQ[0]==1?_Kg(_Bv,_Bv,_LP,_LQ):E(_LP);}else{var _LR = E(_LO);}return _LR;});var _LS = [2,_LL];return [1,_LS,_LT];};var _LT = T(function(){return [2,_LI];});var _LU = T(function(){return _dk(_cR,_Bu,_LT);});var _LV = T(function(){return _i8(_cR,_LU);});var _LW = function(_LX,_LY){var _LZ = T(function(){var _M0 = E(_LY);var _M1 = _M0[1];var _M2 = _M0[2];var _M3 = E(_M1);var _M4 = _M3[1];var _M5 = _M3[2];var _M6 = E(_M2);var _M7 = _M6[1];var _M8 = _M6[2];var _M9 = _M6[3];var _Ma = _M6[4];var _Mb = [1,_M7,_M8,_M9,_Ma,_M4,_M5];return _Mb;});var _Mc = [2,_LZ];return [1,_Mc,_Md];};var _Md = T(function(){return [2,_LW];});var _Me = T(function(){return _dk(_cR,_Md,_LV);});var _Mf = function(_Mg,_Mh){var _Mi = T(function(){var _Mj = E(_Mh);var _Mk = _Mj[1];var _Ml = _Mj[2];var _Mm = _Mj[3];var _Mn = _Mj[4];var _Mo = _Mj[5];var _Mp = _Mj[6];var _Mq = [1,_Mk,_Mn,_Mo,_Mp];var _Mr = [1,_Ml,_Mm];var _Ms = [1,_Mr,_Mq];return _Ms;});var _Mt = [2,_Mi];return [1,_Mt,_Mu];};var _Mu = T(function(){return [2,_Mf];});var _Mv = T(function(){return _dk(_cR,_Me,_Mu);});var _Mw = T(function(){return _oo(_2k);});var _Mx = function(_My,_Mz){var _MA = [2,_Mz];return [1,_MA,_MB];};var _MB = T(function(){return [2,_Mx];});var _MC = T(function(){return _dk(_cR,_Mw,_MB);});var _MD = T(function(){return _i8(_cR,_MC);});var _ME = function(_MF,_MG){var _MH = T(function(){var _MI = E(_MG);var _MJ = _MI[1];var _MK = _MI[2];var _ML = E(_MK);var _MM = _ML[1];var _MN = _ML[2];var _MO = _ML[3];var _MP = _ML[4];var _MQ = _ML[5];var _MR = _ML[6];var _MS = [1,_MM,_MN,_MO,_MP,_MQ,_MR,_MJ];return _MS;});var _MT = [2,_MH];return [1,_MT,_MU];};var _MU = T(function(){return [2,_ME];});var _MV = T(function(){return _dk(_cR,_MU,_MD);});var _MW = function(_MX,_MY){var _MZ = T(function(){var _N0 = E(_MY);var _N1 = _N0[6];var _N2 = [1,_N1,_N0];return _N2;});var _N3 = [2,_MZ];return [1,_N3,_N4];};var _N4 = T(function(){return [2,_MW];});var _N5 = T(function(){return _dk(_cR,_MV,_N4);});var _N6 = [1];var _N7 = T(function(){return _p7(_N6);});var _N8 = [1,_ph];var _N9 = T(function(){return [1,_N8,_Na];});var _Nb = function(_Nc,_Nd){return E(_N9);};var _Na = T(function(){return [2,_Nb];});var _Ne = function(_Nf,_Ng){var _Nh = [2,_Ng];return [1,_Nh,_Na];};var _Ni = [2,_Ne];var _Nj = function(_Nk,_Nl){var _Nm = T(function(){var _Nn = E(_Nl);return _Nn[0]==1?E(_N8):[2,_Nn];});return [1,_Nm,_No];};var _No = T(function(){return [2,_Nj];});var _Np = T(function(){return _cS(_cR,_Ni,_No);});var _Nq = T(function(){return _cS(_cR,_Np,_N7);});var _Nr = function(_Ns,_Nt){var _Nu = T(function(){return _q0(_gy,_Nt);});var _Nv = [2,_Nu];return [1,_Nv,_Nw];};var _Nw = T(function(){return [2,_Nr];});var _Nx = T(function(){return _dk(_cR,_Nq,_Nw);});var _Ny = T(function(){return _i8(_cR,_Nx);});var _Nz = function(_NA,_NB){var _NC = T(function(){var _ND = E(_NB);var _NE = _ND[2];var _NF = E(_NE);return _NF;});var _NG = [2,_NC];return [1,_NG,_NH];};var _NH = T(function(){return [2,_Nz];});var _NI = T(function(){return _dk(_cR,_NH,_Ny);});var _NJ = function(_NK,_NL){var _NM = T(function(){var _NN = E(_NL);var _NO = _NN[7];var _NP = [1,_NO,_NN];return _NP;});var _NQ = [2,_NM];return [1,_NQ,_NR];};var _NR = T(function(){return [2,_NJ];});var _NS = T(function(){return _dk(_cR,_NI,_NR);});var _NT = function(_NU,_NV){var _NW = [2,_NV];return [1,_NW,_NX];};var _NX = T(function(){return [2,_NT];});var _NY = function(_NZ,_O0){var _O1 = T(function(){var _O2 = E(_O0);var _O3 = _O2[1];var _O4 = _O2[2];var _O5 = [1,_O4];var _O6 = [1,_O3,_O5];return _O6;});var _O7 = [2,_O1];return [1,_O7,_O8];};var _O8 = T(function(){return [2,_NY];});var _O9 = function(_Oa,_Ob){var _Oc = [2,_Ob];return [1,_Oc,_Od];};var _Od = T(function(){return [2,_O9];});var _Oe = T(function(){return _dk(_cR,_Od,_O8);});var _Of = function(_Og,_Oh){var _Oi = T(function(){var _Oj = E(_Oh);var _Ok = [1,_Oj,_0];return _Ok;});var _Ol = [2,_Oi];return [1,_Ol,_Om];};var _Om = T(function(){return [2,_Of];});var _On = function(_Oo,_Op){var _Oq = T(function(){var _Or = E(_Op);var _Os = _Or[1];var _Ot = _Or[2];var _Ou = E(_Os);var _Ov = E(_Ot);var _Ow = E(_Ou);return _Ow;});var _Ox = [2,_Oq];return [1,_Ox,_Oy];};var _Oy = T(function(){return [2,_On];});var _Oz = function(_OA,_OB){var _OC = T(function(){var _OD = E(_OB);var _OE = _OD[1];var _OF = _OD[2];var _OG = _OD[3];var _OH = T(function(){return _zK(_OG,_OE);});var _OI = [1,_OF,_OH];return _OI;});var _OJ = [2,_OC];return [1,_OJ,_OK];};var _OK = T(function(){return [2,_Oz];});var _OL = function(_OM,_ON){var _OO = T(function(){var _OP = E(_ON);var _OQ = _OP[2];var _OR = [1,_OP,_OQ];return _OR;});var _OS = [2,_OO];return [1,_OS,_OT];};var _OT = T(function(){return [2,_OL];});var _OU = function(_OV,_OW){var _OX = T(function(){var _OY = E(_OW);var _OZ = _OY[1];var _P0 = _OY[2];var _P1 = E(_OZ);var _P2 = _P1[1];var _P3 = _v3(_P0,0);var _P4 = _P2-_P3|0;var _P5 = [1,_P4];return _P5;});var _P6 = [2,_OX];return [1,_P6,_P7];};var _P7 = T(function(){return [2,_OU];});var _P8 = function(_P9,_Pa){var _Pb = E(_P9);var _Pc = _Pb[1];var _Pd = E(_Pa);var _Pe = _Pd[1];var _Pf = _Pc+_Pe|0;var _Pg = [1,_Pf];return _Pg;};var _Ph = function(_Pi,_Pj,_Pk){return _P8(_Pj,_Pk);};var _Pl = T(function(){return _ie(_Ph,_rb);});var _Pm = T(function(){return _dk(_cR,_Pl,_P7);});var _Pn = T(function(){return _i8(_cR,_Pm);});var _Po = function(_Pp,_Pq){var _Pr = T(function(){var _Ps = E(_Pq);var _Pt = _Ps[1];var _Pu = _Ps[2];var _Pv = [1,_Pu,_Pt];return _Pv;});var _Pw = [2,_Pr];return [1,_Pw,_Px];};var _Px = T(function(){return [2,_Po];});var _Py = T(function(){return _dk(_cR,_Px,_Pn);});var _Pz = T(function(){return _dk(_cR,_Py,_OT);});var _PA = T(function(){return _dk(_cR,_Pz,_OK);});var _PB = function(_PC,_PD){var _PE = T(function(){var _PF = E(_PD);var _PG = _PF[2];var _PH = [1,_PF,_PG];return _PH;});var _PI = [2,_PE];return [1,_PI,_PJ];};var _PJ = T(function(){return [2,_PB];});var _PK = T(function(){return _dk(_cR,_PJ,_PA);});var _PL = function(_PM,_PN){var _PO = T(function(){var _PP = E(_PN);var _PQ = _PP[1];var _PR = _PP[2];var _PS = E(_PQ);var _PT = _PS[1];var _PU = _PS[2];var _PV = [1,_PT,_PU,_PR];return _PV;});var _PW = [2,_PO];return [1,_PW,_PX];};var _PX = T(function(){return [2,_PL];});var _PY = T(function(){return _dk(_cR,_PK,_PX);});var _PZ = T(function(){return _kz(_ll,_PY);});var _Q0 = T(function(){return _i8(_cR,_PZ);});var _Q1 = T(function(){return _dk(_cR,_Oy,_Q0);});var _Q2 = T(function(){return _dk(_cR,_Q1,_Om);});var _Q3 = T(function(){return _dk(_cR,_Oe,_Q2);});var _Q4 = T(function(){return _dk(_cR,_Q3,_NX);});var _Q5 = function(_Q6,_Q7){var _Q8 = [2,_Q7];return [1,_Q8,_Q9];};var _Q9 = T(function(){return [2,_Q5];});var _Qa = T(function(){return _dk(_cR,_Q4,_Q9);});var _Qb = T(function(){return _i8(_cR,_Qa);});var _Qc = function(_Qd,_Qe){var _Qf = T(function(){var _Qg = E(_Qe);var _Qh = _Qg[1];var _Qi = _Qg[2];var _Qj = E(_Qh);var _Qk = _Qj[1];var _Ql = _Qj[2];var _Qm = E(_Qi);var _Qn = _Qm[1];var _Qo = _Qm[2];var _Qp = _Qm[3];var _Qq = _Qm[4];var _Qr = _Qm[5];var _Qs = [1,_Qn,_Qo,_Qp,_Qq,_Qr,_Qk,_Ql];return _Qs;});var _Qt = [2,_Qf];return [1,_Qt,_Qu];};var _Qu = T(function(){return [2,_Qc];});var _Qv = T(function(){return _dk(_cR,_Qu,_Qb);});var _Qw = function(_Qx,_Qy){var _Qz = T(function(){var _QA = E(_Qy);var _QB = _QA[1];var _QC = _QA[2];var _QD = _QA[3];var _QE = _QA[4];var _QF = _QA[5];var _QG = _QA[6];var _QH = _QA[7];var _QI = [1,_QC,_QD,_QE,_QG,_QH];var _QJ = [1,_QB,_QF];var _QK = [1,_QJ,_QI];return _QK;});var _QL = [2,_Qz];return [1,_QL,_QM];};var _QM = T(function(){return [2,_Qw];});var _QN = T(function(){return _dk(_cR,_Qv,_QM);});var _QO = [1,(-10)];var _QP = [1,_5k,_QO];var _QQ = T(function(){return _qZ(_QP);});var _QR = function(_QS){var _QT = E(_QS);if(_QT[0]==1){var _QU = _QT[1];var _QV = [1,_QU];}else{var _QW = _QT[1];var _QX = [1,_QW];var _QV = [2,_QX];}return _QV;};var _QY = function(_QZ){var _R0 = E(_QZ);var _R1 = _R0[1];var _R2 = E(_R1);var _R3 = _R2[2];var _R4 = E(_R3);var _R5 = _R4[1];var _R6 = _R5>0;return _R6;};var _R7 = T(function(){return _qn(_qX,_QY);});var _R8 = function(_R9){var _Ra = T(function(){var _Rb = _lm(_mH,_R9);var _Rc = _dk(_cR,_Rb,_QQ);var _Rd = _ho(_cR,_QR,_Rc);var _Re = _dk(_cR,_Rd,_qY);return _Re;});return _dk(_cR,_R7,_Ra);};var _Rf = function(_Rg){var _Rh = E(_Rg);var _Ri = _Rh[1];var _Rj = _R8(_Ri);return _Rj;};var _Rk = T(function(){return _Ah(_cR,_Rf,_2k);});var _Rl = function(_Rm,_Rn){var _Ro = [2,_Rn];return [1,_Ro,_Rp];};var _Rp = T(function(){return [2,_Rl];});var _Rq = T(function(){return _dk(_cR,_Rk,_Rp);});var _Rr = T(function(){return _i8(_cR,_Rq);});var _Rs = function(_Rt,_Ru){var _Rv = T(function(){var _Rw = E(_Ru);var _Rx = _Rw[1];var _Ry = _Rw[2];var _Rz = E(_Ry);var _RA = _Rz[1];var _RB = _Rz[2];var _RC = _Rz[3];var _RD = _Rz[4];var _RE = _Rz[5];var _RF = [1,_RA,_RB,_RC,_RD,_RE,_Rx];return _RF;});var _RG = [2,_Rv];return [1,_RG,_RH];};var _RH = T(function(){return [2,_Rs];});var _RI = T(function(){return _dk(_cR,_RH,_Rr);});var _RJ = function(_RK,_RL){var _RM = T(function(){var _RN = E(_RL);var _RO = _RN[1];var _RP = _RN[2];var _RQ = _RN[3];var _RR = _RN[4];var _RS = _RN[5];var _RT = _RN[6];var _RU = _RN[7];var _RV = [1,_RP,_RQ,_RR,_RS,_RU];var _RW = [1,_RO,_RT];var _RX = [1,_RW,_RV];return _RX;});var _RY = [2,_RM];return [1,_RY,_RZ];};var _RZ = T(function(){return [2,_RJ];});var _S0 = T(function(){return _dk(_cR,_RI,_RZ);});var _S1 = T(function(){return _oo(_2k);});var _S2 = function(_S3,_S4){var _S5 = [2,_S4];return [1,_S5,_S6];};var _S6 = T(function(){return [2,_S2];});var _S7 = T(function(){return _dk(_cR,_S1,_S6);});var _S8 = T(function(){return _i8(_cR,_S7);});var _S9 = function(_Sa,_Sb){var _Sc = T(function(){var _Sd = E(_Sb);var _Se = _Sd[1];var _Sf = _Sd[2];var _Sg = E(_Sf);var _Sh = _Sg[1];var _Si = _Sg[2];var _Sj = _Sg[3];var _Sk = _Sg[4];var _Sl = _Sg[5];var _Sm = _Sg[6];var _Sn = [1,_Sh,_Si,_Sj,_Sk,_Sl,_Sm,_Se];return _Sn;});var _So = [2,_Sc];return [1,_So,_Sp];};var _Sp = T(function(){return [2,_S9];});var _Sq = T(function(){return _dk(_cR,_Sp,_S8);});var _Sr = function(_Ss,_St){var _Su = T(function(){var _Sv = E(_St);var _Sw = _Sv[6];var _Sx = [1,_Sw,_Sv];return _Sx;});var _Sy = [2,_Su];return [1,_Sy,_Sz];};var _Sz = T(function(){return [2,_Sr];});var _SA = T(function(){return _dk(_cR,_Sq,_Sz);});var _SB = T(function(){return _dk(_cR,_SA,_S0);});var _SC = T(function(){return _dk(_cR,_SB,_QN);});var _SD = T(function(){return _dk(_cR,_SC,_NS);});var _SE = T(function(){return _dk(_cR,_SD,_N5);});var _SF = T(function(){return _dk(_cR,_SE,_Mv);});var _SG = T(function(){return _dk(_cR,_SF,_pX);});var _SH = T(function(){return _dk(_cR,_SG,_p6);});var _SI = T(function(){return _dk(_cR,_SH,_on);});var _SJ = function(_SK,_SL,_SM){while(1){var _SN = E(_SM);if(_SN[0]==1){var _SO = [1,_SL];var _SP = [1,_SK,_SO];}else{var _SQ = _SN[1];var _SR = _SN[2];var _SS = E(_SQ);var _ST = _SS[1];var _SU = _SS[2];var _SV = E(_SU);var _SW = _SV[1];var _SX = _SL<_SW;if(_SX){_SK=_SK;_SL=_SL;_SM=_SR;continue;var _SY = die("Unreachable!");}else{var _SZ = _SL==_SW;if(_SZ){_SK=_SK;_SL=_SL;_SM=_SR;continue;var _T0 = die("Unreachable!");}else{_SK=_ST;_SL=_SW;_SM=_SR;continue;var _T0 = die("Unreachable!");}var _SY = _T0;}var _SP = _SY;}return _SP;}};var _T1 = function(_T2,_T3,_T4,_T5,_T6,_T7){var _T8 = -_T3;var _T9 = _T6+_T8;var _Ta = -_T2;var _Tb = _T5+_Ta;var _Tc = _T4+_T7;var _Td = _T4+_T7;var _Te = _Td*_Tc;var _Tf = _T9*_T9;var _Tg = _Tb*_Tb;var _Th = _Tg+_Tf;var _Ti = _Th<=_Te;if(_Ti){var _Tj = T(function(){var _Tk = _T9*_T9;var _Tl = _Tb*_Tb;var _Tm = _Tl+_Tk;var _Tn = Math.sqrt(_Tm);var _To = 1/_Tn;var _Tp = [1,_To];return _Tp;});var _Tq = T(function(){var _Tr = E(_Tj);var _Ts = _Tr[1];var _Tt = _Ts*_T9;var _Tu = [1,_Tt];return _Tu;});var _Tv = T(function(){var _Tw = E(_Tj);var _Tx = _Tw[1];var _Ty = _Tx*_Tb;var _Tz = [1,_Ty];return _Tz;});var _TA = [1,_Tv,_Tq];var _TB = [1,_TA];var _TC = [2,_TB];}else{var _TC = [1];}return _TC;};var _TD = [1,(-1)];var _TE = [1,0];var _TF = [1,_TE,_TD];var _TG = [1,_TF];var _TH = [1,1];var _TI = [1,_TE,_TH];var _TJ = [1,_TI];var _TK = [1,_TD,_TE];var _TL = [1,_TK];var _TM = [1,_TH,_TE];var _TN = [1,_TM];var _TO = function(_TP,_TQ,_TR,_TS){var _TT = A(_TP,[_TR]);if(_TT[0]==1){var _TU = [1];}else{var _TV = _TT[1];var _TW = A(_TQ,[_TS]);if(_TW[0]==1){var _TX = [1];}else{var _TY = _TW[1];var _TZ = E(_TV);var _U0 = _TZ[1];var _U1 = _TZ[2];var _U2 = E(_U0);var _U3 = _U2[1];var _U4 = _U2[2];var _U5 = E(_TY);var _U6 = _U5[1];var _U7 = _U5[2];var _U8 = _U5[3];var _U9 = E(_U6);var _Ua = _U9[1];var _Ub = _U9[2];var _Uc = E(_U7);var _Ud = _Uc[1];var _Ue = _Uc[2];var _Uf = E(_U3);var _Ug = _Uf[1];var _Uh = E(_Ua);var _Ui = _Uh[1];var _Uj = E(_U8);var _Uk = _Uj[1];var _Ul = _Ui+_Uk;var _Um = T(function(){var _Un = E(_Ub);var _Uo = _Un[1];var _Up = _Uo+_Uk;var _Uq = [1,_Up];return _Uq;});var _Ur = T(function(){var _Us = E(_Ud);var _Ut = _Us[1];var _Uu = _Ut-_Uk;var _Uv = T(function(){var _Uw = T(function(){var _Ux = E(_Ue);var _Uy = _Ux[1];var _Uz = _Uy-_Uk;var _UA = [1,_Uz];return _UA;});var _UB = T(function(){var _UC = T(function(){var _UD = E(_U1);var _UE = _UD[1];var _UF = _Ut+_UE;var _UG = _Ug>_UF;if(_UG){var _UH = [1];}else{var _UI = _Ui-_UE;var _UJ = _Ug<_UI;if(_UJ){var _UK = [1];}else{var _UL = E(_U4);var _UM = _UL[1];var _UN = E(_Ue);var _UO = _UN[1];var _UP = _UO+_UE;var _UQ = _UM>_UP;if(_UQ){var _UR = [1];}else{var _US = E(_Ub);var _UT = _US[1];var _UU = _UT-_UE;var _UV = _UM<_UU;if(_UV){var _UW = [1];}else{var _UX = T(function(){var _UY = _UO-_UM;var _UZ = [1,_UY];var _V0 = [1,_TJ,_UZ];var _V1 = [2,_V0,_2k];var _V2 = _UM-_UT;var _V3 = [1,_V2];var _V4 = [1,_TG,_V3];var _V5 = [2,_V4,_V1];var _V6 = _Ut-_Ug;var _V7 = [1,_V6];var _V8 = [1,_TN,_V7];var _V9 = [2,_V8,_V5];var _Va = _Ug-_Ui;var _Vb = _SJ(_TL,_Va,_V9);var _Vc = _Vb[1];var _Vd = E(_Vc);return _Vd;});var _UW = [2,_UX];}var _UR = _UW;}var _UK = _UR;}var _UH = _UK;}return _UH;});var _Ve = _Ug<=_Ul;if(_Ve){var _Vf = E(_U4);var _Vg = _Vf[1];var _Vh = E(_Um);var _Vi = _Vh[1];var _Vj = _Vg<=_Vi;if(_Vj){var _Vk = E(_Uw);var _Vl = _Vk[1];var _Vm = E(_U1);var _Vn = _Vm[1];var _Vo = -_Vl;var _Vp = _Vg+_Vo;var _Vq = -_Ul;var _Vr = _Ug+_Vq;var _Vs = _Uk+_Vn;var _Vt = _Uk+_Vn;var _Vu = _Vt*_Vs;var _Vv = _Vp*_Vp;var _Vw = _Vr*_Vr;var _Vx = _Vw+_Vv;var _Vy = _Vx<=_Vu;if(_Vy){var _Vz = T(function(){var _VA = _Vp*_Vp;var _VB = _Vr*_Vr;var _VC = _VB+_VA;var _VD = Math.sqrt(_VC);var _VE = 1/_VD;var _VF = [1,_VE];return _VF;});var _VG = T(function(){var _VH = E(_Vz);var _VI = _VH[1];var _VJ = _VI*_Vp;var _VK = [1,_VJ];return _VK;});var _VL = T(function(){var _VM = E(_Vz);var _VN = _VM[1];var _VO = _VN*_Vr;var _VP = [1,_VO];return _VP;});var _VQ = [1,_VL,_VG];var _VR = [1,_VQ];var _VS = [2,_VR];}else{var _VS = [1];}var _VT = _VS;}else{var _VT = E(_UC);}var _VU = _VT;}else{var _VU = E(_UC);}return _VU;});var _VV = _Ug>=_Uu;if(_VV){var _VW = E(_U4);var _VX = _VW[1];var _VY = E(_Uw);var _VZ = _VY[1];var _W0 = _VX>=_VZ;if(_W0){var _W1 = E(_U1);var _W2 = _W1[1];var _W3 = -_VZ;var _W4 = _VX+_W3;var _W5 = -_Uu;var _W6 = _Ug+_W5;var _W7 = _Uk+_W2;var _W8 = _Uk+_W2;var _W9 = _W8*_W7;var _Wa = _W4*_W4;var _Wb = _W6*_W6;var _Wc = _Wb+_Wa;var _Wd = _Wc<=_W9;if(_Wd){var _We = T(function(){var _Wf = _W4*_W4;var _Wg = _W6*_W6;var _Wh = _Wg+_Wf;var _Wi = Math.sqrt(_Wh);var _Wj = 1/_Wi;var _Wk = [1,_Wj];return _Wk;});var _Wl = T(function(){var _Wm = E(_We);var _Wn = _Wm[1];var _Wo = _Wn*_W4;var _Wp = [1,_Wo];return _Wp;});var _Wq = T(function(){var _Wr = E(_We);var _Ws = _Wr[1];var _Wt = _Ws*_W6;var _Wu = [1,_Wt];return _Wu;});var _Wv = [1,_Wq,_Wl];var _Ww = [1,_Wv];var _Wx = [2,_Ww];}else{var _Wx = [1];}var _Wy = _Wx;}else{var _Wy = E(_UB);}var _Wz = _Wy;}else{var _Wz = E(_UB);}return _Wz;});var _WA = _Ug>=_Uu;if(_WA){var _WB = E(_U4);var _WC = _WB[1];var _WD = E(_Um);var _WE = _WD[1];var _WF = _WC<=_WE;if(_WF){var _WG = E(_U1);var _WH = _WG[1];var _WI = -_WE;var _WJ = _WC+_WI;var _WK = -_Uu;var _WL = _Ug+_WK;var _WM = _Uk+_WH;var _WN = _Uk+_WH;var _WO = _WN*_WM;var _WP = _WJ*_WJ;var _WQ = _WL*_WL;var _WR = _WQ+_WP;var _WS = _WR<=_WO;if(_WS){var _WT = T(function(){var _WU = _WJ*_WJ;var _WV = _WL*_WL;var _WW = _WV+_WU;var _WX = Math.sqrt(_WW);var _WY = 1/_WX;var _WZ = [1,_WY];return _WZ;});var _X0 = T(function(){var _X1 = E(_WT);var _X2 = _X1[1];var _X3 = _X2*_WJ;var _X4 = [1,_X3];return _X4;});var _X5 = T(function(){var _X6 = E(_WT);var _X7 = _X6[1];var _X8 = _X7*_WL;var _X9 = [1,_X8];return _X9;});var _Xa = [1,_X5,_X0];var _Xb = [1,_Xa];var _Xc = [2,_Xb];}else{var _Xc = [1];}var _Xd = _Xc;}else{var _Xd = E(_Uv);}var _Xe = _Xd;}else{var _Xe = E(_Uv);}return _Xe;});var _Xf = _Ug<=_Ul;if(_Xf){var _Xg = E(_U4);var _Xh = _Xg[1];var _Xi = E(_Um);var _Xj = _Xi[1];var _Xk = _Xh<=_Xj;if(_Xk){var _Xl = E(_U1);var _Xm = _Xl[1];var _Xn = _T1(_Ul,_Xj,_Uk,_Ug,_Xh,_Xm);var _Xo = _Xn;}else{var _Xo = E(_Ur);}var _Xp = _Xo;}else{var _Xp = E(_Ur);}var _TX = _Xp;}var _TU = _TX;}return _TU;};var _Xq = function(_Xr,_Xs,_Xt,_Xu,_Xv){var _Xw = E(_Xv);if(_Xw[0]==1){var _Xx = _Xw[1];var _Xy = _Xw[2];var _Xz = _Xw[3];var _XA = _Xw[4];var _XB = _Xw[5];var _XC = T(function(){var _XD = _Xq(_Xx,_Xy,_Xz,_XA,_XB);var _XE = _XD[1];var _XF = _XD[2];var _XG = [1,_XE,_XF];return _XG;});var _XH = T(function(){var _XI = E(_XC);var _XJ = _XI[2];var _XK = _Bz(_Xs,_Xt,_Xu,_XJ);return _XK;});var _XL = T(function(){var _XM = E(_XC);var _XN = _XM[1];var _XO = E(_XN);return _XO;});var _XP = [1,_XL,_XH];}else{var _XQ = [1,_Xs,_Xt];var _XP = [1,_XQ,_Xu];}return _XP;};var _XR = function(_XS,_XT,_XU,_XV,_XW){var _XX = E(_XV);if(_XX[0]==1){var _XY = _XX[1];var _XZ = _XX[2];var _Y0 = _XX[3];var _Y1 = _XX[4];var _Y2 = _XX[5];var _Y3 = T(function(){var _Y4 = _XR(_XY,_XZ,_Y0,_Y1,_Y2);var _Y5 = _Y4[1];var _Y6 = _Y4[2];var _Y7 = [1,_Y5,_Y6];return _Y7;});var _Y8 = T(function(){var _Y9 = E(_Y3);var _Ya = _Y9[2];var _Yb = _DM(_XT,_XU,_Ya,_XW);return _Yb;});var _Yc = T(function(){var _Yd = E(_Y3);var _Ye = _Yd[1];var _Yf = E(_Ye);return _Yf;});var _Yg = [1,_Yc,_Y8];}else{var _Yh = [1,_XT,_XU];var _Yg = [1,_Yh,_XW];}return _Yg;};var _Yi = function(_Yj,_Yk){var _Yl = E(_Yj);if(_Yl[0]==1){var _Ym = _Yl[1];var _Yn = _Yl[2];var _Yo = _Yl[3];var _Yp = _Yl[4];var _Yq = _Yl[5];var _Yr = E(_Yk);if(_Yr[0]==1){var _Ys = _Yr[1];var _Yt = _Yr[2];var _Yu = _Yr[3];var _Yv = _Yr[4];var _Yw = _Yr[5];var _Yx = _Ym>_Ys;if(_Yx){var _Yy = _Xq(_Ym,_Yn,_Yo,_Yp,_Yq);var _Yz = _Yy[1];var _YA = _Yy[2];var _YB = E(_Yz);var _YC = _YB[1];var _YD = _YB[2];var _YE = _DM(_YC,_YD,_YA,_Yr);var _YF = _YE;}else{var _YG = _XR(_Ys,_Yt,_Yu,_Yv,_Yw);var _YH = _YG[1];var _YI = _YG[2];var _YJ = E(_YH);var _YK = _YJ[1];var _YL = _YJ[2];var _YM = _Bz(_YK,_YL,_Yl,_YI);var _YF = _YM;}var _YN = _YF;}else{var _YN = E(_Yl);}var _YO = _YN;}else{var _YO = E(_Yk);}return _YO;};var _YP = function(_YQ,_YR,_YS,_YT,_YU,_YV){var _YW = E(_YQ);if(_YW[0]==1){var _YX = _YW[1];var _YY = _YW[2];var _YZ = _YW[3];var _Z0 = _YW[4];var _Z1 = _YW[5];var _Z2 = imul(3,_YX)|0;var _Z3 = _Z2<_YR;if(_Z3){var _Z4 = _Z5(_YX,_YY,_YZ,_Z0,_Z1,_YU);var _Z6 = _Bz(_YS,_YT,_Z4,_YV);var _Z7 = _Z6;}else{var _Z8 = imul(3,_YR)|0;var _Z9 = _Z8<_YX;if(_Z9){var _Za = _YP(_Z1,_YR,_YS,_YT,_YU,_YV);var _Zb = _DM(_YY,_YZ,_Z0,_Za);var _Zc = _Zb;}else{var _Zd = [1,_YR,E(_YS),_YT,E(_YU),E(_YV)];var _Zc = _Yi(_YW,_Zd);}var _Z7 = _Zc;}var _Ze = _Z7;}else{var _Ze = [1,_YR,E(_YS),_YT,E(_YU),E(_YV)];}return _Ze;};var _Z5 = function(_Zf,_Zg,_Zh,_Zi,_Zj,_Zk){var _Zl = E(_Zk);if(_Zl[0]==1){var _Zm = _Zl[1];var _Zn = _Zl[2];var _Zo = _Zl[3];var _Zp = _Zl[4];var _Zq = _Zl[5];var _Zr = imul(3,_Zf)|0;var _Zs = _Zr<_Zm;if(_Zs){var _Zt = _Z5(_Zf,_Zg,_Zh,_Zi,_Zj,_Zp);var _Zu = _Bz(_Zn,_Zo,_Zt,_Zq);var _Zv = _Zu;}else{var _Zw = imul(3,_Zm)|0;var _Zx = _Zw<_Zf;if(_Zx){var _Zy = _YP(_Zj,_Zm,_Zn,_Zo,_Zp,_Zq);var _Zz = _DM(_Zg,_Zh,_Zi,_Zy);var _ZA = _Zz;}else{var _ZB = [1,_Zf,E(_Zg),_Zh,E(_Zi),E(_Zj)];var _ZA = _Yi(_ZB,_Zl);}var _Zv = _ZA;}var _ZC = _Zv;}else{var _ZC = [1,_Zf,E(_Zg),_Zh,E(_Zi),E(_Zj)];}return _ZC;};var _ZD = function(_ZE,_ZF){var _ZG = E(_ZE);if(_ZG[0]==1){var _ZH = _ZG[1];var _ZI = _ZG[2];var _ZJ = _ZG[3];var _ZK = _ZG[4];var _ZL = _ZG[5];var _ZM = E(_ZF);if(_ZM[0]==1){var _ZN = _ZM[1];var _ZO = _ZM[2];var _ZP = _ZM[3];var _ZQ = _ZM[4];var _ZR = _ZM[5];var _ZS = imul(3,_ZH)|0;var _ZT = _ZS<_ZN;if(_ZT){var _ZU = _Z5(_ZH,_ZI,_ZJ,_ZK,_ZL,_ZQ);var _ZV = _Bz(_ZO,_ZP,_ZU,_ZR);var _ZW = _ZV;}else{var _ZX = imul(3,_ZN)|0;var _ZY = _ZX<_ZH;if(_ZY){var _ZZ = _YP(_ZL,_ZN,_ZO,_ZP,_ZQ,_ZR);var _100 = _DM(_ZI,_ZJ,_ZK,_ZZ);var _101 = _100;}else{var _101 = _Yi(_ZG,_ZM);}var _ZW = _101;}var _102 = _ZW;}else{var _102 = E(_ZG);}var _103 = _102;}else{var _103 = E(_ZF);}return _103;};var _104 = function(_105,_106){var _107 = E(_106);if(_107[0]==1){var _108 = _107[2];var _109 = _107[3];var _10a = _107[4];var _10b = _107[5];var _10c = A(_105,[_108,_109]);if(_10c){var _10d = _104(_105,_10b);var _10e = _104(_105,_10a);var _10f = _Ho(_108,_109,_10e,_10d);var _10g = _10f;}else{var _10h = _104(_105,_10b);var _10i = _104(_105,_10a);var _10j = _ZD(_10i,_10h);var _10g = _10j;}var _10k = _10g;}else{var _10k = [2];}return _10k;};var _10l = function(_10m){while(1){var r=(function(_10n){var _10o = E(_10n);if(_10o[0]==1){var _10p = [1];}else{var _10q = _10o[1];var _10r = _10o[2];var _10s = E(_10q);if(_10s[0]==1){_10m=_10r;return null;var _10t = die("Unreachable!");}else{var _10u = _10s[1];var _10v = T(function(){return _10l(_10r);});var _10t = [2,_10u,_10v];}var _10p = _10t;}return _10p;})(_10m);if(null!==r)return r;}};var _10w = function(_10x,_10y){while(1){var r=(function(_10z,_10A){var _10B = E(_10A);if(_10B[0]==1){var _10C = [1];}else{var _10D = _10B[1];var _10E = _10B[2];var _10F = A(_10z,[_10D]);if(_10F){var _10G = T(function(){return _10w(_10z,_10E);});var _10H = [2,_10D,_10G];}else{_10x=_10z;_10y=_10E;return null;var _10H = die("Unreachable!");}var _10C = _10H;}return _10C;})(_10x,_10y);if(null!==r)return r;}};var _10I = function(_10J){var _10K = E(_10J);var _10L = _10K[1];var _10M = [1,_10L,_5h];var _10N = [2,_10M];return _10N;};var _10O = [1,400];var _10P = function(_10Q){var _10R = E(_10Q);var _10S = _10R[1];var _10T = T(function(){var _10U = E(_10S);var _10V = _10U[1];var _10W = _10V+50;var _10X = [1,_10W];return _10X;});var _10Y = [1,_10T,_10O];var _10Z = [1,_10S,_6D];var _110 = [1,_10Z,_10Y,_6B];var _111 = [2,_110];return _111;};var _112 = function(_113){var _114 = E(_113);var _115 = _114[1];var _116 = [1,_115,_67];var _117 = [2,_116];return _117;};var _118 = function(_119,_11a){var _11b = E(_11a);if(_11b[0]==1){var _11c = E(_119);var _11d = _11c[1];var _11e = _11c[2];var _11f = T(function(){var _11g = E(_11e);var _11h = _11g[1];var _11i = _11h+20;var _11j = [1,_11i];return _11j;});var _11k = T(function(){var _11l = E(_11d);var _11m = _11l[1];var _11n = _11m+60;var _11o = [1,_11n];return _11o;});var _11p = [1,_11k,_11f];var _11q = [1,_11c,_11p,_5h];var _11r = [2,_11q];var _11s = _11r;}else{var _11s = [1];}return _11s;};var _11t = function(_11u){var _11v = E(_11u);var _11w = _11v[2];var _11x = _11v[3];var _11y = _118(_11w,_11x);return _11y;};var _11z = function(_11A,_11B,_11C){var _11D = E(_11C);if(_11D[0]==1){var _11E = _11D[1];var _11F = _11D[2];var _11G = _11D[3];var _11H = _11D[4];var _11I = _11D[5];var _11J = E(_11F);var _11K = _11J[1];var _11L = _11A<_11K;if(_11L){var _11M = _11z(_11A,_11B,_11H);var _11N = _Bz(_11J,_11G,_11M,_11I);var _11O = _11N;}else{var _11P = _11A==_11K;if(_11P){var _11Q = [1,_11A];var _11R = [1,_11E,E(_11Q),_11B,E(_11H),E(_11I)];}else{var _11S = _11z(_11A,_11B,_11I);var _11T = _DM(_11J,_11G,_11H,_11S);var _11R = _11T;}var _11O = _11R;}var _11U = _11O;}else{var _11V = [1,_11A];var _11U = [1,1,E(_11V),_11B,E(_Bw),E(_Bw)];}return _11U;};var _11W = function(_11X,_11Y,_11Z){var _120 = E(_11X);var _121 = _120[1];var _122 = E(_11Z);if(_122[0]==1){var _123 = _122[1];var _124 = _122[2];var _125 = _122[3];var _126 = _122[4];var _127 = _122[5];var _128 = E(_124);var _129 = _128[1];var _12a = _121<_129;if(_12a){var _12b = _11z(_121,_11Y,_126);var _12c = _Bz(_128,_125,_12b,_127);var _12d = _12c;}else{var _12e = _121==_129;if(_12e){var _12f = [1,_123,E(_120),_11Y,E(_126),E(_127)];}else{var _12g = _11z(_121,_11Y,_127);var _12h = _DM(_128,_125,_126,_12g);var _12f = _12h;}var _12d = _12f;}var _12i = _12d;}else{var _12i = [1,1,E(_120),_11Y,E(_Bw),E(_Bw)];}return _12i;};var _12j = function(_12k,_12l,_12m){while(1){var r=(function(_12n,_12o,_12p){var _12q = E(_12p);if(_12q[0]==1){var _12r = [1,_12n,_12o];}else{var _12s = _12q[1];var _12t = _12q[2];var _12u = E(_12s);var _12v = _12u[1];var _12w = _12u[2];var _12x = E(_12v);var _12y = _12x[1];var _12z = _12x[2];var _12A = E(_12w);var _12B = _12A[1];var _12C = _12A[2];var _12D = _TO(_112,_11t,_12C,_12z);if(_12D[0]==1){_12k=_12n;_12l=_12o;_12m=_12t;return null;var _12E = die("Unreachable!");}else{var _12F = _12D[1];var _12G = T(function(){return _11W(_12B,_12F,_12o);});var _12H = T(function(){return _11W(_12y,_12F,_12n);});_12k=_12H;_12l=_12G;_12m=_12t;return null;var _12E = die("Unreachable!");}var _12r = _12E;}return _12r;})(_12k,_12l,_12m);if(null!==r)return r;}};var _12I = function(_12J){while(1){var _12K = E(_12J);if(_12K[0]==1){var _12L = [2];}else{var _12M = _12K[1];var _12N = _12K[2];var _12O = E(_12M);var _12P = _12O[1];var _12Q = _12O[2];var _12R = E(_12Q);if(_12R[0]==1){_12J=_12N;continue;var _12S = die("Unreachable!");}else{var _12T = _12R[1];var _12U = _12I(_12N);var _12V = _11W(_12P,_12T,_12U);var _12S = _12V;}var _12L = _12S;}return _12L;}};var _12W = function(_12X,_12Y){var _12Z = function(_130){var _131 = E(_130);var _132 = _131[1];var _133 = _131[2];var _134 = T(function(){return _TO(_10I,_11t,_12X,_133);});var _135 = [1,_132,_134];return _135;};var _136 = _q0(_12Z,_12Y);var _137 = _12I(_136);return _137;};var _138 = [1,_1E,_5k];var _139 = [1,_138];var _13a = [1,(-1)];var _13b = [1,_13a,_5k];var _13c = [1,_13b];var _13d = [1,_5k,_1E];var _13e = [1,_13d];var _13f = [2,_13e,_2k];var _13g = function(_13h,_13i){var _13j = T(function(){var _13k = _13h>=600;if(_13k){var _13l = T(function(){var _13m = E(_13i);var _13n = _13m[1];var _13o = _13n<=0;var _13p = _13o?E(_13f):[1];return _13p;});var _13q = [2,_13c,_13l];}else{var _13r = E(_13i);var _13s = _13r[1];var _13t = _13s<=0;var _13u = _13t?E(_13f):[1];var _13q = _13u;}return _13q;});var _13v = _13h<=0;return _13v?[2,_139,_13j]:E(_13j);};var _13w = function(_13x){var _13y = E(_13x);var _13z = _13y[1];var _13A = E(_13z);var _13B = _13A[1];var _13C = _13A[2];var _13D = E(_13B);var _13E = _13D[1];var _13F = _13g(_13E,_13C);return _13F;};var _13G = function(_13H,_13I){var _13J = function(_13K){var _13L = E(_13K);if(_13L[0]==1){var _13M = [1];}else{var _13N = _13L[1];var _13O = _13L[2];var _13P = T(function(){return _13J(_13O);});var _13Q = function(_13R){var _13S = E(_13R);if(_13S[0]==1){var _13T = E(_13P);}else{var _13U = _13S[1];var _13V = _13S[2];var _13W = T(function(){return _13Q(_13V);});var _13X = [1,_13N,_13U];var _13T = [2,_13X,_13W];}return _13T;};var _13M = _13Q(_13I);}return _13M;};return _13J(_13H);};var _13Y = function(_13Z,_140,_141,_142,_143){var _144 = T(function(){var _145 = _13G(_142,_143);var _146 = _12j(_Bw,_Bw,_145);var _147 = _146[1];var _148 = _146[2];var _149 = [1,_147,_148];return _149;});var _14a = T(function(){var _14b = E(_141);var _14c = _14b[2];var _14d = E(_14c);return _14d;});var _14e = T(function(){var _14f = E(_144);var _14g = _14f[2];var _14h = E(_14g);return _14h;});var _14i = T(function(){var _14j = E(_144);var _14k = _14j[1];var _14l = E(_14k);return _14l;});var _14m = T(function(){var _14n = T(function(){return _TO(_10I,_10P,_141,_13Z);});var _14o = [2,_14n,_2k];var _14p = _10l(_14o);var _14q = function(_14r){var _14s = E(_14r);var _14t = _14s[1];var _14u = E(_14t);var _14v = _14u[1];var _14w = _14u[2];var _14x = E(_14a);var _14y = _14x[1];var _14z = _14x[2];var _14A = E(_14v);var _14B = _14A[1];var _14C = E(_14y);var _14D = _14C[1];var _14E = E(_14w);var _14F = _14E[1];var _14G = E(_14z);var _14H = _14G[1];var _14I = _14F*_14H;var _14J = _14B*_14D;var _14K = _14J+_14I;var _14L = _14K<0;return _14L;};var _14M = _10w(_14q,_14p);return _14M;});var _14N = T(function(){return _13w(_141);});var _14O = T(function(){var _14P = _12W(_141,_142);var _14Q = function(_14R,_14S){var _14T = E(_14S);var _14U = _14T[1];var _14V = E(_14U);var _14W = _14V[1];var _14X = _14V[2];var _14Y = E(_14a);var _14Z = _14Y[1];var _150 = _14Y[2];var _151 = E(_14W);var _152 = _151[1];var _153 = E(_14Z);var _154 = _153[1];var _155 = E(_14X);var _156 = _155[1];var _157 = E(_150);var _158 = _157[1];var _159 = _156*_158;var _15a = _152*_154;var _15b = _15a+_159;var _15c = _15b<0;return _15c;};var _15d = _104(_14Q,_14P);return _15d;});return [1,_140,_14O,_14N,_14m,_14i,_14e];};var _15e = function(_15f,_15g){var _15h = T(function(){var _15i = E(_15g);var _15j = _15i[1];var _15k = _15i[2];var _15l = _15i[3];var _15m = _15i[4];var _15n = _15i[5];var _15o = _13Y(_15j,_15k,_15l,_15m,_15n);var _15p = _15o[1];var _15q = _15o[2];var _15r = _15o[3];var _15s = _15o[4];var _15t = _15o[5];var _15u = _15o[6];var _15v = [1,_15p,_15q,_15r,_15s,_15t,_15u];return _15v;});var _15w = [2,_15h];return [1,_15w,_15x];};var _15x = T(function(){return [2,_15e];});var _15y = T(function(){return _dk(_cR,_SI,_15x);});var _15z = function(_15A,_15B){var _15C = T(function(){var _15D = E(_15B);var _15E = _15D[1];var _15F = _15D[2];var _15G = _15D[3];var _15H = _15D[4];var _15I = _15D[5];var _15J = _15D[6];var _15K = _15D[7];var _15L = [1,_15F,_15H,_15K];var _15M = [1,_15E,_15G,_15I,_15J];var _15N = [1,_15M,_15L];return _15N;});var _15O = [2,_15C];return [1,_15O,_15P];};var _15P = T(function(){return [2,_15z];});var _15Q = T(function(){return _dk(_cR,_15P,_15y);});var _15R = function(_15S,_15T){var _15U = T(function(){var _15V = E(_15T);var _15W = _15V[1];var _15X = _15V[2];var _15Y = E(_15W);var _15Z = _15Y[1];var _160 = _15Y[2];var _161 = T(function(){var _162 = E(_15X);var _163 = _162[3];var _164 = E(_163);return _164;});var _165 = T(function(){var _166 = E(_15X);var _167 = _166[2];var _168 = E(_167);return _168;});var _169 = T(function(){var _16a = E(_15X);var _16b = _16a[1];var _16c = E(_16b);return _16c;});var _16d = [1,_15Z,_160,_169,_165,_161];return _16d;});var _16e = [2,_15U];return [1,_16e,_16f];};var _16f = T(function(){return [2,_15R];});var _16g = T(function(){return _dk(_cR,_15Q,_16f);});var _16h = T(function(){return _kz(_ll,_16g);});var _16i = T(function(){return _i8(_cR,_16h);});var _16j = function(_16k,_16l){var _16m = T(function(){var _16n = E(_16l);var _16o = _16n[1];var _16p = _16n[2];var _16q = E(_16o);var _16r = _16q[1];var _16s = _16q[2];var _16t = _16q[3];var _16u = _16q[4];var _16v = [1,_16p,_16r,_16s,_16t,_16u];return _16v;});var _16w = [2,_16m];return [1,_16w,_16x];};var _16x = T(function(){return [2,_16j];});var _16y = T(function(){return _dk(_cR,_16x,_16i);});var _16z = function(_16A,_16B){var _16C = T(function(){var _16D = E(_16B);var _16E = _16D[1];var _16F = [1,_16D,_16E];return _16F;});var _16G = [2,_16C];return [1,_16G,_16H];};var _16H = T(function(){return [2,_16z];});var _16I = T(function(){return _dk(_cR,_16y,_16H);});var _16J = function(_16K,_16L){var _16M = T(function(){var _16N = E(_16L);var _16O = _16N[1];var _16P = _16N[2];var _16Q = _16N[3];var _16R = _16N[4];var _16S = _16N[5];var _16T = T(function(){return _q0(_gy,_16S);});var _16U = T(function(){return _q0(_gy,_16Q);});var _16V = [1,_16O,_16R,_16P,_16U,_16T];var _16W = [2,_16V];return _16W;});var _16X = [2,_16M];return [1,_16X,_16Y];};var _16Y = T(function(){return [2,_16J];});var _16Z = function(_170,_171){var _172 = [2,_171];return [1,_172,_173];};var _173 = T(function(){return [2,_16Z];});var _174 = T(function(){return _dk(_cR,_173,_16Y);});var _175 = T(function(){return _dk(_cR,_174,_16I);});var _176 = T(function(){return _j2(_cR,_175,_km);});var _177 = [2,_0];var _178 = function(_179,_17a){var _17b = T(function(){var _17c = E(_17a);var _17d = _17c[1];var _17e = _17c[2];var _17f = _17c[3];var _17g = E(_17d);switch(_17g[0]){case 1:var _17h = E(_177);break;case 2:var _17h = E(_177);break;case 3:var _17i = [1,_17e,_17f];var _17h = [1,_17i];break;}return _17h;});var _17j = [2,_17b];return [1,_17j,_17k];};var _17k = T(function(){return [2,_178];});var _17l = T(function(){return _dk(_cR,_176,_17k);});var _17m = T(function(){return _dk(_cR,_17l,_j1);});var _17n = function(_17o,_17p){var _17q = T(function(){var _17r = E(_17p);var _17s = _17r[1];var _17t = _17r[2];var _17u = T(function(){var _17v = E(_17t);var _17w = _17v[1];var _17x = T(function(){var _17y = E(_17w);var _17z = _17y[1];var _17A = _17z+25;var _17B = [1,_17A];return _17B;});var _17C = [1,_17x,_6D];var _17D = [1,_17C];return _17D;});var _17E = function(_17F){var _17G = E(_17s);switch(_17G[0]){case 1:var _17H = E(_17F);break;case 2:var _17I = _17G[1];var _17J = E(_17I);var _17K = _17J[1];var _17L = E(_17K);var _17M = _17L==32?[2,_17u,_17F]:E(_17F);var _17H = _17M;break;case 3:var _17H = [1];break;}return _17H;};var _17N = [1,_17s,_17t,_17E];return _17N;});var _17O = [2,_17q];return [1,_17O,_17P];};var _17P = T(function(){return [2,_17n];});var _17Q = T(function(){return _dk(_cR,_17m,_17P);});var _17R = function(_17S){var _17T = E(_17S);if(_17T[0]==1){var _17U = _17T[1];var _17V = [1,_17U];}else{var _17W = _17T[1];var _17X = [1,_17W];var _17V = [2,_17X];}return _17V;};var _17Y = [1,275];var _17Z = [1,550];var _180 = function(_181,_182,_183){var _184 = E(_183);var _185 = _184[1];var _186 = E(_182);var _187 = _186[1];var _188 = E(_181);var _189 = _188[1];var _18a = E(_185);var _18b = _18a[1];var _18c = _189*_18b;var _18d = _187+_18c;var _18e = 550<=_18d;if(_18e){var _18f = E(_17Z);}else{var _18g = 0<=_18d;var _18f = _18g?[1,_18d]:E(_5k);}return _18f;};var _18h = T(function(){return _lm(_180,_17Y);});var _18i = T(function(){return _qZ(_0);});var _18j = function(_18k,_18l,_18m){var _18n = T(function(){return _q9(_18l);});var _18o = [1,_18n];var _18p = function(_18q,_18r){var _18s = [2,_18r];var _18t = function(_18u){var _18v = E(_18u);var _18w = _18v[1];var _18x = _18v[2];var _18y = T(function(){return _18j(_18k,_18l,_18x);});var _18z = T(function(){var _18A = E(_18w);return _18A[0]==1?E(_18s):E(_18o);});var _18B = [1,_18z,_18y];var _18C = A(_c3,[_18k,_18B]);return _18C;};var _18D = T(function(){var _18E = E(_18m);if(_18E[0]==1){var _18F = _18E[1];var _18G = A(_18F,[_18q,_18r]);}else{var _18H = _18E[1];var _18I = T(function(){return A(_18H,[_18q,_18r]);});var _18G = A(_c3,[_18k,_18I]);}return _18G;});return A(_bY,[_18k,_18D,_18t]);};return [1,_18p];};var _18J = [1,_ph];var _18K = function(_18L){var _18M = T(function(){return [2,_18N];});var _18N = function(_18O,_18P){var _18Q = T(function(){var _18R = E(_18P);switch(_18R[0]){case 1:var _18S = E(_18J);break;case 2:var _18T = _18R[1];var _18U = E(_18T);var _18V = _18U[1];var _18W = E(_18L);var _18X = _18W[1];var _18Y = _18V==_18X;var _18Z = _18Y?[2,_18R]:E(_18J);var _18S = _18Z;break;case 3:var _18S = E(_18J);break;}return _18S;});return [1,_18Q,_18M];};return E(_18M);};var _190 = function(_191){var _192 = T(function(){return [2,_193];});var _193 = function(_194,_195){var _196 = T(function(){var _197 = E(_195);switch(_197[0]){case 1:var _198 = _197[1];var _199 = E(_198);var _19a = _199[1];var _19b = E(_191);var _19c = _19b[1];var _19d = _19a==_19c;var _19e = _19d?[2,_197]:E(_18J);var _19f = _19e;break;case 2:var _19f = E(_18J);break;case 3:var _19f = E(_18J);break;}return _19f;});return [1,_196,_192];};return E(_192);};var _19g = function(_19h,_19i,_19j){var _19k = T(function(){var _19l = T(function(){var _19m = T(function(){var _19n = T(function(){var _19o = T(function(){return _190(_19h);});return _18j(_cR,_qX,_19o);});var _19p = _qZ(_19j);var _19q = _dk(_cR,_19p,_19n);return _19q;});return _cS(_cR,_19m,_19k);});var _19r = T(function(){var _19s = T(function(){var _19t = T(function(){return _18K(_19h);});return _18j(_cR,_qX,_19t);});var _19u = _qZ(_19i);var _19v = _dk(_cR,_19u,_19s);return _19v;});return _cS(_cR,_19r,_19l);});return E(_19k);};var _19w = [1,39];var _19x = T(function(){return _19g(_19w,_5k,_6B);});var _19y = [1,37];var _19z = [1,(-7)];var _19A = T(function(){return _19g(_19y,_5k,_19z);});var _19B = function(_19C,_19D){var _19E = E(_19C);var _19F = _19E[1];var _19G = E(_19D);var _19H = _19G[1];var _19I = _19F+_19H;var _19J = [1,_19I];return _19J;};var _19K = function(_19L){var _19M = E(_19L);if(_19M[0]==1){var _19N = _19M[1];var _19O = [1,_19N];}else{var _19P = _19M[1];var _19Q = function(_lT){return _19B(_19P,_lT);};var _19O = [2,_19Q];}return _19O;};var _19R = T(function(){return _ho(_cR,_19K,_19A);});var _19S = T(function(){return _eK(_cR,_19R,_19x);});var _19T = T(function(){return _ho(_cR,_gq,_19S);});var _19U = T(function(){return _eK(_cR,_19T,_18i);});var _19V = T(function(){return _dk(_cR,_18h,_19U);});var _19W = T(function(){return _ho(_cR,_17R,_19V);});var _19X = function(_19Y,_19Z){var _1a0 = [2,_19Z];return [1,_1a0,_1a1];};var _1a1 = T(function(){return [2,_19X];});var _1a2 = T(function(){return _dk(_cR,_19W,_1a1);});var _1a3 = T(function(){return _i8(_cR,_1a2);});var _1a4 = function(_1a5,_1a6){var _1a7 = T(function(){var _1a8 = E(_1a6);var _1a9 = _1a8[1];var _1aa = _1a8[2];var _1ab = [1,_1aa,_1a9];return _1ab;});var _1ac = [2,_1a7];return [1,_1ac,_1ad];};var _1ad = T(function(){return [2,_1a4];});var _1ae = T(function(){return _dk(_cR,_1ad,_1a3);});var _1af = function(_1ag,_1ah){var _1ai = [1,_1ah,_1ah];var _1aj = [2,_1ai];return [1,_1aj,_1ak];};var _1ak = T(function(){return [2,_1af];});var _1al = T(function(){return _dk(_cR,_1ae,_1ak);});var _1am = T(function(){return _dk(_cR,_17Q,_1al);});var _1an = function(_1ao,_1ap){var _1aq = [2,_1ap];return [1,_1aq,_1ar];};var _1ar = T(function(){return [2,_1an];});var _1as = T(function(){return _dk(_cR,_1am,_1ar);});var _1at = T(function(){return unCStr("Press Enter to start (click canvas to focus)");});var _1au = [1,13];var _1av = T(function(){return _18K(_1au);});var _1aw = T(function(){return _18j(_cR,_qX,_1av);});var _1ax = function(_1ay){var _1az = [2,_1ay];var _1aA = [2,_1az];var _1aB = [2,_1aA];var _1aC = T(function(){return [2,_1aD];});var _1aD = function(_1aE,_1aF){return E(_1aG);};var _1aG = T(function(){return [1,_1aB,_1aC];});return _dk(_cR,_1aC,_1aw);};var _1aH = T(function(){return _1ax(_1at);});var _1aI = T(function(){return _cS(_cR,_1aH,_1as);});var _1aJ = T(function(){return unCStr("Sorry, you loose! Press Enter to restart.");});var _1aK = T(function(){return _1ax(_1aJ);});var _1aL = T(function(){return _cS(_cR,_1aK,_1as);});var _1aM = T(function(){return unCStr("Congratulations, you won! Press Enter to restart.");});var _1aN = T(function(){return _1ax(_1aM);});var _1aO = T(function(){return _cS(_cR,_1aN,_1as);});var _1aP = function(_1aQ){var _1aR = E(_1aQ);switch(_1aR[0]){case 1:var _1aS = E(_1aO);break;case 2:var _1aS = E(_1aL);break;case 3:var _1aS = E(_1aI);break;}return _1aS;};var _1aT = T(function(){return _c8(_cR,_1aP,_1aI);});var _1aU = function(_1aV){var _1aW = nMV(_1aT,_1aV);var _1aX = _1aW[1];var _1aY = _1aW[2];var _1aZ = function(_1b0,_1b1){return _bC(_1aY,_1b0,_1b1);};var _1b2 = A(_18,[_6t,_1aZ,_1aX]);var _1b3 = _1b2[1];var _1b4 = function(_1b5,_1b6){return _bg(_1aY,_1b5,_1b6);};var _1b7 = A(_1C,[_6t,_1b4,_1b3]);var _1b8 = _1b7[1];var _1b9 = function(_1ba){return _aF(_1aY,_1ba);};var _1bb = A(_p,[_5i,_1b9,_1b8]);return _1bb;};var _1bc = T(function(){return A(_a,[_1aU]);});
E(E(_1bc)(0));
