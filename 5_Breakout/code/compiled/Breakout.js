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

function jsFillText(context, text, x, y, _) {
	context.fillText(text,x,y);
	return [1,0];
}

function jsSetFillColor(context, color, _) {
	context.fillStyle = color;
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


var _0 = [1];var _1 = function(_2,_3){var _4 = E(_2);var _5 = [1,_4];var _6 = _5[1];var _7 = jsSetOnLoad(_6,_3);var _8 = _7[1];var _9 = [1,_8,_0];return _9;};var _a = function(_b,_c){return _1(_b,_c);};var _d = function(_e,_f){var _g = E(_e);if(_g[0]==1){var _h = E(_f);}else{var _i = _g[1];var _j = _g[2];var _k = T(function(){return _d(_j,_f);});var _h = [2,_i,_k];}return _h;};var _l = [1];var _m = function(_n,_o,_p){var _q = E(_n);var _r = _q[1];var _s = E(_o);var _t = [1,_s];var _u = _t[1];var _v = jsSetInterval(_r,_u,_p);var _w = _v[1];var _x = [1,_w,_0];return _x;};var _y = function(_z,_b,_c){return _m(_z,_b,_c);};var _A = [15,coercionToken];var _B = "load";var _C = [1,_B];var _D = "mousemove";var _E = [1,_D];var _F = "mouseover";var _G = [1,_F];var _H = "mouseout";var _I = [1,_H];var _J = "click";var _K = [1,_J];var _L = "dblclick";var _M = [1,_L];var _N = "mousedown";var _O = [1,_N];var _P = "mouseup";var _Q = [1,_P];var _R = "keypress";var _S = [1,_R];var _T = "keyup";var _U = [1,_T];var _V = "keydown";var _W = [1,_V];var _X = "unload";var _Y = [1,_X];var _Z = "change";var _10 = [1,_Z];var _11 = "focus";var _12 = [1,_11];var _13 = "blur";var _14 = [1,_13];var _15 = function(_16,_17,_18,_19){var _1a = [1,_18];var _1b = _1a[1];var _1c = function(_1d){var _1e = E(_18);var _1f = jsSetCB(_16,_1d,_1b,_19);var _1g = _1f[1];var _1h = _1f[2];var _1i = T(function(){var _1j = E(_1h);return _1j?true:false;});var _1k = [1,_1g,_1i];return _1k;};var _1l = E(_17);switch(_1l[0]){case 1:var _1m = E(_C);var _1n = _1m[1];var _1o = _1c(_1n);var _1p = _1o;break;case 2:var _1q = E(_Y);var _1r = _1q[1];var _1s = _1c(_1r);var _1p = _1s;break;case 3:var _1t = E(_10);var _1u = _1t[1];var _1v = _1c(_1u);var _1p = _1v;break;case 4:var _1w = E(_12);var _1x = _1w[1];var _1y = _1c(_1x);var _1p = _1y;break;case 5:var _1z = E(_14);var _1A = _1z[1];var _1B = _1c(_1A);var _1p = _1B;break;case 6:var _1C = E(_E);var _1D = _1C[1];var _1E = _1c(_1D);var _1p = _1E;break;case 7:var _1F = E(_G);var _1G = _1F[1];var _1H = _1c(_1G);var _1p = _1H;break;case 8:var _1I = E(_I);var _1J = _1I[1];var _1K = _1c(_1J);var _1p = _1K;break;case 9:var _1L = E(_K);var _1M = _1L[1];var _1N = _1c(_1M);var _1p = _1N;break;case 10:var _1O = E(_M);var _1P = _1O[1];var _1Q = _1c(_1P);var _1p = _1Q;break;case 11:var _1R = E(_O);var _1S = _1R[1];var _1T = _1c(_1S);var _1p = _1T;break;case 12:var _1U = E(_Q);var _1V = _1U[1];var _1W = _1c(_1V);var _1p = _1W;break;case 13:var _1X = E(_S);var _1Y = _1X[1];var _1Z = _1c(_1Y);var _1p = _1Z;break;case 14:var _20 = E(_U);var _21 = _20[1];var _22 = _1c(_21);var _1p = _22;break;case 15:var _23 = E(_W);var _24 = _23[1];var _25 = _1c(_24);var _1p = _25;break;}return _1p;};var _26 = T(function(){return unCStr(" could be found!");});var _27 = function(_28){var _29 = T(function(){return _d(_28,_26);});var _2a = unAppCStr("No element with ID ",_29);var _2b = err(_2a);return _2b;};var _2c = function(_2d,_2e,_2f){var _2g = toJSStr(_2d);var _2h = _2g[1];var _2i = jsFind(_2h,_2f);var _2j = _2i[1];var _2k = _2i[2];var _2l = [1,_2k];var _2m = _2l[1];var _2n = E(_2m);if(_2n[0]==1){var _2o = _27(_2d);}else{var _2p = _2n[1];var _2q = E(_2p);var _2r = _2q[1];var _2s = _15(_2r,_A,_2e,_2j);var _2o = _2s;}return _2o;};var _2t = function(_z,_b,_c){return _2c(_z,_b,_c);};var _2u = [14,coercionToken];var _2v = function(_2w,_2x,_2y){var _2z = toJSStr(_2w);var _2A = _2z[1];var _2B = jsFind(_2A,_2y);var _2C = _2B[1];var _2D = _2B[2];var _2E = [1,_2D];var _2F = _2E[1];var _2G = E(_2F);if(_2G[0]==1){var _2H = _27(_2w);}else{var _2I = _2G[1];var _2J = E(_2I);var _2K = _2J[1];var _2L = _15(_2K,_2u,_2x,_2C);var _2H = _2L;}return _2H;};var _2M = function(_z,_b,_c){return _2v(_z,_b,_c);};var _2N = [1,20];var _2O = T(function(){return unCStr("canvas3");});var _2P = function(_2Q,_2R){var _2S = E(_2Q);var _2T = _2S[1];var _2U = jsClear(_2T,_2R);var _2V = _2U[1];var _2W = [1,_2V,_0];return _2W;};var _2X = function(_b,_c){return _2P(_b,_c);};var _2Y = function(_2Z,_30,_31,_32,_33){var _34 = jsBeginPath(_2Z,_33);var _35 = _34[1];var _36 = jsArc(_2Z,_30,_31,_32,0,6.283185307179586,1,_35);var _37 = _36[1];var _38 = jsClosePath(_2Z,_37);var _39 = _38[1];var _3a = jsFill(_2Z,_39);var _3b = _3a[1];var _3c = [1,_3b,_0];return _3c;};var _3d = function(_3e,_3f,_3g,_3h,_3i){var _3j = E(_3e);var _3k = _3j[1];var _3l = E(_3f);var _3m = _3l[1];var _3n = E(_3g);var _3o = _3n[1];var _3p = E(_3h);var _3q = _3p[1];var _3r = _2Y(_3k,_3m,_3o,_3q,_3i);return _3r;};var _3s = function(_3t,_3u,_z,_b,_c){return _3d(_3t,_3u,_z,_b,_c);};var _3v = function(_3w,_3x,_3y,_3z,_3A,_3B){var _3C = E(_3w);var _3D = _3C[1];var _3E = E(_3x);var _3F = _3E[1];var _3G = E(_3y);var _3H = _3G[1];var _3I = E(_3z);var _3J = _3I[1];var _3K = E(_3A);var _3L = _3K[1];var _3M = jsFillRect(_3D,_3F,_3H,_3J,_3L,_3B);var _3N = _3M[1];var _3O = [1,_3N,_0];return _3O;};var _3P = function(_3Q,_3t,_3u,_z,_b,_c){return _3v(_3Q,_3t,_3u,_z,_b,_c);};var _3R = function(_3S,_3T,_3U,_3V,_3W){var _3X = E(_3S);var _3Y = _3X[1];var _3Z = E(_3T);var _40 = _3Z[1];var _41 = E(_3U);var _42 = _41[1];var _43 = E(_3V);var _44 = _43[1];var _45 = jsFillText(_3Y,_40,_42,_44,_3W);var _46 = _45[1];var _47 = [1,_46,_0];return _47;};var _48 = function(_3t,_3u,_z,_b,_c){return _3R(_3t,_3u,_z,_b,_c);};var _49 = function(_4a,_4b){var _4c = T(function(){return toJSStr(_4b);});return A(_48,[_4a,_4c]);};var _4d = function(_4e,_4f){var _4g = toJSStr(_4e);var _4h = _4g[1];var _4i = jsFind(_4h,_4f);var _4j = _4i[1];var _4k = _4i[2];var _4l = [1,_4k];var _4m = _4l[1];var _4n = E(_4m);if(_4n[0]==1){var _4o = _27(_4e);}else{var _4p = _4n[1];var _4q = E(_4p);var _4r = _4q[1];var _4s = jsGetContext2D(_4r,_4j);var _4t = _4s[1];var _4u = _4s[2];var _4v = [1,_4u];var _4w = [1,_4t,_4v];var _4o = _4w;}return _4o;};var _4x = function(_b,_c){return _4d(_b,_c);};var _4y = function(_4z,_4A,_4B){var _4C = E(_4z);var _4D = _4C[1];var _4E = toJSStr(_4A);var _4F = _4E[1];var _4G = jsSetFillColor(_4D,_4F,_4B);var _4H = _4G[1];var _4I = [1,_4H,_0];return _4I;};var _4J = function(_z,_b,_c){return _4y(_z,_b,_c);};var _4K = T(function(){return unCStr("red");});var _4L = [1,5];var _4M = T(function(){return unCStr("blue");});var _4N = T(function(){return unCStr("darkblue");});var _4O = [1,60];var _4P = function(_4Q,_4R,_4S){var _4T = T(function(){var _4U = E(_4R);var _4V = _4U[2];var _4W = E(_4V);var _4X = _4W[1];var _4Y = E(_4X);var _4Z = _4Y==1?E(_4M):E(_4N);return _4Z;});var _50 = A(_4J,[_4Q,_4T,_4S]);var _51 = _50[1];var _52 = T(function(){var _53 = E(_4R);var _54 = _53[1];var _55 = E(_54);var _56 = _55[1];var _57 = _55[2];var _58 = [1,_56,_57,_4O,_2N];return _58;});var _59 = T(function(){var _5a = E(_52);var _5b = _5a[4];var _5c = E(_5b);return _5c;});var _5d = T(function(){var _5e = E(_52);var _5f = _5e[3];var _5g = E(_5f);return _5g;});var _5h = T(function(){var _5i = E(_52);var _5j = _5i[2];var _5k = E(_5j);return _5k;});var _5l = T(function(){var _5m = E(_52);var _5n = _5m[1];var _5o = E(_5n);return _5o;});var _5p = A(_3P,[_4Q,_5l,_5h,_5d,_59,_51]);return _5p;};var _5q = T(function(){return A(_4x,[_2O]);});var _5r = T(function(){return unCStr("Press enter to start --- (click the canvas for input focus)");});var _5s = [1,200];var _5t = T(function(){return unCStr("black");});var _5u = [1,15];var _5v = [1,40];var _5w = [1,385];var _5x = function(_5y,_5z){var _5A = E(_5y);if(_5A[0]==1){var _5B = _5A[1];var _5C = _5A[2];var _5D = _5A[3];var _5E = A(_5q,[_5z]);var _5F = _5E[1];var _5G = _5E[2];var _5H = A(_2X,[_5G,_5F]);var _5I = _5H[1];var _5J = A(_4J,[_5G,_5t,_5I]);var _5K = _5J[1];var _5L = T(function(){var _5M = E(_5B);var _5N = _5M[1];var _5O = [1,_5N,_5w,_5v,_5u];return _5O;});var _5P = T(function(){var _5Q = E(_5L);var _5R = _5Q[4];var _5S = E(_5R);return _5S;});var _5T = T(function(){var _5U = E(_5L);var _5V = _5U[3];var _5W = E(_5V);return _5W;});var _5X = T(function(){var _5Y = E(_5L);var _5Z = _5Y[2];var _60 = E(_5Z);return _60;});var _61 = T(function(){var _62 = E(_5L);var _63 = _62[1];var _64 = E(_63);return _64;});var _65 = A(_3P,[_5G,_61,_5X,_5T,_5P,_5K]);var _66 = _65[1];var _67 = function(_68,_69){while(1){var _6a = E(_68);if(_6a[0]==1){var _6b = [1,_69,_0];}else{var _6c = _6a[1];var _6d = _6a[2];var _6e = _4P(_5G,_6c,_69);var _6f = _6e[1];_68=_6d;_69=_6f;continue;var _6g = die("Unreachable!");var _6b = _6g;}return _6b;}};var _6h = _67(_5D,_66);var _6i = _6h[1];var _6j = A(_4J,[_5G,_4K,_6i]);var _6k = _6j[1];var _6l = T(function(){var _6m = E(_5C);var _6n = _6m[1];var _6o = E(_6n);return _6o;});var _6p = T(function(){var _6q = E(_6l);var _6r = _6q[2];var _6s = E(_6r);return _6s;});var _6t = T(function(){var _6u = E(_6l);var _6v = _6u[1];var _6w = E(_6v);return _6w;});var _6x = A(_3s,[_5G,_6t,_6p,_4L,_6k]);var _6y = _6x;}else{var _6z = A(_4x,[_2O,_5z]);var _6A = _6z[1];var _6B = _6z[2];var _6C = A(_2X,[_6B,_6A]);var _6D = _6C[1];var _6E = A(_4J,[_6B,_5t,_6D]);var _6F = _6E[1];var _6G = A(_49,[_6B,_5r,_5s,_5s,_6F]);var _6y = _6G;}return _6y;};var _6H = function(_6I,_6J,_6K){var _6L = T(function(){return A(_6J,[_6K]);});var _6M = T(function(){var _6N = T(function(){var _6O = E(_6L);var _6P = _6O[1];var _6Q = E(_6P);return _6Q;});return A(_6I,[_6N]);});var _6R = T(function(){var _6S = E(_6L);var _6T = _6S[2];var _6U = E(_6T);return _6U;});var _6V = T(function(){var _6W = E(_6M);var _6X = _6W[2];var _6Y = E(_6X);return _6Y;});var _6Z = function(_70){var _71 = _6H(_6V,_6R,_70);var _72 = _71[1];var _73 = _71[2];var _74 = [1,_72,_73];return _74;};var _75 = T(function(){var _76 = E(_6M);var _77 = _76[1];var _78 = E(_77);return _78;});return [1,_75,_6Z];};var _79 = function(_7a,_7b,_7c){var _7d = _6H(_7a,_7b,_7c);var _7e = _7d[1];var _7f = _7d[2];var _7g = [1,_7e,_7f];return _7g;};var _7h = function(_7i){var _7j = T(function(){return _7h(_7i);});var _7k = function(_7l){var _7m = T(function(){return A(_7i,[_7l]);});return [1,_7m,_7j];};return E(_7k);};var _7n = function(_7o){return E(_7o);};var _7p = T(function(){return _7h(_7n);});var _7q = function(_7r){return E(_7r);};var _7s = T(function(){return _7h(_7q);});var _7t = function(_7u){return _79(_7s,_7p,_7u);};var _7v = function(_7w,_7x,_7y){var _7z = T(function(){return A(_7w,[_7x]);});var _7A = T(function(){var _7B = E(_7z);var _7C = _7B[2];var _7D = E(_7C);return _7D;});var _7E = function(_7F){var _7G = E(_7F);var _7H = _7G[1];var _7I = _7G[2];var _7J = _7v(_7A,_7H,_7I);var _7K = _7J[1];var _7L = _7J[2];var _7M = [1,_7K,_7L];return _7M;};var _7N = T(function(){var _7O = E(_7z);var _7P = _7O[1];var _7Q = E(_7P);return _7Q;});var _7R = [1,_7N,_7y];return [1,_7R,_7E];};var _7S = function(_7T,_7U){var _7V = T(function(){var _7W = T(function(){var _7X = E(_7V);var _7Y = _7X[2];var _7Z = E(_7Y);return _7Z;});var _80 = [1,_7U,_7W];var _81 = A(_7T,[_80]);var _82 = _81[1];var _83 = _81[2];var _84 = E(_82);var _85 = _84[1];var _86 = _84[2];var _87 = [1,_85,_86,_83];return _87;});var _88 = T(function(){var _89 = E(_7V);var _8a = _89[3];var _8b = E(_8a);return _8b;});var _8c = function(_8d){var _8e = _7S(_88,_8d);var _8f = _8e[1];var _8g = _8e[2];var _8h = [1,_8f,_8g];return _8h;};var _8i = T(function(){var _8j = E(_7V);var _8k = _8j[1];var _8l = E(_8k);return _8l;});return [1,_8i,_8c];};var _8m = function(_8n,_8o){while(1){var _8p = E(_8o);if(_8p[0]==1){var _8q = E(_8n);}else{var _8r = _8p[1];var _8s = _8p[2];_8n=_8r;_8o=_8s;continue;var _8q = die("Unreachable!");}return _8q;}};var _8t = function(_8u,_8v,_8w){var _8x = T(function(){return A(_8m,[_8u,_8v,_8w]);});var _8y = T(function(){var _8z = E(_8x);var _8A = _8z[2];var _8B = E(_8A);return _8B;});var _8C = function(_8D){var _8E = E(_8D);var _8F = _8E[1];var _8G = _8E[2];var _8H = _8t(_8y,_8F,_8G);var _8I = _8H[1];var _8J = _8H[2];var _8K = [1,_8I,_8J];return _8K;};var _8L = T(function(){var _8M = E(_8x);var _8N = _8M[1];var _8O = E(_8N);return _8O;});return [1,_8L,_8C];};var _8P = function(_8Q,_8R){var _8S = E(_8R);var _8T = _8S[1];var _8U = _8S[2];var _8V = _8t(_8Q,_8T,_8U);var _8W = _8V[1];var _8X = _8V[2];var _8Y = [1,_8W,_8X];return _8Y;};var _8Z = function(_90){return [2];};var _91 = T(function(){return _7h(_8Z);});var _92 = function(_93){return _8P(_91,_93);};var _94 = function(_95){var _96 = E(_95);var _97 = _96[1];var _98 = _96[2];var _99 = _96[3];var _9a = T(function(){return _d(_98,_99);});var _9b = [1,_9a,_97];return _9b;};var _9c = T(function(){return _7h(_94);});var _9d = function(_9e){var _9f = _6H(_92,_9c,_9e);var _9g = _9f[1];var _9h = _9f[2];var _9i = [1,_9g,_9h];return _9i;};var _9j = function(_9k){var _9l = E(_9k);var _9m = _9l[1];var _9n = _9l[2];var _9o = _7v(_9d,_9m,_9n);var _9p = _9o[1];var _9q = _9o[2];var _9r = [1,_9p,_9q];return _9r;};var _9s = function(_9t){var _9u = E(_9t);var _9v = _9u[1];var _9w = _9u[2];var _9x = E(_9w);var _9y = E(_9v);return _9y;};var _9z = T(function(){return _7h(_9s);});var _9A = function(_9B){var _9C = _6H(_9z,_9j,_9B);var _9D = _9C[1];var _9E = _9C[2];var _9F = [1,_9D,_9E];return _9F;};var _9G = function(_9H){var _9I = E(_9H);var _9J = [1,_9I,_0];return _9J;};var _9K = T(function(){return _7h(_9G);});var _9L = function(_9M){var _9N = _6H(_9A,_9K,_9M);var _9O = _9N[1];var _9P = _9N[2];var _9Q = [1,_9O,_9P];return _9Q;};var _9R = function(_9S){var _9T = E(_9S);var _9U = _9T[2];var _9V = E(_9U);return _9V;};var _9W = T(function(){return _7h(_9R);});var _9X = function(_9Y,_9Z){var _a0 = T(function(){return A(_a1,[_9Z]);});var _a2 = [1,_9Z,_9Y];return [1,_a2,_a0];};var _a1 = function(_a3,_a4){var _a5 = _9X(_a3,_a4);var _a6 = _a5[1];var _a7 = _a5[2];var _a8 = [1,_a6,_a7];return _a8;};var _a9 = [2];var _aa = function(_ab){var _ac = T(function(){return A(_a1,[_ab]);});var _ad = [1,_ab,_a9];return [1,_ad,_ac];};var _ae = function(_af){var _ag = _6H(_9W,_aa,_af);var _ah = _ag[1];var _ai = _ag[2];var _aj = [1,_ah,_ai];return _aj;};var _ak = T(function(){return _7h(_7n);});var _al = function(_am){var _an = _6H(_ae,_ak,_am);var _ao = _an[1];var _ap = _an[2];var _aq = [1,_ao,_ap];return _aq;};var _ar = function(_as){var _at = E(_as);var _au = _at[1];var _av = _at[2];var _aw = _7v(_al,_au,_av);var _ax = _aw[1];var _ay = _aw[2];var _az = [1,_ax,_ay];return _az;};var _aA = function(_aB){var _aC = E(_aB);var _aD = _aC[1];var _aE = _aC[2];var _aF = [1,_aE,_aD];return _aF;};var _aG = T(function(){return _7h(_aA);});var _aH = function(_aI){var _aJ = _6H(_aG,_ar,_aI);var _aK = _aJ[1];var _aL = _aJ[2];var _aM = [1,_aK,_aL];return _aM;};var _aN = function(_aO){return [1,_aO,_aO];};var _aP = T(function(){return _7h(_aN);});var _aQ = function(_aR){var _aS = _6H(_aH,_aP,_aR);var _aT = _aS[1];var _aU = _aS[2];var _aV = [1,_aT,_aU];return _aV;};var _aW = function(_aX){var _aY = _6H(_aQ,_9L,_aX);var _aZ = _aY[1];var _b0 = _aY[2];var _b1 = [1,_aZ,_b0];return _b1;};var _b2 = [2,_91,_l];var _b3 = function(_b4,_b5,_b6){var _b7 = _b8(_b4,_b5,_b6);var _b9 = _b7[1];var _ba = _b7[2];var _bb = [1,_b9,_ba];return _bb;};var _b8 = function(_bc,_bd,_be){var _bf = T(function(){return A(_bc,[_bd,_be]);});var _bg = T(function(){return A(_b3,[_bc,_bf]);});return [1,_bf,_bg];};var _bh = [1,280];var _bi = [1,560];var _bj = [1,0];var _bk = function(_bl,_bm){var _bn = E(_bl);var _bo = _bn[1];var _bp = E(_bm);var _bq = _bp[1];var _br = _bo+_bq;var _bs = 0<=_br;if(_bs){var _bt = 560<=_br;var _bu = _bt?E(_bi):[1,_br];}else{var _bu = E(_bj);}return _bu;};var _bv = function(_bw){var _bx = _b8(_bk,_bh,_bw);var _by = _bx[1];var _bz = _bx[2];var _bA = [1,_by,_bz];return _bA;};var _bB = function(_bC){return E(_bC);};var _bD = T(function(){return _7h(_bB);});var _bE = function(_bF){var _bG = _6H(_bv,_bD,_bF);var _bH = _bG[1];var _bI = _bG[2];var _bJ = [1,_bH,_bI];return _bJ;};var _bK = function(_bL){var _bM = E(_bL);var _bN = _bM[1];var _bO = _bM[2];var _bP = _7v(_bE,_bN,_bO);var _bQ = _bP[1];var _bR = _bP[2];var _bS = [1,_bQ,_bR];return _bS;};var _bT = function(_bU){var _bV = E(_bU);var _bW = _bV[1];var _bX = _bV[2];var _bY = E(_bX);var _bZ = E(_bW);return _bZ;};var _c0 = T(function(){return _7h(_bT);});var _c1 = function(_c2){var _c3 = _6H(_c0,_bK,_c2);var _c4 = _c3[1];var _c5 = _c3[2];var _c6 = [1,_c4,_c5];return _c6;};var _c7 = function(_c8){return [1,_c8,_0];};var _c9 = T(function(){return _7h(_c7);});var _ca = function(_cb){var _cc = _6H(_c1,_c9,_cb);var _cd = _cc[1];var _ce = _cc[2];var _cf = [1,_cd,_ce];return _cf;};var _cg = function(_93){return [1,_93];};var _ch = T(function(){return _7h(_cg);});var _ci = T(function(){return _7h(_7q);});var _cj = function(_ck){return _79(_ci,_ch,_ck);};var _cl = function(_cm){var _cn = _6H(_cj,_ca,_cm);var _co = _cn[1];var _cp = _cn[2];var _cq = [1,_co,_cp];return _cq;};var _cr = function(_cs,_ct,_cu){var _cv = _cw(_cs,_ct,_cu);var _cx = _cv[1];var _cy = _cv[2];var _cz = [1,_cx,_cy];return _cz;};var _cA = function(_cB,_cC,_cD){var _cE = function(_cF,_cG){while(1){var _cH = E(_cG);if(_cH[0]==1){var _cI = E(_cF);}else{var _cJ = _cH[1];var _cK = _cH[2];var _cL = A(_cB,[_cF,_cJ]);_cF=_cL;_cG=_cK;continue;var _cM = die("Unreachable!");var _cI = _cM;}return _cI;}};return _cE(_cC,_cD);};var _cw = function(_cN,_cO,_cP){var _cQ = T(function(){return _cA(_cN,_cO,_cP);});var _cR = T(function(){return A(_cr,[_cN,_cQ]);});return [1,_cQ,_cR];};var _cS = false;var _cT = function(_cU,_cV){var _cW = E(_cV);if(_cW[0]==1){var _cX = _cW[1];var _cY = E(_cX);var _cZ = _cY[1];var _d0 = E(_cZ);var _d1 = _d0==37?false:E(_cU);var _d2 = _d1;}else{var _d3 = _cW[1];var _d4 = E(_d3);var _d5 = _d4[1];var _d6 = E(_d5);var _d7 = _d6==37?true:E(_cU);var _d2 = _d7;}return _d2;};var _d8 = function(_d9){var _da = _cw(_cT,_cS,_d9);var _db = _da[1];var _dc = _da[2];var _dd = [1,_db,_dc];return _dd;};var _de = function(_df){return E(_df);};var _dg = T(function(){return _7h(_de);});var _dh = function(_di){var _dj = _6H(_d8,_dg,_di);var _dk = _dj[1];var _dl = _dj[2];var _dm = [1,_dk,_dl];return _dm;};var _dn = function(_do){var _dp = E(_do);var _dq = _dp[1];var _dr = _dp[2];var _ds = _7v(_dh,_dq,_dr);var _dt = _ds[1];var _du = _ds[2];var _dv = [1,_dt,_du];return _dv;};var _dw = function(_dx){var _dy = E(_dx);var _dz = _dy[1];var _dA = _dy[2];var _dB = [1,_dA,_dz];return _dB;};var _dC = T(function(){return _7h(_dw);});var _dD = function(_dE){var _dF = _6H(_dC,_dn,_dE);var _dG = _dF[1];var _dH = _dF[2];var _dI = [1,_dG,_dH];return _dI;};var _dJ = function(_dK){return [1,_dK,_dK];};var _dL = T(function(){return _7h(_dJ);});var _dM = function(_dN){var _dO = _6H(_dD,_dL,_dN);var _dP = _dO[1];var _dQ = _dO[2];var _dR = [1,_dP,_dQ];return _dR;};var _dS = function(_dT,_dU){var _dV = E(_dU);if(_dV[0]==1){var _dW = _dV[1];var _dX = E(_dW);var _dY = _dX[1];var _dZ = E(_dY);var _e0 = _dZ==39?false:E(_dT);var _e1 = _e0;}else{var _e2 = _dV[1];var _e3 = E(_e2);var _e4 = _e3[1];var _e5 = E(_e4);var _e6 = _e5==39?true:E(_dT);var _e1 = _e6;}return _e1;};var _e7 = function(_e8){var _e9 = _cw(_dS,_cS,_e8);var _ea = _e9[1];var _eb = _e9[2];var _ec = [1,_ea,_eb];return _ec;};var _ed = T(function(){return _7h(_de);});var _ee = function(_ef){var _eg = _6H(_e7,_ed,_ef);var _eh = _eg[1];var _ei = _eg[2];var _ej = [1,_eh,_ei];return _ej;};var _ek = function(_el){var _em = E(_el);var _en = _em[1];var _eo = _em[2];var _ep = _7v(_ee,_en,_eo);var _eq = _ep[1];var _er = _ep[2];var _es = [1,_eq,_er];return _es;};var _et = function(_eu){var _ev = E(_eu);var _ew = _ev[1];var _ex = _ev[2];var _ey = [1,_ex,_ew];return _ey;};var _ez = T(function(){return _7h(_et);});var _eA = function(_eB){var _eC = _6H(_ez,_ek,_eB);var _eD = _eC[1];var _eE = _eC[2];var _eF = [1,_eD,_eE];return _eF;};var _eG = function(_eH){return E(_eH);};var _eI = T(function(){return _7h(_eG);});var _eJ = function(_eK){var _eL = _6H(_eA,_eI,_eK);var _eM = _eL[1];var _eN = _eL[2];var _eO = [1,_eM,_eN];return _eO;};var _eP = [1,(-5)];var _eQ = function(_eR){var _eS = E(_eR);var _eT = _eS[1];var _eU = _eS[2];var _eV = E(_eT);if(_eV){var _eW = E(_eP);}else{var _eX = E(_eU);var _eW = _eX?E(_4L):E(_bj);}return _eW;};var _eY = T(function(){return _7h(_eQ);});var _eZ = T(function(){return _7h(_7q);});var _f0 = function(_f1){return _79(_eZ,_eY,_f1);};var _f2 = function(_f3){var _f4 = _6H(_f0,_eJ,_f3);var _f5 = _f4[1];var _f6 = _f4[2];var _f7 = [1,_f5,_f6];return _f7;};var _f8 = function(_f9){var _fa = _6H(_f2,_dM,_f9);var _fb = _fa[1];var _fc = _fa[2];var _fd = [1,_fb,_fc];return _fd;};var _fe = T(function(){return _7h(_de);});var _ff = function(_fg){var _fh = _6H(_f8,_fe,_fg);var _fi = _fh[1];var _fj = _fh[2];var _fk = [1,_fi,_fj];return _fk;};var _fl = T(function(){return _7h(_de);});var _fm = function(_fn){var _fo = _6H(_ff,_fl,_fn);var _fp = _fo[1];var _fq = _fo[2];var _fr = [1,_fp,_fq];return _fr;};var _fs = function(_ft){var _fu = E(_ft);var _fv = _fu[1];var _fw = _fu[2];var _fx = _7v(_fm,_fv,_fw);var _fy = _fx[1];var _fz = _fx[2];var _fA = [1,_fy,_fz];return _fA;};var _fB = T(function(){return _7h(_bT);});var _fC = function(_fD){var _fE = _6H(_fB,_fs,_fD);var _fF = _fE[1];var _fG = _fE[2];var _fH = [1,_fF,_fG];return _fH;};var _fI = function(_fJ){return [1,_fJ,_0];};var _fK = T(function(){return _7h(_fI);});var _fL = function(_fM){var _fN = _6H(_fC,_fK,_fM);var _fO = _fN[1];var _fP = _fN[2];var _fQ = [1,_fO,_fP];return _fQ;};var _fR = function(_fS){var _fT = _6H(_cl,_fL,_fS);var _fU = _fT[1];var _fV = _fT[2];var _fW = [1,_fU,_fV];return _fW;};var _fX = T(function(){return _7h(_de);});var _fY = function(_fZ){var _g0 = _6H(_fR,_fX,_fZ);var _g1 = _g0[1];var _g2 = _g0[2];var _g3 = [1,_g1,_g2];return _g3;};var _g4 = T(function(){return _7h(_de);});var _g5 = function(_g6){var _g7 = _6H(_fY,_g4,_g6);var _g8 = _g7[1];var _g9 = _g7[2];var _ga = [1,_g8,_g9];return _ga;};var _gb = function(_gc){var _gd = E(_gc);var _ge = _gd[1];var _gf = _gd[2];var _gg = _7v(_g5,_ge,_gf);var _gh = _gg[1];var _gi = _gg[2];var _gj = [1,_gh,_gi];return _gj;};var _gk = function(_gl){var _gm = E(_gl);var _gn = _gm[1];var _go = _gm[2];var _gp = E(_go);var _gq = E(_gn);return _gq;};var _gr = T(function(){return _7h(_gk);});var _gs = function(_gt){var _gu = _6H(_gr,_gb,_gt);var _gv = _gu[1];var _gw = _gu[2];var _gx = [1,_gv,_gw];return _gx;};var _gy = T(function(){return _7h(_fI);});var _gz = function(_gA){var _gB = _6H(_gs,_gy,_gA);var _gC = _gB[1];var _gD = _gB[2];var _gE = [1,_gC,_gD];return _gE;};var _gF = [1,350];var _gG = [1,300];var _gH = [1,_gG,_gF];var _gI = function(_gJ,_gK){var _gL = E(_gJ);var _gM = _gL[1];var _gN = E(_gK);var _gO = _gN[1];var _gP = _gM+_gO;var _gQ = [1,_gP];return _gQ;};var _gR = function(_gS,_gT){var _gU = E(_gS);var _gV = _gU[1];var _gW = _gU[2];var _gX = E(_gT);var _gY = _gX[1];var _gZ = _gX[2];var _h0 = T(function(){return _gI(_gW,_gZ);});var _h1 = T(function(){return _gI(_gV,_gY);});var _h2 = [1,_h1,_h0];return _h2;};var _h3 = function(_h4){var _h5 = _b8(_gR,_gH,_h4);var _h6 = _h5[1];var _h7 = _h5[2];var _h8 = [1,_h6,_h7];return _h8;};var _h9 = function(_ha){return E(_ha);};var _hb = T(function(){return _7h(_h9);});var _hc = function(_hd){var _he = _6H(_h3,_hb,_hd);var _hf = _he[1];var _hg = _he[2];var _hh = [1,_hf,_hg];return _hh;};var _hi = function(_hj){var _hk = E(_hj);var _hl = _hk[1];var _hm = _hk[2];var _hn = _7v(_hc,_hl,_hm);var _ho = _hn[1];var _hp = _hn[2];var _hq = [1,_ho,_hp];return _hq;};var _hr = function(_hs){var _ht = E(_hs);var _hu = _ht[1];var _hv = _ht[2];var _hw = E(_hv);var _hx = E(_hu);return _hx;};var _hy = T(function(){return _7h(_hr);});var _hz = function(_hA){var _hB = _6H(_hy,_hi,_hA);var _hC = _hB[1];var _hD = _hB[2];var _hE = [1,_hC,_hD];return _hE;};var _hF = function(_hG){return [1,_hG,_0];};var _hH = T(function(){return _7h(_hF);});var _hI = function(_hJ){var _hK = _6H(_hz,_hH,_hJ);var _hL = _hK[1];var _hM = _hK[2];var _hN = [1,_hL,_hM];return _hN;};var _hO = function(_93){return [1,_93];};var _hP = T(function(){return _7h(_hO);});var _hQ = T(function(){return _7h(_7q);});var _hR = function(_hS){return _79(_hQ,_hP,_hS);};var _hT = function(_hU){var _hV = _6H(_hR,_hI,_hU);var _hW = _hV[1];var _hX = _hV[2];var _hY = [1,_hW,_hX];return _hY;};var _hZ = function(_i0){var _i1 = E(_i0);var _i2 = _i1[1];var _i3 = _i2>=0;if(_i3){var _i4 = E(_i1);}else{var _i5 = -_i2;var _i6 = [1,_i5];var _i4 = _i6;}return _i4;};var _i7 = function(_i8,_i9){var _ia = E(_i8);var _ib = _ia[1];var _ic = _ia[2];var _id = E(_i9);switch(_id[0]){case 1:var _ie = T(function(){return _hZ(_ib);});var _if = [1,_ie,_ic];break;case 2:var _ig = T(function(){var _ih = E(_ib);var _ii = _ih[1];var _ij = _ii>=0;if(_ij){var _ik = -_ii;var _il = [1,_ik];var _im = _il;}else{var _in = -_ii;var _io = -_in;var _ip = [1,_io];var _im = _ip;}return _im;});var _if = [1,_ig,_ic];break;case 3:var _iq = T(function(){return _hZ(_ic);});var _if = [1,_ib,_iq];break;case 4:var _ir = T(function(){var _is = E(_ic);var _it = _is[1];var _iu = _it>=0;if(_iu){var _iv = -_it;var _iw = [1,_iv];var _ix = _iw;}else{var _iy = -_it;var _iz = -_iy;var _iA = [1,_iz];var _ix = _iA;}return _ix;});var _if = [1,_ib,_ir];break;}return _if;};var _iB = [1,(-3)];var _iC = [1,3];var _iD = [1,_iC,_iB];var _iE = function(_iF){var _iG = _cw(_i7,_iD,_iF);var _iH = _iG[1];var _iI = _iG[2];var _iJ = [1,_iH,_iI];return _iJ;};var _iK = function(_iL){return E(_iL);};var _iM = T(function(){return _7h(_iK);});var _iN = function(_iO){var _iP = _6H(_iE,_iM,_iO);var _iQ = _iP[1];var _iR = _iP[2];var _iS = [1,_iQ,_iR];return _iS;};var _iT = function(_iU){var _iV = E(_iU);var _iW = _iV[1];var _iX = _iV[2];var _iY = _7v(_iN,_iW,_iX);var _iZ = _iY[1];var _j0 = _iY[2];var _j1 = [1,_iZ,_j0];return _j1;};var _j2 = T(function(){return _7h(_hr);});var _j3 = function(_j4){var _j5 = _6H(_j2,_iT,_j4);var _j6 = _j5[1];var _j7 = _j5[2];var _j8 = [1,_j6,_j7];return _j8;};var _j9 = function(_ja){return [1,_ja,_0];};var _jb = T(function(){return _7h(_j9);});var _jc = function(_jd){var _je = _6H(_j3,_jb,_jd);var _jf = _je[1];var _jg = _je[2];var _jh = [1,_jf,_jg];return _jh;};var _ji = function(_jj){var _jk = _6H(_hT,_jc,_jj);var _jl = _jk[1];var _jm = _jk[2];var _jn = [1,_jl,_jm];return _jn;};var _jo = T(function(){return _7h(_iK);});var _jp = function(_jq){var _jr = _6H(_ji,_jo,_jq);var _js = _jr[1];var _jt = _jr[2];var _ju = [1,_js,_jt];return _ju;};var _jv = T(function(){return _7h(_iK);});var _jw = function(_jx){var _jy = _6H(_jp,_jv,_jx);var _jz = _jy[1];var _jA = _jy[2];var _jB = [1,_jz,_jA];return _jB;};var _jC = function(_jD){var _jE = E(_jD);var _jF = _jE[1];var _jG = _jE[2];var _jH = _7v(_jw,_jF,_jG);var _jI = _jH[1];var _jJ = _jH[2];var _jK = [1,_jI,_jJ];return _jK;};var _jL = function(_jM){var _jN = E(_jM);var _jO = _jN[1];var _jP = _jN[2];var _jQ = [1,_jP,_jO];return _jQ;};var _jR = T(function(){return _7h(_jL);});var _jS = function(_jT){var _jU = _6H(_jR,_jC,_jT);var _jV = _jU[1];var _jW = _jU[2];var _jX = [1,_jV,_jW];return _jX;};var _jY = function(_jZ){var _k0 = E(_jZ);var _k1 = _k0[1];var _k2 = _k0[2];var _k3 = [1,_k2,_k1];return _k3;};var _k4 = T(function(){return _7h(_jY);});var _k5 = function(_k6){var _k7 = _6H(_jS,_k4,_k6);var _k8 = _k7[1];var _k9 = _k7[2];var _ka = [1,_k8,_k9];return _ka;};var _kb = [1,_l,_l];var _kc = function(_kd,_ke,_kf){var _kg = A(_kd,[_ke]);var _kh = _kg[1];var _ki = _kg[2];var _kj = E(_kh);if(_kj[0]==1){var _kk = E(_kf);}else{var _kl = T(function(){var _km = E(_kf);var _kn = _km[2];var _ko = E(_kn);return _ko;});var _kp = [2,_ki,_kl];var _kq = T(function(){var _kr = E(_kf);var _ks = _kr[1];var _kt = E(_ks);return _kt;});var _ku = [2,_kj,_kq];var _kk = [1,_ku,_kp];}return _kk;};var _kv = function(_kw){while(1){var _kx = E(_kw);if(_kx[0]==1){var _ky = [1];}else{var _kz = _kx[1];var _kA = _kx[2];var _kB = E(_kz);if(_kB[0]==1){_kw=_kA;continue;var _kC = die("Unreachable!");}else{var _kD = _kB[1];var _kE = (function(_kA){return T(function(){return _kv(_kA);})})(_kA);var _kC = [2,_kD,_kE];}var _ky = _kC;}return _ky;}};var _kF = function(_kG,_kH,_kI,_kJ){var _kK = E(_kI);if(_kK[0]==1){var _kL = E(_kH);}else{var _kM = _kK[1];var _kN = _kK[2];var _kO = E(_kJ);if(_kO[0]==1){var _kP = E(_kH);}else{var _kQ = _kO[1];var _kR = _kO[2];var _kS = T(function(){return _kF(_kG,_kH,_kN,_kR);});var _kP = A(_kG,[_kM,_kQ,_kS]);}var _kL = _kP;}return _kL;};var _kT = function(_kU,_kV){var _kW = T(function(){return _kF(_kc,_kb,_kU,_kV);});var _kX = T(function(){var _kY = E(_kW);var _kZ = _kY[2];var _l0 = E(_kZ);return _l0;});var _l1 = function(_l2){var _l3 = _kT(_kX,_l2);var _l4 = _l3[1];var _l5 = _l3[2];var _l6 = [1,_l4,_l5];return _l6;};var _l7 = T(function(){var _l8 = E(_kW);var _l9 = _l8[1];var _la = _kv(_l9);return _la;});return [1,_l7,_l1];};var _lb = function(_lc,_ld){var _le = _kT(_lc,_ld);var _lf = _le[1];var _lg = _le[2];var _lh = [1,_lf,_lg];return _lh;};var _li = function(_lj,_lk){var _ll = E(_lk);if(_ll[0]==1){var _lm = [1];}else{var _ln = _ll[1];var _lo = _ll[2];var _lp = T(function(){return _li(_lj,_lo);});var _lq = T(function(){return A(_lj,[_ln]);});var _lm = [2,_lq,_lp];}return _lm;};var _lr = [1,1];var _ls = function(_lt,_lu){var _lv = E(_lt);if(_lv[0]==1){var _lw = [1];}else{var _lx = _lv[1];var _ly = E(_lx);var _lz = _ly[1];var _lA = _ly[2];var _lB = E(_lA);var _lC = _lB[1];var _lD = E(_lC);if(_lD==1){var _lE = [1];}else{var _lF = [1,_lz,_lr];var _lE = [2,_lF];}var _lw = _lE;}return _lw;};var _lG = function(_lH,_lI){var _lJ = [2,_lH];var _lK = _cw(_ls,_lJ,_lI);var _lL = _lK[1];var _lM = _lK[2];var _lN = [1,_lL,_lM];return _lN;};var _lO = [1,140];var _lP = [1,240];var _lQ = [1,520];var _lR = [2,_lQ,_l];var _lS = [1,440];var _lT = [2,_lS,_lR];var _lU = [1,340];var _lV = [2,_lU,_lT];var _lW = [2,_lP,_lV];var _lX = [2,_lO,_lW];var _lY = [2,_2N,_lX];var _lZ = [1,2];var _m0 = [1,260];var _m1 = [1,_m0,_lr];var _m2 = [2,_m1,_l];var _m3 = [1,220];var _m4 = [1,_m3,_lr];var _m5 = [2,_m4,_m2];var _m6 = [1,180];var _m7 = [1,_m6,_lr];var _m8 = [2,_m7,_m5];var _m9 = [1,_lO,_lZ];var _ma = [2,_m9,_m8];var _mb = [1,100];var _mc = [1,_mb,_lr];var _md = [2,_mc,_ma];var _me = function(_mf){var _mg = E(_mf);if(_mg[0]==1){var _mh = [1];}else{var _mi = _mg[1];var _mj = _mg[2];var _mk = T(function(){return _me(_mj);});var _ml = function(_mm,_mn,_mo){var _mp = T(function(){return _mq(_mo);});var _mr = [1,_mi,_mm];var _ms = [1,_mr,_mn];return [2,_ms,_mp];};var _mq = function(_mt){var _mu = E(_mt);if(_mu[0]==1){var _mv = E(_mk);}else{var _mw = _mu[1];var _mx = _mu[2];var _my = E(_mw);var _mz = _my[1];var _mA = _my[2];var _mB = T(function(){return _mq(_mx);});var _mC = [1,_mi,_mz];var _mD = [1,_mC,_mA];var _mE = [2,_mD,_mB];var _mv = _mE;}return _mv;};var _mh = _ml(_4O,_lZ,_md);}return _mh;};var _mF = T(function(){return _me(_lY);});var _mG = T(function(){return _li(_lG,_mF);});var _mH = function(_93){return _lb(_mG,_93);};var _mI = function(_mJ){return E(_mJ);};var _mK = T(function(){return _7h(_mI);});var _mL = function(_mM){var _mN = _6H(_mH,_mK,_mM);var _mO = _mN[1];var _mP = _mN[2];var _mQ = [1,_mO,_mP];return _mQ;};var _mR = function(_mS){var _mT = E(_mS);var _mU = _mT[1];var _mV = _mT[2];var _mW = _7v(_mL,_mU,_mV);var _mX = _mW[1];var _mY = _mW[2];var _mZ = [1,_mX,_mY];return _mZ;};var _n0 = function(_n1){var _n2 = E(_n1);var _n3 = _n2[1];var _n4 = _n2[2];var _n5 = [1,_n4,_n3];return _n5;};var _n6 = T(function(){return _7h(_n0);});var _n7 = function(_n8){var _n9 = _6H(_n6,_mR,_n8);var _na = _n9[1];var _nb = _n9[2];var _nc = [1,_na,_nb];return _nc;};var _nd = function(_ne){return E(_ne);};var _nf = T(function(){return _7h(_nd);});var _ng = function(_nh){var _ni = _6H(_n7,_nf,_nh);var _nj = _ni[1];var _nk = _ni[2];var _nl = [1,_nj,_nk];return _nl;};var _nm = [1,_gH];var _nn = function(_no){var _np = T(function(){return A(_a1,[_no]);});var _nq = [1,_no,_nm];return [1,_nq,_np];};var _nr = function(_ns){var _nt = _6H(_9W,_nn,_ns);var _nu = _nt[1];var _nv = _nt[2];var _nw = [1,_nu,_nv];return _nw;};var _nx = function(_ny){return E(_ny);};var _nz = T(function(){return _7h(_nx);});var _nA = function(_nB){var _nC = _6H(_nr,_nz,_nB);var _nD = _nC[1];var _nE = _nC[2];var _nF = [1,_nD,_nE];return _nF;};var _nG = function(_nH){var _nI = E(_nH);var _nJ = _nI[1];var _nK = _nI[2];var _nL = _7v(_nA,_nJ,_nK);var _nM = _nL[1];var _nN = _nL[2];var _nO = [1,_nM,_nN];return _nO;};var _nP = function(_nQ){var _nR = E(_nQ);var _nS = _nR[1];var _nT = _nR[2];var _nU = E(_nT);var _nV = _nU[1];var _nW = _nU[2];var _nX = [1,_nV,_nW,_nS];return _nX;};var _nY = T(function(){return _7h(_nP);});var _nZ = function(_o0){var _o1 = _6H(_nY,_nG,_o0);var _o2 = _o1[1];var _o3 = _o1[2];var _o4 = [1,_o2,_o3];return _o4;};var _o5 = function(_o6){var _o7 = E(_o6);var _o8 = _o7[1];var _o9 = [1,_o8,_o7];return _o9;};var _oa = T(function(){return _7h(_o5);});var _ob = function(_oc){var _od = _6H(_nZ,_oa,_oc);var _oe = _od[1];var _of = _od[2];var _og = [1,_oe,_of];return _og;};var _oh = function(_oi,_oj){var _ok = T(function(){return A(_a1,[_oj]);});var _ol = [1,_oj,_oi];return [1,_ol,_ok];};var _om = function(_on,_oo){var _op = T(function(){return A(_oh,[_on]);});var _oq = _6H(_9W,_op,_oo);var _or = _oq[1];var _os = _oq[2];var _ot = [1,_or,_os];return _ot;};var _ou = function(_93){return _om(_mF,_93);};var _ov = function(_ow){return E(_ow);};var _ox = T(function(){return _7h(_ov);});var _oy = function(_oz){var _oA = _6H(_ou,_ox,_oz);var _oB = _oA[1];var _oC = _oA[2];var _oD = [1,_oB,_oC];return _oD;};var _oE = function(_oF){var _oG = E(_oF);var _oH = _oG[1];var _oI = _oG[2];var _oJ = _7v(_oy,_oH,_oI);var _oK = _oJ[1];var _oL = _oJ[2];var _oM = [1,_oK,_oL];return _oM;};var _oN = function(_oO){var _oP = E(_oO);var _oQ = _oP[1];var _oR = _oP[2];var _oS = E(_oR);var _oT = _oS[1];var _oU = _oS[2];var _oV = _oS[3];var _oW = [1,_oT,_oU,_oV,_oQ];return _oW;};var _oX = T(function(){return _7h(_oN);});var _oY = function(_oZ){var _p0 = _6H(_oX,_oE,_oZ);var _p1 = _p0[1];var _p2 = _p0[2];var _p3 = [1,_p1,_p2];return _p3;};var _p4 = function(_p5){var _p6 = E(_p5);var _p7 = _p6[2];var _p8 = [1,_p7,_p6];return _p8;};var _p9 = T(function(){return _7h(_p4);});var _pa = function(_pb){var _pc = _6H(_oY,_p9,_pb);var _pd = _pc[1];var _pe = _pc[2];var _pf = [1,_pd,_pe];return _pf;};var _pg = function(_ph){var _pi = _6H(_pa,_ob,_ph);var _pj = _pi[1];var _pk = _pi[2];var _pl = [1,_pj,_pk];return _pl;};var _pm = function(_pn){var _po = _6H(_pg,_ng,_pn);var _pp = _po[1];var _pq = _po[2];var _pr = [1,_pp,_pq];return _pr;};var _ps = function(_pt){var _pu = _6H(_pm,_k5,_pt);var _pv = _pu[1];var _pw = _pu[2];var _px = [1,_pv,_pw];return _px;};var _py = [4];var _pz = [1];var _pA = [2];var _pB = [3];var _pC = [2,_pB,_l];var _pD = function(_pE,_pF,_pG){var _pH = _pF-5;var _pI = _pH+10;var _pJ = _pE>=_pI;if(_pJ){var _pK = [1];}else{var _pL = _pE+40;var _pM = _pH>=_pL;if(_pM){var _pN = [1];}else{var _pO = E(_pG);var _pP = _pO[1];var _pQ = _pP-5;var _pR = _pQ+10;var _pS = 385>=_pR;if(_pS){var _pT = [1];}else{var _pU = _pQ>=400;if(_pU){var _pV = [1];}else{var _pW = T(function(){var _pX = _pP<=385;if(_pX){var _pY = T(function(){var _pZ = _pP>=400;return _pZ?E(_pC):[1];});var _q0 = [2,_py,_pY];}else{var _q1 = _pP>=400;var _q0 = _q1?E(_pC):[1];}return _q0;});var _q2 = _pF<=_pE;if(_q2){var _q3 = T(function(){var _q4 = _pE+40;var _q5 = _pF>=_q4;var _q6 = _q5?[2,_pz,_pW]:E(_pW);return _q6;});var _q7 = [2,_pA,_q3];}else{var _q8 = _pE+40;var _q9 = _pF>=_q8;var _qa = _q9?[2,_pz,_pW]:E(_pW);var _q7 = _qa;}var _pV = _q7;}var _pT = _pV;}var _pN = _pT;}var _pK = _pN;}return _pK;};var _qb = function(_qc){var _qd = E(_qc);var _qe = _qd[1];var _qf = _qd[2];var _qg = _qd[3];var _qh = _qd[4];var _qi = T(function(){var _qj = E(_qh);var _qk = _qj[1];var _ql = E(_qk);var _qm = _ql[1];var _qn = _ql[2];var _qo = E(_qm);var _qp = _qo[1];var _qq = T(function(){var _qr = _qp>595;if(_qr){var _qs = T(function(){var _qt = E(_qn);var _qu = _qt[1];var _qv = _qu<5;var _qw = _qv?E(_pC):[1];return _qw;});var _qx = [2,_pA,_qs];}else{var _qy = E(_qn);var _qz = _qy[1];var _qA = _qz<5;var _qB = _qA?E(_pC):[1];var _qx = _qB;}return _qx;});var _qC = T(function(){var _qD = E(_qe);var _qE = _qD[1];var _qF = E(_qE);var _qG = _qF[1];var _qH = _pD(_qG,_qp,_qn);var _qI = _d(_qH,_qf);return _qI;});var _qJ = _qp<5;if(_qJ){var _qK = [2,_pz,_qq];var _qL = _d(_qK,_qC);}else{var _qL = _d(_qq,_qC);}return _qL;});var _qM = [1,_qg,_qi];return _qM;};var _qN = T(function(){return _7h(_qb);});var _qO = function(_qP){var _qQ = _6H(_ps,_qN,_qP);var _qR = _qQ[1];var _qS = _qQ[2];var _qT = [1,_qR,_qS];return _qT;};var _qU = [2,_l,_l];var _qV = [1];var _qW = [2,_qV,_l];var _qX = [2,_qW,_l];var _qY = function(_qZ,_r0){var _r1 = E(_r0);if(_r1[0]==1){var _r2 = [1,_l,_l];}else{var _r3 = _r1[1];var _r4 = _r1[2];var _r5 = E(_qZ);var _r6 = _r5[1];var _r7 = E(_r6);var _r8 = _r7[1];var _r9 = _r7[2];var _ra = E(_r8);var _rb = _ra[1];var _rc = _rb-5;var _rd = E(_r3);var _re = _rd[1];var _rf = E(_re);var _rg = _rf[1];var _rh = _rf[2];var _ri = E(_rg);var _rj = _ri[1];var _rk = _rj+60;var _rl = _rc>=_rk;if(_rl){var _rm = function(_rn,_ro,_rp){while(1){var _rq = E(_rp);if(_rq[0]==1){var _rr = [1,_rn,_ro];}else{var _rs = _rq[1];var _rt = _rq[2];var _ru = E(_rs);var _rv = _ru[1];var _rw = E(_rv);var _rx = _rw[1];var _ry = _rw[2];var _rz = E(_rx);var _rA = _rz[1];var _rB = _rA+60;var _rC = _rc>=_rB;if(_rC){var _rD = (function(_ro){return T(function(){return _d(_ro,_qU);})})(_ro);_rn=_rn;_ro=_rD;_rp=_rt;continue;var _rE = die("Unreachable!");}else{var _rF = _rc+10;var _rG = _rA>=_rF;if(_rG){var _rH = (function(_ro){return T(function(){return _d(_ro,_qU);})})(_ro);_rn=_rn;_ro=_rH;_rp=_rt;continue;var _rI = die("Unreachable!");}else{var _rJ = E(_r9);var _rK = _rJ[1];var _rL = _rK-5;var _rM = E(_ry);var _rN = _rM[1];var _rO = _rN+20;var _rP = _rL>=_rO;if(_rP){var _rQ = (function(_ro){return T(function(){return _d(_ro,_qU);})})(_ro);_rn=_rn;_ro=_rQ;_rp=_rt;continue;var _rR = die("Unreachable!");}else{var _rS = _rL+10;var _rT = _rN>=_rS;if(_rT){var _rU = (function(_ro){return T(function(){return _d(_ro,_qU);})})(_ro);_rn=_rn;_ro=_rU;_rp=_rt;continue;var _rV = die("Unreachable!");}else{var _rW = (function(_ro){return T(function(){return _d(_ro,_qX);})})(_ro);var _rX = (function(_rb,_rn,_rA,_rK,_rN){return T(function(){var _rY = T(function(){var _rZ = _rK<=_rN;if(_rZ){var _s0 = T(function(){var _s1 = _rN+20;var _s2 = _rK>=_s1;var _s3 = _s2?E(_pC):[1];return _s3;});var _s4 = [2,_py,_s0];}else{var _s5 = _rN+20;var _s6 = _rK>=_s5;var _s7 = _s6?E(_pC):[1];var _s4 = _s7;}return _s4;});var _s8 = _rb<=_rA;if(_s8){var _s9 = T(function(){var _sa = _rA+60;var _sb = _rb>=_sa;var _sc = _sb?[2,_pz,_rY]:E(_rY);return _sc;});var _sd = [2,_pA,_s9];var _se = _d(_sd,_rn);}else{var _sf = _rA+60;var _sg = _rb>=_sf;if(_sg){var _sh = [2,_pz,_rY];var _si = _d(_sh,_rn);}else{var _si = _d(_rY,_rn);}var _se = _si;}return _se;})})(_rb,_rn,_rA,_rK,_rN);_rn=_rX;_ro=_rW;_rp=_rt;continue;var _rV = die("Unreachable!");}var _rR = _rV;}var _rI = _rR;}var _rE = _rI;}var _rr = _rE;}return _rr;}};var _sj = T(function(){return _d(_l,_qU);});var _sk = _rm(_l,_sj,_r4);}else{var _sl = _rc+10;var _sm = _rj>=_sl;if(_sm){var _sn = function(_so,_sp,_sq){while(1){var _sr = E(_sq);if(_sr[0]==1){var _ss = [1,_so,_sp];}else{var _st = _sr[1];var _su = _sr[2];var _sv = E(_st);var _sw = _sv[1];var _sx = E(_sw);var _sy = _sx[1];var _sz = _sx[2];var _sA = E(_sy);var _sB = _sA[1];var _sC = _sB+60;var _sD = _rc>=_sC;if(_sD){var _sE = (function(_sp){return T(function(){return _d(_sp,_qU);})})(_sp);_so=_so;_sp=_sE;_sq=_su;continue;var _sF = die("Unreachable!");}else{var _sG = _rc+10;var _sH = _sB>=_sG;if(_sH){var _sI = (function(_sp){return T(function(){return _d(_sp,_qU);})})(_sp);_so=_so;_sp=_sI;_sq=_su;continue;var _sJ = die("Unreachable!");}else{var _sK = E(_r9);var _sL = _sK[1];var _sM = _sL-5;var _sN = E(_sz);var _sO = _sN[1];var _sP = _sO+20;var _sQ = _sM>=_sP;if(_sQ){var _sR = (function(_sp){return T(function(){return _d(_sp,_qU);})})(_sp);_so=_so;_sp=_sR;_sq=_su;continue;var _sS = die("Unreachable!");}else{var _sT = _sM+10;var _sU = _sO>=_sT;if(_sU){var _sV = (function(_sp){return T(function(){return _d(_sp,_qU);})})(_sp);_so=_so;_sp=_sV;_sq=_su;continue;var _sW = die("Unreachable!");}else{var _sX = (function(_sp){return T(function(){return _d(_sp,_qX);})})(_sp);var _sY = (function(_rb,_so,_sB,_sL,_sO){return T(function(){var _sZ = T(function(){var _t0 = _sL<=_sO;if(_t0){var _t1 = T(function(){var _t2 = _sO+20;var _t3 = _sL>=_t2;var _t4 = _t3?E(_pC):[1];return _t4;});var _t5 = [2,_py,_t1];}else{var _t6 = _sO+20;var _t7 = _sL>=_t6;var _t8 = _t7?E(_pC):[1];var _t5 = _t8;}return _t5;});var _t9 = _rb<=_sB;if(_t9){var _ta = T(function(){var _tb = _sB+60;var _tc = _rb>=_tb;var _td = _tc?[2,_pz,_sZ]:E(_sZ);return _td;});var _te = [2,_pA,_ta];var _tf = _d(_te,_so);}else{var _tg = _sB+60;var _th = _rb>=_tg;if(_th){var _ti = [2,_pz,_sZ];var _tj = _d(_ti,_so);}else{var _tj = _d(_sZ,_so);}var _tf = _tj;}return _tf;})})(_rb,_so,_sB,_sL,_sO);_so=_sY;_sp=_sX;_sq=_su;continue;var _sW = die("Unreachable!");}var _sS = _sW;}var _sJ = _sS;}var _sF = _sJ;}var _ss = _sF;}return _ss;}};var _tk = T(function(){return _d(_l,_qU);});var _tl = _sn(_l,_tk,_r4);}else{var _tm = E(_r9);var _tn = _tm[1];var _to = _tn-5;var _tp = E(_rh);var _tq = _tp[1];var _tr = _tq+20;var _ts = _to>=_tr;if(_ts){var _tt = function(_tu,_tv,_tw){while(1){var _tx = E(_tw);if(_tx[0]==1){var _ty = [1,_tu,_tv];}else{var _tz = _tx[1];var _tA = _tx[2];var _tB = E(_tz);var _tC = _tB[1];var _tD = E(_tC);var _tE = _tD[1];var _tF = _tD[2];var _tG = E(_tE);var _tH = _tG[1];var _tI = _tH+60;var _tJ = _rc>=_tI;if(_tJ){var _tK = (function(_tv){return T(function(){return _d(_tv,_qU);})})(_tv);_tu=_tu;_tv=_tK;_tw=_tA;continue;var _tL = die("Unreachable!");}else{var _tM = _rc+10;var _tN = _tH>=_tM;if(_tN){var _tO = (function(_tv){return T(function(){return _d(_tv,_qU);})})(_tv);_tu=_tu;_tv=_tO;_tw=_tA;continue;var _tP = die("Unreachable!");}else{var _tQ = E(_tF);var _tR = _tQ[1];var _tS = _tR+20;var _tT = _to>=_tS;if(_tT){var _tU = (function(_tv){return T(function(){return _d(_tv,_qU);})})(_tv);_tu=_tu;_tv=_tU;_tw=_tA;continue;var _tV = die("Unreachable!");}else{var _tW = _to+10;var _tX = _tR>=_tW;if(_tX){var _tY = (function(_tv){return T(function(){return _d(_tv,_qU);})})(_tv);_tu=_tu;_tv=_tY;_tw=_tA;continue;var _tZ = die("Unreachable!");}else{var _u0 = (function(_tv){return T(function(){return _d(_tv,_qX);})})(_tv);var _u1 = (function(_rb,_tn,_tu,_tH,_tR){return T(function(){var _u2 = T(function(){var _u3 = _tn<=_tR;if(_u3){var _u4 = T(function(){var _u5 = _tR+20;var _u6 = _tn>=_u5;var _u7 = _u6?E(_pC):[1];return _u7;});var _u8 = [2,_py,_u4];}else{var _u9 = _tR+20;var _ua = _tn>=_u9;var _ub = _ua?E(_pC):[1];var _u8 = _ub;}return _u8;});var _uc = _rb<=_tH;if(_uc){var _ud = T(function(){var _ue = _tH+60;var _uf = _rb>=_ue;var _ug = _uf?[2,_pz,_u2]:E(_u2);return _ug;});var _uh = [2,_pA,_ud];var _ui = _d(_uh,_tu);}else{var _uj = _tH+60;var _uk = _rb>=_uj;if(_uk){var _ul = [2,_pz,_u2];var _um = _d(_ul,_tu);}else{var _um = _d(_u2,_tu);}var _ui = _um;}return _ui;})})(_rb,_tn,_tu,_tH,_tR);_tu=_u1;_tv=_u0;_tw=_tA;continue;var _tZ = die("Unreachable!");}var _tV = _tZ;}var _tP = _tV;}var _tL = _tP;}var _ty = _tL;}return _ty;}};var _un = T(function(){return _d(_l,_qU);});var _uo = _tt(_l,_un,_r4);}else{var _up = _to+10;var _uq = _tq>=_up;if(_uq){var _ur = function(_us,_ut,_uu){while(1){var _uv = E(_uu);if(_uv[0]==1){var _uw = [1,_us,_ut];}else{var _ux = _uv[1];var _uy = _uv[2];var _uz = E(_ux);var _uA = _uz[1];var _uB = E(_uA);var _uC = _uB[1];var _uD = _uB[2];var _uE = E(_uC);var _uF = _uE[1];var _uG = _uF+60;var _uH = _rc>=_uG;if(_uH){var _uI = (function(_ut){return T(function(){return _d(_ut,_qU);})})(_ut);_us=_us;_ut=_uI;_uu=_uy;continue;var _uJ = die("Unreachable!");}else{var _uK = _rc+10;var _uL = _uF>=_uK;if(_uL){var _uM = (function(_ut){return T(function(){return _d(_ut,_qU);})})(_ut);_us=_us;_ut=_uM;_uu=_uy;continue;var _uN = die("Unreachable!");}else{var _uO = E(_uD);var _uP = _uO[1];var _uQ = _uP+20;var _uR = _to>=_uQ;if(_uR){var _uS = (function(_ut){return T(function(){return _d(_ut,_qU);})})(_ut);_us=_us;_ut=_uS;_uu=_uy;continue;var _uT = die("Unreachable!");}else{var _uU = _to+10;var _uV = _uP>=_uU;if(_uV){var _uW = (function(_ut){return T(function(){return _d(_ut,_qU);})})(_ut);_us=_us;_ut=_uW;_uu=_uy;continue;var _uX = die("Unreachable!");}else{var _uY = (function(_ut){return T(function(){return _d(_ut,_qX);})})(_ut);var _uZ = (function(_rb,_tn,_us,_uF,_uP){return T(function(){var _v0 = T(function(){var _v1 = _tn<=_uP;if(_v1){var _v2 = T(function(){var _v3 = _uP+20;var _v4 = _tn>=_v3;var _v5 = _v4?E(_pC):[1];return _v5;});var _v6 = [2,_py,_v2];}else{var _v7 = _uP+20;var _v8 = _tn>=_v7;var _v9 = _v8?E(_pC):[1];var _v6 = _v9;}return _v6;});var _va = _rb<=_uF;if(_va){var _vb = T(function(){var _vc = _uF+60;var _vd = _rb>=_vc;var _ve = _vd?[2,_pz,_v0]:E(_v0);return _ve;});var _vf = [2,_pA,_vb];var _vg = _d(_vf,_us);}else{var _vh = _uF+60;var _vi = _rb>=_vh;if(_vi){var _vj = [2,_pz,_v0];var _vk = _d(_vj,_us);}else{var _vk = _d(_v0,_us);}var _vg = _vk;}return _vg;})})(_rb,_tn,_us,_uF,_uP);_us=_uZ;_ut=_uY;_uu=_uy;continue;var _uX = die("Unreachable!");}var _uT = _uX;}var _uN = _uT;}var _uJ = _uN;}var _uw = _uJ;}return _uw;}};var _vl = T(function(){return _d(_l,_qU);});var _vm = _ur(_l,_vl,_r4);}else{var _vn = function(_vo,_vp,_vq){while(1){var _vr = E(_vq);if(_vr[0]==1){var _vs = [1,_vo,_vp];}else{var _vt = _vr[1];var _vu = _vr[2];var _vv = E(_vt);var _vw = _vv[1];var _vx = E(_vw);var _vy = _vx[1];var _vz = _vx[2];var _vA = E(_vy);var _vB = _vA[1];var _vC = _vB+60;var _vD = _rc>=_vC;if(_vD){var _vE = (function(_vp){return T(function(){return _d(_vp,_qU);})})(_vp);_vo=_vo;_vp=_vE;_vq=_vu;continue;var _vF = die("Unreachable!");}else{var _vG = _rc+10;var _vH = _vB>=_vG;if(_vH){var _vI = (function(_vp){return T(function(){return _d(_vp,_qU);})})(_vp);_vo=_vo;_vp=_vI;_vq=_vu;continue;var _vJ = die("Unreachable!");}else{var _vK = E(_vz);var _vL = _vK[1];var _vM = _vL+20;var _vN = _to>=_vM;if(_vN){var _vO = (function(_vp){return T(function(){return _d(_vp,_qU);})})(_vp);_vo=_vo;_vp=_vO;_vq=_vu;continue;var _vP = die("Unreachable!");}else{var _vQ = _to+10;var _vR = _vL>=_vQ;if(_vR){var _vS = (function(_vp){return T(function(){return _d(_vp,_qU);})})(_vp);_vo=_vo;_vp=_vS;_vq=_vu;continue;var _vT = die("Unreachable!");}else{var _vU = (function(_vp){return T(function(){return _d(_vp,_qX);})})(_vp);var _vV = (function(_rb,_tn,_vo,_vB,_vL){return T(function(){var _vW = T(function(){var _vX = _tn<=_vL;if(_vX){var _vY = T(function(){var _vZ = _vL+20;var _w0 = _tn>=_vZ;var _w1 = _w0?E(_pC):[1];return _w1;});var _w2 = [2,_py,_vY];}else{var _w3 = _vL+20;var _w4 = _tn>=_w3;var _w5 = _w4?E(_pC):[1];var _w2 = _w5;}return _w2;});var _w6 = _rb<=_vB;if(_w6){var _w7 = T(function(){var _w8 = _vB+60;var _w9 = _rb>=_w8;var _wa = _w9?[2,_pz,_vW]:E(_vW);return _wa;});var _wb = [2,_pA,_w7];var _wc = _d(_wb,_vo);}else{var _wd = _vB+60;var _we = _rb>=_wd;if(_we){var _wf = [2,_pz,_vW];var _wg = _d(_wf,_vo);}else{var _wg = _d(_vW,_vo);}var _wc = _wg;}return _wc;})})(_rb,_tn,_vo,_vB,_vL);_vo=_vV;_vp=_vU;_vq=_vu;continue;var _vT = die("Unreachable!");}var _vP = _vT;}var _vJ = _vP;}var _vF = _vJ;}var _vs = _vF;}return _vs;}};var _wh = T(function(){return _d(_l,_qX);});var _wi = T(function(){var _wj = T(function(){var _wk = _tn<=_tq;if(_wk){var _wl = T(function(){var _wm = _tq+20;var _wn = _tn>=_wm;var _wo = _wn?E(_pC):[1];return _wo;});var _wp = [2,_py,_wl];}else{var _wq = _tq+20;var _wr = _tn>=_wq;var _ws = _wr?E(_pC):[1];var _wp = _ws;}return _wp;});var _wt = _rb<=_rj;if(_wt){var _wu = T(function(){var _wv = _rj+60;var _ww = _rb>=_wv;var _wx = _ww?[2,_pz,_wj]:E(_wj);return _wx;});var _wy = [2,_pA,_wu];var _wz = _d(_wy,_l);}else{var _wA = _rj+60;var _wB = _rb>=_wA;if(_wB){var _wC = [2,_pz,_wj];var _wD = _d(_wC,_l);}else{var _wD = _d(_wj,_l);}var _wz = _wD;}return _wz;});var _vm = _vn(_wi,_wh,_r4);}var _uo = _vm;}var _tl = _uo;}var _sk = _tl;}var _r2 = _sk;}return _r2;};var _wE = function(_wF){var _wG = E(_wF);var _wH = _wG[1];var _wI = _wG[2];var _wJ = _wG[3];var _wK = T(function(){var _wL = _qY(_wI,_wJ);var _wM = _wL[1];var _wN = _wL[2];var _wO = [1,_wM,_wN];return _wO;});var _wP = T(function(){var _wQ = E(_wK);var _wR = _wQ[2];var _wS = E(_wR);return _wS;});var _wT = T(function(){var _wU = E(_wK);var _wV = _wU[1];var _wW = E(_wV);return _wW;});var _wX = [1,_wH,_wT,_wP,_wI];return _wX;};var _wY = T(function(){return _7h(_wE);});var _wZ = function(_x0){var _x1 = _6H(_qO,_wY,_x0);var _x2 = _x1[1];var _x3 = _x1[2];var _x4 = [1,_x2,_x3];return _x4;};var _x5 = function(_x6){var _x7 = E(_x6);var _x8 = _x7[1];var _x9 = _x7[2];var _xa = _x7[3];var _xb = _x7[4];var _xc = [1,_xa,_xb];var _xd = [1,_x8,_x9];var _xe = [1,_xd,_xc];return _xe;};var _xf = T(function(){return _7h(_x5);});var _xg = function(_xh){var _xi = _6H(_xf,_wZ,_xh);var _xj = _xi[1];var _xk = _xi[2];var _xl = [1,_xj,_xk];return _xl;};var _xm = function(_xn){var _xo = E(_xn);var _xp = _xo[1];var _xq = _xo[2];var _xr = T(function(){var _xs = E(_xq);var _xt = _xs[2];var _xu = E(_xt);return _xu;});var _xv = T(function(){var _xw = E(_xq);var _xx = _xw[1];var _xy = E(_xx);return _xy;});var _xz = [1,_xp,_xv,_xr];return _xz;};var _xA = T(function(){return _7h(_xm);});var _xB = function(_xC){var _xD = _6H(_xg,_xA,_xC);var _xE = _xD[1];var _xF = _xD[2];var _xG = [1,_xE,_xF];return _xG;};var _xH = function(_xI){var _xJ = _7S(_xB,_xI);var _xK = _xJ[1];var _xL = _xJ[2];var _xM = [1,_xK,_xL];return _xM;};var _xN = function(_xO){var _xP = E(_xO);var _xQ = _xP[1];var _xR = _xP[2];var _xS = _7v(_xH,_xQ,_xR);var _xT = _xS[1];var _xU = _xS[2];var _xV = [1,_xT,_xU];return _xV;};var _xW = function(_xX){var _xY = E(_xX);var _xZ = _xY[1];var _y0 = _xY[2];var _y1 = E(_xZ);var _y2 = _y1[1];var _y3 = _y1[2];var _y4 = [1,_y0,_y2,_y3];return _y4;};var _y5 = T(function(){return _7h(_xW);});var _y6 = function(_y7){var _y8 = _6H(_y5,_xN,_y7);var _y9 = _y8[1];var _ya = _y8[2];var _yb = [1,_y9,_ya];return _yb;};var _yc = function(_yd){return [1,_yd,_yd];};var _ye = T(function(){return _7h(_yc);});var _yf = function(_yg){var _yh = _6H(_y6,_ye,_yg);var _yi = _yh[1];var _yj = _yh[2];var _yk = [1,_yi,_yj];return _yk;};var _yl = function(_ym){var _yn = E(_ym);var _yo = _yn[1];var _yp = _yn[2];var _yq = _yn[3];var _yr = [1,_yo,_yp,_yq];return _yr;};var _ys = T(function(){return _7h(_yl);});var _yt = function(_yu){return _79(_7s,_ys,_yu);};var _yv = function(_yw){var _yx = _6H(_yt,_yf,_yw);var _yy = _yx[1];var _yz = _yx[2];var _yA = [1,_yy,_yz];return _yA;};var _yB = function(_yC){var _yD = _6H(_yv,_gz,_yC);var _yE = _yD[1];var _yF = _yD[2];var _yG = [1,_yE,_yF];return _yG;};var _yH = T(function(){return _7h(_de);});var _yI = function(_yJ){var _yK = _6H(_yB,_yH,_yJ);var _yL = _yK[1];var _yM = _yK[2];var _yN = [1,_yL,_yM];return _yN;};var _yO = function(_yP){while(1){var _yQ = E(_yP);if(_yQ[0]==1){var _yR = [1];}else{var _yS = _yQ[1];var _yT = _yQ[2];var _yU = E(_yS);if(_yU[0]==1){var _yV = _yU[1];var _yW = E(_yV);var _yX = _yW[1];var _yY = E(_yX);if(_yY==13){var _yZ = (function(_yT){return T(function(){return _yO(_yT);})})(_yT);var _z0 = [2,_yI,_yZ];}else{_yP=_yT;continue;var _z0 = die("Unreachable!");}var _z1 = _z0;}else{_yP=_yT;continue;var _z1 = die("Unreachable!");}var _yR = _z1;}return _yR;}};var _z2 = function(_z3){var _z4 = E(_z3);var _z5 = _z4[1];var _z6 = _z4[2];var _z7 = T(function(){var _z8 = E(_z6);if(_z8[0]==1){var _z9 = _z8[2];var _za = E(_z9);var _zb = _za[1];var _zc = E(_zb);var _zd = _zc[2];var _ze = E(_zd);var _zf = _ze[1];var _zg = _zf>400;var _zh = _zg?E(_b2):[1];var _zi = _zh;}else{var _zi = [1];}return _zi;});var _zj = T(function(){return _yO(_z5);});var _zk = [1,_z5,_zj,_z7];return _zk;};var _zl = T(function(){return _7h(_z2);});var _zm = function(_zn){var _zo = _6H(_aW,_zl,_zn);var _zp = _zo[1];var _zq = _zo[2];var _zr = [1,_zp,_zq];return _zr;};var _zs = function(_zt){return E(_zt);};var _zu = T(function(){return _7h(_zs);});var _zv = function(_zw){var _zx = _6H(_zu,_zm,_zw);var _zy = _zx[1];var _zz = _zx[2];var _zA = [1,_zy,_zz];return _zA;};var _zB = function(_zC){return E(_zC);};var _zD = T(function(){return _7h(_zB);});var _zE = function(_zF){var _zG = _6H(_zv,_zD,_zF);var _zH = _zG[1];var _zI = _zG[2];var _zJ = [1,_zH,_zI];return _zJ;};var _zK = function(_zL){var _zM = _7S(_zE,_zL);var _zN = _zM[1];var _zO = _zM[2];var _zP = [1,_zN,_zO];return _zP;};var _zQ = function(_zR){var _zS = E(_zR);var _zT = _zS[1];var _zU = _zS[2];var _zV = _7v(_zK,_zT,_zU);var _zW = _zV[1];var _zX = _zV[2];var _zY = [1,_zW,_zX];return _zY;};var _zZ = T(function(){return _7h(_9s);});var _A0 = function(_A1){var _A2 = _6H(_zZ,_zQ,_A1);var _A3 = _A2[1];var _A4 = _A2[2];var _A5 = [1,_A3,_A4];return _A5;};var _A6 = T(function(){return _7h(_fI);});var _A7 = function(_A8){var _A9 = _6H(_A0,_A6,_A8);var _Aa = _A9[1];var _Ab = _A9[2];var _Ac = [1,_Aa,_Ab];return _Ac;};var _Ad = function(_Ae){var _Af = _6H(_7t,_A7,_Ae);var _Ag = _Af[1];var _Ah = _Af[2];var _Ai = [1,_Ag,_Ah];return _Ai;};var _Aj = T(function(){return _7h(_de);});var _Ak = function(_Al){var _Am = _6H(_Ad,_Aj,_Al);var _An = _Am[1];var _Ao = _Am[2];var _Ap = [1,_An,_Ao];return _Ap;};var _Aq = function(_Ar){var _As = nMV(_Ak,_Ar);var _At = _As[1];var _Au = _As[2];var _Av = nMV(_l,_At);var _Aw = _Av[1];var _Ax = _Av[2];var _Ay = function(_Az,_AA){var _AB = rMV(_Ax,_AA);var _AC = _AB[1];var _AD = _AB[2];var _AE = T(function(){var _AF = [2,_Az];var _AG = [2,_AF,_l];return _d(_AD,_AG);});var _AH = wMV(_Ax,_AE,_AC);var _AI = [1,_AH,_0];return _AI;};var _AJ = A(_2t,[_2O,_Ay,_Aw]);var _AK = _AJ[1];var _AL = function(_AM,_AN){var _AO = rMV(_Ax,_AN);var _AP = _AO[1];var _AQ = _AO[2];var _AR = T(function(){var _AS = [1,_AM];var _AT = [2,_AS,_l];return _d(_AQ,_AT);});var _AU = wMV(_Ax,_AR,_AP);var _AV = [1,_AU,_0];return _AV;};var _AW = A(_2M,[_2O,_AL,_AK]);var _AX = _AW[1];var _AY = function(_AZ){var _B0 = rMV(_Au,_AZ);var _B1 = _B0[1];var _B2 = _B0[2];var _B3 = rMV(_Ax,_B1);var _B4 = _B3[1];var _B5 = _B3[2];var _B6 = A(_B2,[_B5]);var _B7 = _B6[1];var _B8 = _B6[2];var _B9 = wMV(_Ax,_l,_B4);var _Ba = _5x(_B7,_B9);var _Bb = _Ba[1];var _Bc = wMV(_Au,_B8,_Bb);var _Bd = [1,_Bc,_0];return _Bd;};var _Be = A(_y,[_2N,_AY,_AX]);return _Be;};var _Bf = T(function(){return A(_a,[_Aq]);});
E(E(_Bf)(0));
