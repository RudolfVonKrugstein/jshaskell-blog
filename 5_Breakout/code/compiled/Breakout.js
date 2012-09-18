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


var _0 = [1];var _1 = function(_2,_3){var _4 = E(_2);var _5 = [1,_4];var _6 = _5[1];var _7 = jsSetOnLoad(_6,_3);var _8 = _7[1];var _9 = [1,_8,_0];return _9;};var _a = function(_b,_c){return _1(_b,_c);};var _d = function(_e){return E(_e);};var _f = function(_g){return _d(_g);};var _h = function(_i,_j){var _k = E(_i);if(_k[0]==1){var _l = E(_j);}else{var _m = _k[1];var _n = _k[2];var _o = T(function(){return _h(_n,_j);});var _l = [2,_m,_o];}return _l;};var _p = [1];var _q = function(_r,_s,_t){var _u = E(_r);var _v = _u[1];var _w = E(_s);var _x = [1,_w];var _y = _x[1];var _z = jsSetInterval(_v,_y,_t);var _A = _z[1];var _B = [1,_A,_0];return _B;};var _C = function(_D,_b,_c){return _q(_D,_b,_c);};var _E = [15,coercionToken];var _F = "load";var _G = [1,_F];var _H = "mousemove";var _I = [1,_H];var _J = "mouseover";var _K = [1,_J];var _L = "mouseout";var _M = [1,_L];var _N = "click";var _O = [1,_N];var _P = "dblclick";var _Q = [1,_P];var _R = "mousedown";var _S = [1,_R];var _T = "mouseup";var _U = [1,_T];var _V = "keypress";var _W = [1,_V];var _X = "keyup";var _Y = [1,_X];var _Z = "keydown";var _10 = [1,_Z];var _11 = "unload";var _12 = [1,_11];var _13 = "change";var _14 = [1,_13];var _15 = "focus";var _16 = [1,_15];var _17 = "blur";var _18 = [1,_17];var _19 = function(_1a,_1b,_1c,_1d){var _1e = [1,_1c];var _1f = _1e[1];var _1g = function(_1h){var _1i = E(_1c);var _1j = jsSetCB(_1a,_1h,_1f,_1d);var _1k = _1j[1];var _1l = _1j[2];var _1m = T(function(){var _1n = E(_1l);return _1n?true:false;});var _1o = [1,_1k,_1m];return _1o;};var _1p = E(_1b);switch(_1p[0]){case 1:var _1q = E(_G);var _1r = _1q[1];var _1s = _1g(_1r);var _1t = _1s;break;case 2:var _1u = E(_12);var _1v = _1u[1];var _1w = _1g(_1v);var _1t = _1w;break;case 3:var _1x = E(_14);var _1y = _1x[1];var _1z = _1g(_1y);var _1t = _1z;break;case 4:var _1A = E(_16);var _1B = _1A[1];var _1C = _1g(_1B);var _1t = _1C;break;case 5:var _1D = E(_18);var _1E = _1D[1];var _1F = _1g(_1E);var _1t = _1F;break;case 6:var _1G = E(_I);var _1H = _1G[1];var _1I = _1g(_1H);var _1t = _1I;break;case 7:var _1J = E(_K);var _1K = _1J[1];var _1L = _1g(_1K);var _1t = _1L;break;case 8:var _1M = E(_M);var _1N = _1M[1];var _1O = _1g(_1N);var _1t = _1O;break;case 9:var _1P = E(_O);var _1Q = _1P[1];var _1R = _1g(_1Q);var _1t = _1R;break;case 10:var _1S = E(_Q);var _1T = _1S[1];var _1U = _1g(_1T);var _1t = _1U;break;case 11:var _1V = E(_S);var _1W = _1V[1];var _1X = _1g(_1W);var _1t = _1X;break;case 12:var _1Y = E(_U);var _1Z = _1Y[1];var _20 = _1g(_1Z);var _1t = _20;break;case 13:var _21 = E(_W);var _22 = _21[1];var _23 = _1g(_22);var _1t = _23;break;case 14:var _24 = E(_Y);var _25 = _24[1];var _26 = _1g(_25);var _1t = _26;break;case 15:var _27 = E(_10);var _28 = _27[1];var _29 = _1g(_28);var _1t = _29;break;}return _1t;};var _2a = T(function(){return unCStr(" could be found!");});var _2b = function(_2c){var _2d = T(function(){return _h(_2c,_2a);});var _2e = unAppCStr("No element with ID ",_2d);var _2f = err(_2e);return _2f;};var _2g = function(_2h,_2i,_2j){var _2k = toJSStr(_2h);var _2l = _2k[1];var _2m = jsFind(_2l,_2j);var _2n = _2m[1];var _2o = _2m[2];var _2p = [1,_2o];var _2q = _2p[1];var _2r = E(_2q);if(_2r[0]==1){var _2s = _2b(_2h);}else{var _2t = _2r[1];var _2u = E(_2t);var _2v = _2u[1];var _2w = _19(_2v,_E,_2i,_2n);var _2s = _2w;}return _2s;};var _2x = function(_D,_b,_c){return _2g(_D,_b,_c);};var _2y = [14,coercionToken];var _2z = function(_2A,_2B,_2C){var _2D = toJSStr(_2A);var _2E = _2D[1];var _2F = jsFind(_2E,_2C);var _2G = _2F[1];var _2H = _2F[2];var _2I = [1,_2H];var _2J = _2I[1];var _2K = E(_2J);if(_2K[0]==1){var _2L = _2b(_2A);}else{var _2M = _2K[1];var _2N = E(_2M);var _2O = _2N[1];var _2P = _19(_2O,_2y,_2B,_2G);var _2L = _2P;}return _2L;};var _2Q = function(_D,_b,_c){return _2z(_D,_b,_c);};var _2R = [1,20];var _2S = T(function(){return unCStr("canvas3");});var _2T = function(_2U,_2V){var _2W = E(_2U);var _2X = _2W[1];var _2Y = jsClear(_2X,_2V);var _2Z = _2Y[1];var _30 = [1,_2Z,_0];return _30;};var _31 = function(_b,_c){return _2T(_b,_c);};var _32 = function(_33,_34,_35,_36,_37){var _38 = jsBeginPath(_33,_37);var _39 = _38[1];var _3a = jsArc(_33,_34,_35,_36,0,6.283185307179586,1,_39);var _3b = _3a[1];var _3c = jsClosePath(_33,_3b);var _3d = _3c[1];var _3e = jsFill(_33,_3d);var _3f = _3e[1];var _3g = [1,_3f,_0];return _3g;};var _3h = function(_3i,_3j,_3k,_3l,_3m){var _3n = E(_3i);var _3o = _3n[1];var _3p = E(_3j);var _3q = _3p[1];var _3r = E(_3k);var _3s = _3r[1];var _3t = E(_3l);var _3u = _3t[1];var _3v = _32(_3o,_3q,_3s,_3u,_3m);return _3v;};var _3w = function(_3x,_3y,_D,_b,_c){return _3h(_3x,_3y,_D,_b,_c);};var _3z = function(_3A,_3B,_3C,_3D,_3E,_3F){var _3G = E(_3A);var _3H = _3G[1];var _3I = E(_3B);var _3J = _3I[1];var _3K = E(_3C);var _3L = _3K[1];var _3M = E(_3D);var _3N = _3M[1];var _3O = E(_3E);var _3P = _3O[1];var _3Q = jsFillRect(_3H,_3J,_3L,_3N,_3P,_3F);var _3R = _3Q[1];var _3S = [1,_3R,_0];return _3S;};var _3T = function(_3U,_3x,_3y,_D,_b,_c){return _3z(_3U,_3x,_3y,_D,_b,_c);};var _3V = function(_3W,_3X,_3Y,_3Z,_40){var _41 = E(_3W);var _42 = _41[1];var _43 = E(_3X);var _44 = _43[1];var _45 = E(_3Y);var _46 = _45[1];var _47 = E(_3Z);var _48 = _47[1];var _49 = jsFillText(_42,_44,_46,_48,_40);var _4a = _49[1];var _4b = [1,_4a,_0];return _4b;};var _4c = function(_3x,_3y,_D,_b,_c){return _3V(_3x,_3y,_D,_b,_c);};var _4d = function(_4e,_4f){var _4g = T(function(){return toJSStr(_4f);});return A(_4c,[_4e,_4g]);};var _4h = function(_4i,_4j){var _4k = toJSStr(_4i);var _4l = _4k[1];var _4m = jsFind(_4l,_4j);var _4n = _4m[1];var _4o = _4m[2];var _4p = [1,_4o];var _4q = _4p[1];var _4r = E(_4q);if(_4r[0]==1){var _4s = _2b(_4i);}else{var _4t = _4r[1];var _4u = E(_4t);var _4v = _4u[1];var _4w = jsGetContext2D(_4v,_4n);var _4x = _4w[1];var _4y = _4w[2];var _4z = [1,_4y];var _4A = [1,_4x,_4z];var _4s = _4A;}return _4s;};var _4B = function(_b,_c){return _4h(_b,_c);};var _4C = function(_4D,_4E,_4F){var _4G = E(_4D);var _4H = _4G[1];var _4I = toJSStr(_4E);var _4J = _4I[1];var _4K = jsSetFillColor(_4H,_4J,_4F);var _4L = _4K[1];var _4M = [1,_4L,_0];return _4M;};var _4N = function(_D,_b,_c){return _4C(_D,_b,_c);};var _4O = T(function(){return unCStr("red");});var _4P = [1,5];var _4Q = T(function(){return unCStr("blue");});var _4R = T(function(){return unCStr("darkblue");});var _4S = [1,60];var _4T = function(_4U,_4V,_4W){var _4X = T(function(){var _4Y = E(_4V);var _4Z = _4Y[2];var _50 = E(_4Z);var _51 = _50[1];var _52 = E(_51);var _53 = _52==1?E(_4Q):E(_4R);return _53;});var _54 = A(_4N,[_4U,_4X,_4W]);var _55 = _54[1];var _56 = T(function(){var _57 = E(_4V);var _58 = _57[1];var _59 = E(_58);var _5a = _59[1];var _5b = _59[2];var _5c = [1,_5a,_5b,_4S,_2R];return _5c;});var _5d = T(function(){var _5e = E(_56);var _5f = _5e[4];var _5g = E(_5f);return _5g;});var _5h = T(function(){var _5i = E(_56);var _5j = _5i[3];var _5k = E(_5j);return _5k;});var _5l = T(function(){var _5m = E(_56);var _5n = _5m[2];var _5o = E(_5n);return _5o;});var _5p = T(function(){var _5q = E(_56);var _5r = _5q[1];var _5s = E(_5r);return _5s;});var _5t = A(_3T,[_4U,_5p,_5l,_5h,_5d,_55]);return _5t;};var _5u = T(function(){return A(_4B,[_2S]);});var _5v = T(function(){return unCStr("Press Space to start --- (click the canvas for input focus)");});var _5w = [1,200];var _5x = T(function(){return unCStr("black");});var _5y = [1,15];var _5z = [1,40];var _5A = [1,385];var _5B = function(_5C,_5D){var _5E = E(_5C);if(_5E[0]==1){var _5F = _5E[1];var _5G = _5E[2];var _5H = _5E[3];var _5I = A(_5u,[_5D]);var _5J = _5I[1];var _5K = _5I[2];var _5L = A(_31,[_5K,_5J]);var _5M = _5L[1];var _5N = A(_4N,[_5K,_5x,_5M]);var _5O = _5N[1];var _5P = T(function(){var _5Q = E(_5F);var _5R = _5Q[1];var _5S = [1,_5R,_5A,_5z,_5y];return _5S;});var _5T = T(function(){var _5U = E(_5P);var _5V = _5U[4];var _5W = E(_5V);return _5W;});var _5X = T(function(){var _5Y = E(_5P);var _5Z = _5Y[3];var _60 = E(_5Z);return _60;});var _61 = T(function(){var _62 = E(_5P);var _63 = _62[2];var _64 = E(_63);return _64;});var _65 = T(function(){var _66 = E(_5P);var _67 = _66[1];var _68 = E(_67);return _68;});var _69 = A(_3T,[_5K,_65,_61,_5X,_5T,_5O]);var _6a = _69[1];var _6b = function(_6c,_6d){while(1){var _6e = E(_6c);if(_6e[0]==1){var _6f = [1,_6d,_0];}else{var _6g = _6e[1];var _6h = _6e[2];var _6i = _4T(_5K,_6g,_6d);var _6j = _6i[1];_6c=_6h;_6d=_6j;continue;var _6k = die("Unreachable!");var _6f = _6k;}return _6f;}};var _6l = _6b(_5H,_6a);var _6m = _6l[1];var _6n = A(_4N,[_5K,_4O,_6m]);var _6o = _6n[1];var _6p = T(function(){var _6q = E(_5G);var _6r = _6q[1];var _6s = E(_6r);return _6s;});var _6t = T(function(){var _6u = E(_6p);var _6v = _6u[2];var _6w = E(_6v);return _6w;});var _6x = T(function(){var _6y = E(_6p);var _6z = _6y[1];var _6A = E(_6z);return _6A;});var _6B = A(_3w,[_5K,_6x,_6t,_4P,_6o]);var _6C = _6B;}else{var _6D = A(_4B,[_2S,_5D]);var _6E = _6D[1];var _6F = _6D[2];var _6G = A(_31,[_6F,_6E]);var _6H = _6G[1];var _6I = A(_4N,[_6F,_5x,_6H]);var _6J = _6I[1];var _6K = A(_4d,[_6F,_5v,_5w,_5w,_6J]);var _6C = _6K;}return _6C;};var _6L = function(_6M){var _6N = E(_6M);var _6O = _6N[2];var _6P = E(_6O);return _6P;};var _6Q = function(_6R){var _6S = E(_6R);var _6T = _6S[1];var _6U = E(_6T);return _6U;};var _6V = function(_6W){return [1,_6W,_6W];};var _6X = function(_6Y){var _6Z = T(function(){return _6X(_6Y);});var _70 = function(_71){var _72 = T(function(){return A(_6Y,[_71]);});return [1,_72,_6Z];};return E(_70);};var _73 = T(function(){return _6X(_6V);});var _74 = function(_75){var _76 = T(function(){var _77 = E(_75);var _78 = _77[1];var _79 = E(_78);return _79;});var _7a = T(function(){var _7b = E(_75);var _7c = _7b[2];var _7d = E(_7c);return _7d;});return [1,_7a,_76];};var _7e = T(function(){return _6X(_74);});var _7f = T(function(){return _6X(_74);});var _7g = function(_7h,_7i,_7j){var _7k = T(function(){return A(_7h,[_7i]);});var _7l = T(function(){var _7m = E(_7k);var _7n = _7m[2];var _7o = E(_7n);return _7o;});var _7p = function(_7q){var _7r = E(_7q);var _7s = _7r[1];var _7t = _7r[2];var _7u = _7g(_7l,_7s,_7t);var _7v = _7u[1];var _7w = _7u[2];var _7x = [1,_7v,_7w];return _7x;};var _7y = T(function(){var _7z = E(_7k);var _7A = _7z[1];var _7B = E(_7A);return _7B;});var _7C = [1,_7y,_7j];return [1,_7C,_7p];};var _7D = function(_7E,_7F){var _7G = E(_7F);var _7H = _7G[1];var _7I = _7G[2];var _7J = _7g(_7E,_7H,_7I);var _7K = _7J[1];var _7L = _7J[2];var _7M = [1,_7K,_7L];return _7M;};var _7N = function(_7O,_7P,_7Q){var _7R = T(function(){return A(_7P,[_7Q]);});var _7S = T(function(){var _7T = T(function(){var _7U = E(_7R);var _7V = _7U[1];var _7W = E(_7V);return _7W;});return A(_7O,[_7T]);});var _7X = T(function(){var _7Y = E(_7R);var _7Z = _7Y[2];var _80 = E(_7Z);return _80;});var _81 = T(function(){var _82 = E(_7S);var _83 = _82[2];var _84 = E(_83);return _84;});var _85 = function(_86){var _87 = _7N(_81,_7X,_86);var _88 = _87[1];var _89 = _87[2];var _8a = [1,_88,_89];return _8a;};var _8b = T(function(){var _8c = E(_7S);var _8d = _8c[1];var _8e = E(_8d);return _8e;});return [1,_8b,_85];};var _8f = function(_8g,_8h,_8i){var _8j = T(function(){return A(_7D,[_8g]);});var _8k = function(_g){return _7D(_8h,_g);};var _8l = function(_8m){var _8n = _7N(_7f,_8k,_8m);var _8o = _8n[1];var _8p = _8n[2];var _8q = [1,_8o,_8p];return _8q;};var _8r = function(_8s){var _8t = _7N(_8l,_7e,_8s);var _8u = _8t[1];var _8v = _8t[2];var _8w = [1,_8u,_8v];return _8w;};return _7N(_8r,_8j,_8i);};var _8x = function(_8y,_8z,_8A){var _8B = _8f(_8y,_8z,_8A);var _8C = _8B[1];var _8D = _8B[2];var _8E = [1,_8C,_8D];return _8E;};var _8F = function(_8G,_8H,_8I){var _8J = T(function(){return A(_8x,[_8G,_8H]);});var _8K = _7N(_8J,_73,_8I);var _8L = _8K[1];var _8M = _8K[2];var _8N = [1,_8L,_8M];return _8N;};var _8O = function(_8P,_8Q){var _8R = function(_g){return _7D(_8P,_g);};var _8S = function(_8T){var _8U = _7N(_7f,_8R,_8T);var _8V = _8U[1];var _8W = _8U[2];var _8X = [1,_8V,_8W];return _8X;};var _8Y = _7N(_8S,_7e,_8Q);var _8Z = _8Y[1];var _90 = _8Y[2];var _91 = [1,_8Z,_90];return _91;};var _92 = function(_93,_94,_95){var _96 = _7N(_93,_94,_95);var _97 = _96[1];var _98 = _96[2];var _99 = [1,_97,_98];return _99;};var _9a = function(_9b){return [1,_9b,_9c];};var _9c = function(_9d){var _9e = _9a(_9d);var _9f = _9e[1];var _9g = _9e[2];var _9h = [1,_9f,_9g];return _9h;};var _9i = [1,_9c,_92];var _9j = [1,_9i,_6X,_7D,_8O,_8x,_8F];var _9k = T(function(){return _6Q(_9j);});var _9l = function(_9m){var _9n = E(_9m);var _9o = _9n[2];var _9p = E(_9o);return _9p;};var _9q = function(_9r){return [1,_9r,_0];};var _9s = T(function(){return A(_9l,[_9j,_9q]);});var _9t = function(_9u){var _9v = E(_9u);var _9w = _9v[3];var _9x = E(_9w);return _9x;};var _9y = function(_9z){var _9A = E(_9z);var _9B = _9A[2];var _9C = E(_9B);return _9C;};var _9D = function(_9E,_9F){var _9G = T(function(){var _9H = T(function(){var _9I = E(_9G);var _9J = _9I[2];var _9K = E(_9J);return _9K;});var _9L = [1,_9F,_9H];var _9M = A(_9E,[_9L]);var _9N = _9M[1];var _9O = _9M[2];var _9P = E(_9N);var _9Q = _9P[1];var _9R = _9P[2];var _9S = [1,_9Q,_9R,_9O];return _9S;});var _9T = T(function(){var _9U = E(_9G);var _9V = _9U[3];var _9W = E(_9V);return _9W;});var _9X = function(_9Y){var _9Z = _9D(_9T,_9Y);var _a0 = _9Z[1];var _a1 = _9Z[2];var _a2 = [1,_a0,_a1];return _a2;};var _a3 = T(function(){var _a4 = E(_9G);var _a5 = _a4[1];var _a6 = E(_a5);return _a6;});return [1,_a3,_9X];};var _a7 = function(_a8,_a9){var _aa = _9D(_a8,_a9);var _ab = _aa[1];var _ac = _aa[2];var _ad = [1,_ab,_ac];return _ad;};var _ae = [1,_9j,_a7];var _af = function(_ag){var _ah = E(_ag);var _ai = _ah[1];var _aj = _ah[2];var _ak = _ah[3];var _al = T(function(){return _h(_aj,_ak);});var _am = [1,_al,_ai];return _am;};var _an = T(function(){return A(_9l,[_9j,_af]);});var _ao = function(_ap,_aq){while(1){var _ar = E(_aq);if(_ar[0]==1){var _as = E(_ap);}else{var _at = _ar[1];var _au = _ar[2];_ap=_at;_aq=_au;continue;var _as = die("Unreachable!");}return _as;}};var _av = function(_aw,_ax,_ay){var _az = T(function(){return A(_ao,[_aw,_ax,_ay]);});var _aA = T(function(){var _aB = E(_az);var _aC = _aB[2];var _aD = E(_aC);return _aD;});var _aE = function(_aF){var _aG = E(_aF);var _aH = _aG[1];var _aI = _aG[2];var _aJ = _av(_aA,_aH,_aI);var _aK = _aJ[1];var _aL = _aJ[2];var _aM = [1,_aK,_aL];return _aM;};var _aN = T(function(){var _aO = E(_az);var _aP = _aO[1];var _aQ = E(_aP);return _aQ;});return [1,_aN,_aE];};var _aR = function(_aS,_aT){var _aU = E(_aT);var _aV = _aU[1];var _aW = _aU[2];var _aX = _av(_aS,_aV,_aW);var _aY = _aX[1];var _aZ = _aX[2];var _b0 = [1,_aY,_aZ];return _b0;};var _b1 = function(_g){return A(_aR,[_g]);};var _b2 = function(_b3){return [2];};var _b4 = T(function(){return A(_9l,[_9j,_b2]);});var _b5 = T(function(){return A(_b1,[_b4]);});var _b6 = T(function(){return A(_6L,[_9k,_b5,_an]);});var _b7 = T(function(){return A(_9t,[_9j,_b6]);});var _b8 = function(_b9){var _ba = E(_b9);var _bb = _ba[1];var _bc = _ba[2];var _bd = E(_bc);var _be = E(_bb);return _be;};var _bf = T(function(){return A(_9l,[_9j,_b8]);});var _bg = function(_bh){return [1,_bh,_bh];};var _bi = T(function(){return A(_9l,[_9j,_bg]);});var _bj = function(_bk){return E(_bk);};var _bl = T(function(){return A(_9l,[_9j,_bj]);});var _bm = function(_bn,_bo){var _bp = T(function(){return A(_bq,[_bo]);});var _br = [1,_bo,_bn];return [1,_br,_bp];};var _bq = function(_bs,_bt){var _bu = _bm(_bs,_bt);var _bv = _bu[1];var _bw = _bu[2];var _bx = [1,_bv,_bw];return _bx;};var _by = function(_bz,_bA){var _bB = T(function(){return A(_bq,[_bA]);});var _bC = [1,_bA,_bz];return [1,_bC,_bB];};var _bD = function(_bE){var _bF = E(_bE);var _bG = _bF[2];var _bH = E(_bG);return _bH;};var _bI = T(function(){return _6X(_bD);});var _bJ = function(_bK,_bL){var _bM = T(function(){return A(_by,[_bK]);});var _bN = _7N(_bI,_bM,_bL);var _bO = _bN[1];var _bP = _bN[2];var _bQ = [1,_bO,_bP];return _bQ;};var _bR = function(_g){return A(_bJ,[_g]);};var _bS = [2];var _bT = T(function(){return A(_bR,[_bS]);});var _bU = T(function(){return A(_6L,[_9k,_bT,_bl]);});var _bV = T(function(){return A(_9t,[_9j,_bU]);});var _bW = function(_bX){var _bY = E(_bX);var _bZ = _bY[1];var _c0 = _bY[2];var _c1 = [1,_c0,_bZ];return _c1;};var _c2 = T(function(){return A(_9l,[_9j,_bW]);});var _c3 = function(_c4){return E(_c4);};var _c5 = T(function(){return A(_9l,[_9j,_c3]);});var _c6 = function(_c7,_c8){var _c9 = E(_c8);if(_c9[0]==1){var _ca = [1];}else{var _cb = _c9[1];var _cc = _c9[2];var _cd = T(function(){return _c6(_c7,_cc);});var _ce = T(function(){return A(_c7,[_cb]);});var _ca = [2,_ce,_cd];}return _ca;};var _cf = function(_cg,_ch){var _ci = function(_cj){return E(_ch);};return _c6(_ci,_cg);};var _ck = function(_cl,_cm){while(1){var _cn = E(_cm);if(_cn[0]==1){var _co = [1];}else{var _cp = _cn[1];var _cq = _cn[2];var _cr = A(_cl,[_cp]);if(_cr){var _cs = (function(_cl,_cq){return T(function(){return _ck(_cl,_cq);})})(_cl,_cq);var _ct = [2,_cp,_cs];}else{_cl=_cl;_cm=_cq;continue;var _ct = die("Unreachable!");}var _co = _ct;}return _co;}};var _cu = [2,_b4,_p];var _cv = function(_cw){var _cx = E(_cw);if(_cx[0]==1){var _cy = _cx[1];var _cz = E(_cy);var _cA = _cz[1];var _cB = E(_cA);var _cC = _cB==32?true:false;var _cD = _cC;}else{var _cD = false;}return _cD;};var _cE = [4];var _cF = [1];var _cG = [2];var _cH = [3];var _cI = [2,_cH,_p];var _cJ = [2,_p,_p];var _cK = [1];var _cL = [2,_cK,_p];var _cM = [2,_cL,_p];var _cN = function(_cO,_cP){var _cQ = E(_cP);if(_cQ[0]==1){var _cR = [1,_p,_p];}else{var _cS = _cQ[1];var _cT = _cQ[2];var _cU = E(_cO);var _cV = _cU[1];var _cW = E(_cV);var _cX = _cW[1];var _cY = _cW[2];var _cZ = E(_cX);var _d0 = _cZ[1];var _d1 = _d0-5;var _d2 = E(_cS);var _d3 = _d2[1];var _d4 = E(_d3);var _d5 = _d4[1];var _d6 = _d4[2];var _d7 = E(_d5);var _d8 = _d7[1];var _d9 = _d8+60;var _da = _d1>=_d9;if(_da){var _db = function(_dc,_dd,_de){while(1){var _df = E(_de);if(_df[0]==1){var _dg = [1,_dc,_dd];}else{var _dh = _df[1];var _di = _df[2];var _dj = E(_dh);var _dk = _dj[1];var _dl = E(_dk);var _dm = _dl[1];var _dn = _dl[2];var _do = E(_dm);var _dp = _do[1];var _dq = _dp+60;var _dr = _d1>=_dq;if(_dr){var _ds = (function(_dd){return T(function(){return _h(_dd,_cJ);})})(_dd);_dc=_dc;_dd=_ds;_de=_di;continue;var _dt = die("Unreachable!");}else{var _du = _d1+10;var _dv = _dp>=_du;if(_dv){var _dw = (function(_dd){return T(function(){return _h(_dd,_cJ);})})(_dd);_dc=_dc;_dd=_dw;_de=_di;continue;var _dx = die("Unreachable!");}else{var _dy = E(_cY);var _dz = _dy[1];var _dA = _dz-5;var _dB = E(_dn);var _dC = _dB[1];var _dD = _dC+20;var _dE = _dA>=_dD;if(_dE){var _dF = (function(_dd){return T(function(){return _h(_dd,_cJ);})})(_dd);_dc=_dc;_dd=_dF;_de=_di;continue;var _dG = die("Unreachable!");}else{var _dH = _dA+10;var _dI = _dC>=_dH;if(_dI){var _dJ = (function(_dd){return T(function(){return _h(_dd,_cJ);})})(_dd);_dc=_dc;_dd=_dJ;_de=_di;continue;var _dK = die("Unreachable!");}else{var _dL = (function(_dd){return T(function(){return _h(_dd,_cM);})})(_dd);var _dM = (function(_d0,_dc,_dp,_dz,_dC){return T(function(){var _dN = T(function(){var _dO = _dz<=_dC;if(_dO){var _dP = T(function(){var _dQ = _dC+20;var _dR = _dz>=_dQ;var _dS = _dR?E(_cI):[1];return _dS;});var _dT = [2,_cE,_dP];}else{var _dU = _dC+20;var _dV = _dz>=_dU;var _dW = _dV?E(_cI):[1];var _dT = _dW;}return _dT;});var _dX = _d0<=_dp;if(_dX){var _dY = T(function(){var _dZ = _dp+60;var _e0 = _d0>=_dZ;var _e1 = _e0?[2,_cF,_dN]:E(_dN);return _e1;});var _e2 = [2,_cG,_dY];var _e3 = _h(_e2,_dc);}else{var _e4 = _dp+60;var _e5 = _d0>=_e4;if(_e5){var _e6 = [2,_cF,_dN];var _e7 = _h(_e6,_dc);}else{var _e7 = _h(_dN,_dc);}var _e3 = _e7;}return _e3;})})(_d0,_dc,_dp,_dz,_dC);_dc=_dM;_dd=_dL;_de=_di;continue;var _dK = die("Unreachable!");}var _dG = _dK;}var _dx = _dG;}var _dt = _dx;}var _dg = _dt;}return _dg;}};var _e8 = T(function(){return _h(_p,_cJ);});var _e9 = _db(_p,_e8,_cT);}else{var _ea = _d1+10;var _eb = _d8>=_ea;if(_eb){var _ec = function(_ed,_ee,_ef){while(1){var _eg = E(_ef);if(_eg[0]==1){var _eh = [1,_ed,_ee];}else{var _ei = _eg[1];var _ej = _eg[2];var _ek = E(_ei);var _el = _ek[1];var _em = E(_el);var _en = _em[1];var _eo = _em[2];var _ep = E(_en);var _eq = _ep[1];var _er = _eq+60;var _es = _d1>=_er;if(_es){var _et = (function(_ee){return T(function(){return _h(_ee,_cJ);})})(_ee);_ed=_ed;_ee=_et;_ef=_ej;continue;var _eu = die("Unreachable!");}else{var _ev = _d1+10;var _ew = _eq>=_ev;if(_ew){var _ex = (function(_ee){return T(function(){return _h(_ee,_cJ);})})(_ee);_ed=_ed;_ee=_ex;_ef=_ej;continue;var _ey = die("Unreachable!");}else{var _ez = E(_cY);var _eA = _ez[1];var _eB = _eA-5;var _eC = E(_eo);var _eD = _eC[1];var _eE = _eD+20;var _eF = _eB>=_eE;if(_eF){var _eG = (function(_ee){return T(function(){return _h(_ee,_cJ);})})(_ee);_ed=_ed;_ee=_eG;_ef=_ej;continue;var _eH = die("Unreachable!");}else{var _eI = _eB+10;var _eJ = _eD>=_eI;if(_eJ){var _eK = (function(_ee){return T(function(){return _h(_ee,_cJ);})})(_ee);_ed=_ed;_ee=_eK;_ef=_ej;continue;var _eL = die("Unreachable!");}else{var _eM = (function(_ee){return T(function(){return _h(_ee,_cM);})})(_ee);var _eN = (function(_d0,_ed,_eq,_eA,_eD){return T(function(){var _eO = T(function(){var _eP = _eA<=_eD;if(_eP){var _eQ = T(function(){var _eR = _eD+20;var _eS = _eA>=_eR;var _eT = _eS?E(_cI):[1];return _eT;});var _eU = [2,_cE,_eQ];}else{var _eV = _eD+20;var _eW = _eA>=_eV;var _eX = _eW?E(_cI):[1];var _eU = _eX;}return _eU;});var _eY = _d0<=_eq;if(_eY){var _eZ = T(function(){var _f0 = _eq+60;var _f1 = _d0>=_f0;var _f2 = _f1?[2,_cF,_eO]:E(_eO);return _f2;});var _f3 = [2,_cG,_eZ];var _f4 = _h(_f3,_ed);}else{var _f5 = _eq+60;var _f6 = _d0>=_f5;if(_f6){var _f7 = [2,_cF,_eO];var _f8 = _h(_f7,_ed);}else{var _f8 = _h(_eO,_ed);}var _f4 = _f8;}return _f4;})})(_d0,_ed,_eq,_eA,_eD);_ed=_eN;_ee=_eM;_ef=_ej;continue;var _eL = die("Unreachable!");}var _eH = _eL;}var _ey = _eH;}var _eu = _ey;}var _eh = _eu;}return _eh;}};var _f9 = T(function(){return _h(_p,_cJ);});var _fa = _ec(_p,_f9,_cT);}else{var _fb = E(_cY);var _fc = _fb[1];var _fd = _fc-5;var _fe = E(_d6);var _ff = _fe[1];var _fg = _ff+20;var _fh = _fd>=_fg;if(_fh){var _fi = function(_fj,_fk,_fl){while(1){var _fm = E(_fl);if(_fm[0]==1){var _fn = [1,_fj,_fk];}else{var _fo = _fm[1];var _fp = _fm[2];var _fq = E(_fo);var _fr = _fq[1];var _fs = E(_fr);var _ft = _fs[1];var _fu = _fs[2];var _fv = E(_ft);var _fw = _fv[1];var _fx = _fw+60;var _fy = _d1>=_fx;if(_fy){var _fz = (function(_fk){return T(function(){return _h(_fk,_cJ);})})(_fk);_fj=_fj;_fk=_fz;_fl=_fp;continue;var _fA = die("Unreachable!");}else{var _fB = _d1+10;var _fC = _fw>=_fB;if(_fC){var _fD = (function(_fk){return T(function(){return _h(_fk,_cJ);})})(_fk);_fj=_fj;_fk=_fD;_fl=_fp;continue;var _fE = die("Unreachable!");}else{var _fF = E(_fu);var _fG = _fF[1];var _fH = _fG+20;var _fI = _fd>=_fH;if(_fI){var _fJ = (function(_fk){return T(function(){return _h(_fk,_cJ);})})(_fk);_fj=_fj;_fk=_fJ;_fl=_fp;continue;var _fK = die("Unreachable!");}else{var _fL = _fd+10;var _fM = _fG>=_fL;if(_fM){var _fN = (function(_fk){return T(function(){return _h(_fk,_cJ);})})(_fk);_fj=_fj;_fk=_fN;_fl=_fp;continue;var _fO = die("Unreachable!");}else{var _fP = (function(_fk){return T(function(){return _h(_fk,_cM);})})(_fk);var _fQ = (function(_d0,_fc,_fj,_fw,_fG){return T(function(){var _fR = T(function(){var _fS = _fc<=_fG;if(_fS){var _fT = T(function(){var _fU = _fG+20;var _fV = _fc>=_fU;var _fW = _fV?E(_cI):[1];return _fW;});var _fX = [2,_cE,_fT];}else{var _fY = _fG+20;var _fZ = _fc>=_fY;var _g0 = _fZ?E(_cI):[1];var _fX = _g0;}return _fX;});var _g1 = _d0<=_fw;if(_g1){var _g2 = T(function(){var _g3 = _fw+60;var _g4 = _d0>=_g3;var _g5 = _g4?[2,_cF,_fR]:E(_fR);return _g5;});var _g6 = [2,_cG,_g2];var _g7 = _h(_g6,_fj);}else{var _g8 = _fw+60;var _g9 = _d0>=_g8;if(_g9){var _ga = [2,_cF,_fR];var _gb = _h(_ga,_fj);}else{var _gb = _h(_fR,_fj);}var _g7 = _gb;}return _g7;})})(_d0,_fc,_fj,_fw,_fG);_fj=_fQ;_fk=_fP;_fl=_fp;continue;var _fO = die("Unreachable!");}var _fK = _fO;}var _fE = _fK;}var _fA = _fE;}var _fn = _fA;}return _fn;}};var _gc = T(function(){return _h(_p,_cJ);});var _gd = _fi(_p,_gc,_cT);}else{var _ge = _fd+10;var _gf = _ff>=_ge;if(_gf){var _gg = function(_gh,_gi,_gj){while(1){var _gk = E(_gj);if(_gk[0]==1){var _gl = [1,_gh,_gi];}else{var _gm = _gk[1];var _gn = _gk[2];var _go = E(_gm);var _gp = _go[1];var _gq = E(_gp);var _gr = _gq[1];var _gs = _gq[2];var _gt = E(_gr);var _gu = _gt[1];var _gv = _gu+60;var _gw = _d1>=_gv;if(_gw){var _gx = (function(_gi){return T(function(){return _h(_gi,_cJ);})})(_gi);_gh=_gh;_gi=_gx;_gj=_gn;continue;var _gy = die("Unreachable!");}else{var _gz = _d1+10;var _gA = _gu>=_gz;if(_gA){var _gB = (function(_gi){return T(function(){return _h(_gi,_cJ);})})(_gi);_gh=_gh;_gi=_gB;_gj=_gn;continue;var _gC = die("Unreachable!");}else{var _gD = E(_gs);var _gE = _gD[1];var _gF = _gE+20;var _gG = _fd>=_gF;if(_gG){var _gH = (function(_gi){return T(function(){return _h(_gi,_cJ);})})(_gi);_gh=_gh;_gi=_gH;_gj=_gn;continue;var _gI = die("Unreachable!");}else{var _gJ = _fd+10;var _gK = _gE>=_gJ;if(_gK){var _gL = (function(_gi){return T(function(){return _h(_gi,_cJ);})})(_gi);_gh=_gh;_gi=_gL;_gj=_gn;continue;var _gM = die("Unreachable!");}else{var _gN = (function(_gi){return T(function(){return _h(_gi,_cM);})})(_gi);var _gO = (function(_d0,_fc,_gh,_gu,_gE){return T(function(){var _gP = T(function(){var _gQ = _fc<=_gE;if(_gQ){var _gR = T(function(){var _gS = _gE+20;var _gT = _fc>=_gS;var _gU = _gT?E(_cI):[1];return _gU;});var _gV = [2,_cE,_gR];}else{var _gW = _gE+20;var _gX = _fc>=_gW;var _gY = _gX?E(_cI):[1];var _gV = _gY;}return _gV;});var _gZ = _d0<=_gu;if(_gZ){var _h0 = T(function(){var _h1 = _gu+60;var _h2 = _d0>=_h1;var _h3 = _h2?[2,_cF,_gP]:E(_gP);return _h3;});var _h4 = [2,_cG,_h0];var _h5 = _h(_h4,_gh);}else{var _h6 = _gu+60;var _h7 = _d0>=_h6;if(_h7){var _h8 = [2,_cF,_gP];var _h9 = _h(_h8,_gh);}else{var _h9 = _h(_gP,_gh);}var _h5 = _h9;}return _h5;})})(_d0,_fc,_gh,_gu,_gE);_gh=_gO;_gi=_gN;_gj=_gn;continue;var _gM = die("Unreachable!");}var _gI = _gM;}var _gC = _gI;}var _gy = _gC;}var _gl = _gy;}return _gl;}};var _ha = T(function(){return _h(_p,_cJ);});var _hb = _gg(_p,_ha,_cT);}else{var _hc = function(_hd,_he,_hf){while(1){var _hg = E(_hf);if(_hg[0]==1){var _hh = [1,_hd,_he];}else{var _hi = _hg[1];var _hj = _hg[2];var _hk = E(_hi);var _hl = _hk[1];var _hm = E(_hl);var _hn = _hm[1];var _ho = _hm[2];var _hp = E(_hn);var _hq = _hp[1];var _hr = _hq+60;var _hs = _d1>=_hr;if(_hs){var _ht = (function(_he){return T(function(){return _h(_he,_cJ);})})(_he);_hd=_hd;_he=_ht;_hf=_hj;continue;var _hu = die("Unreachable!");}else{var _hv = _d1+10;var _hw = _hq>=_hv;if(_hw){var _hx = (function(_he){return T(function(){return _h(_he,_cJ);})})(_he);_hd=_hd;_he=_hx;_hf=_hj;continue;var _hy = die("Unreachable!");}else{var _hz = E(_ho);var _hA = _hz[1];var _hB = _hA+20;var _hC = _fd>=_hB;if(_hC){var _hD = (function(_he){return T(function(){return _h(_he,_cJ);})})(_he);_hd=_hd;_he=_hD;_hf=_hj;continue;var _hE = die("Unreachable!");}else{var _hF = _fd+10;var _hG = _hA>=_hF;if(_hG){var _hH = (function(_he){return T(function(){return _h(_he,_cJ);})})(_he);_hd=_hd;_he=_hH;_hf=_hj;continue;var _hI = die("Unreachable!");}else{var _hJ = (function(_he){return T(function(){return _h(_he,_cM);})})(_he);var _hK = (function(_d0,_fc,_hd,_hq,_hA){return T(function(){var _hL = T(function(){var _hM = _fc<=_hA;if(_hM){var _hN = T(function(){var _hO = _hA+20;var _hP = _fc>=_hO;var _hQ = _hP?E(_cI):[1];return _hQ;});var _hR = [2,_cE,_hN];}else{var _hS = _hA+20;var _hT = _fc>=_hS;var _hU = _hT?E(_cI):[1];var _hR = _hU;}return _hR;});var _hV = _d0<=_hq;if(_hV){var _hW = T(function(){var _hX = _hq+60;var _hY = _d0>=_hX;var _hZ = _hY?[2,_cF,_hL]:E(_hL);return _hZ;});var _i0 = [2,_cG,_hW];var _i1 = _h(_i0,_hd);}else{var _i2 = _hq+60;var _i3 = _d0>=_i2;if(_i3){var _i4 = [2,_cF,_hL];var _i5 = _h(_i4,_hd);}else{var _i5 = _h(_hL,_hd);}var _i1 = _i5;}return _i1;})})(_d0,_fc,_hd,_hq,_hA);_hd=_hK;_he=_hJ;_hf=_hj;continue;var _hI = die("Unreachable!");}var _hE = _hI;}var _hy = _hE;}var _hu = _hy;}var _hh = _hu;}return _hh;}};var _i6 = T(function(){return _h(_p,_cM);});var _i7 = T(function(){var _i8 = T(function(){var _i9 = _fc<=_ff;if(_i9){var _ia = T(function(){var _ib = _ff+20;var _ic = _fc>=_ib;var _id = _ic?E(_cI):[1];return _id;});var _ie = [2,_cE,_ia];}else{var _if = _ff+20;var _ig = _fc>=_if;var _ih = _ig?E(_cI):[1];var _ie = _ih;}return _ie;});var _ii = _d0<=_d8;if(_ii){var _ij = T(function(){var _ik = _d8+60;var _il = _d0>=_ik;var _im = _il?[2,_cF,_i8]:E(_i8);return _im;});var _in = [2,_cG,_ij];var _io = _h(_in,_p);}else{var _ip = _d8+60;var _iq = _d0>=_ip;if(_iq){var _ir = [2,_cF,_i8];var _is = _h(_ir,_p);}else{var _is = _h(_i8,_p);}var _io = _is;}return _io;});var _hb = _hc(_i7,_i6,_cT);}var _gd = _hb;}var _fa = _gd;}var _e9 = _fa;}var _cR = _e9;}return _cR;};var _it = function(_iu,_iv,_iw){var _ix = _iv-5;var _iy = _ix+10;var _iz = _iu>=_iy;if(_iz){var _iA = [1];}else{var _iB = _iu+40;var _iC = _ix>=_iB;if(_iC){var _iD = [1];}else{var _iE = E(_iw);var _iF = _iE[1];var _iG = _iF-5;var _iH = _iG+10;var _iI = 385>=_iH;if(_iI){var _iJ = [1];}else{var _iK = _iG>=400;if(_iK){var _iL = [1];}else{var _iM = T(function(){var _iN = _iF<=385;if(_iN){var _iO = T(function(){var _iP = _iF>=400;return _iP?E(_cI):[1];});var _iQ = [2,_cE,_iO];}else{var _iR = _iF>=400;var _iQ = _iR?E(_cI):[1];}return _iQ;});var _iS = _iv<=_iu;if(_iS){var _iT = T(function(){var _iU = _iu+40;var _iV = _iv>=_iU;var _iW = _iV?[2,_cF,_iM]:E(_iM);return _iW;});var _iX = [2,_cG,_iT];}else{var _iY = _iu+40;var _iZ = _iv>=_iY;var _j0 = _iZ?[2,_cF,_iM]:E(_iM);var _iX = _j0;}var _iL = _iX;}var _iJ = _iL;}var _iD = _iJ;}var _iA = _iD;}return _iA;};var _j1 = function(_j2){var _j3 = E(_j2);var _j4 = _j3[1];var _j5 = _j3[2];var _j6 = T(function(){var _j7 = E(_j5);var _j8 = _j7[1];var _j9 = E(_j8);return _j9;});var _ja = T(function(){var _jb = E(_j5);var _jc = _jb[2];var _jd = _cN(_j6,_jc);var _je = _jd[1];var _jf = _jd[2];var _jg = [1,_je,_jf];return _jg;});var _jh = T(function(){var _ji = E(_ja);var _jj = _ji[2];var _jk = E(_jj);return _jk;});var _jl = T(function(){var _jm = E(_j6);var _jn = _jm[1];var _jo = E(_jn);var _jp = _jo[1];var _jq = _jo[2];var _jr = E(_jp);var _js = _jr[1];var _jt = T(function(){var _ju = _js>595;if(_ju){var _jv = T(function(){var _jw = E(_jq);var _jx = _jw[1];var _jy = _jx<5;var _jz = _jy?E(_cI):[1];return _jz;});var _jA = [2,_cG,_jv];}else{var _jB = E(_jq);var _jC = _jB[1];var _jD = _jC<5;var _jE = _jD?E(_cI):[1];var _jA = _jE;}return _jA;});var _jF = T(function(){var _jG = E(_j4);var _jH = _jG[1];var _jI = E(_jH);var _jJ = _jI[1];var _jK = T(function(){var _jL = E(_ja);var _jM = _jL[1];var _jN = E(_jM);return _jN;});var _jO = _it(_jJ,_js,_jq);var _jP = _h(_jO,_jK);return _jP;});var _jQ = _js<5;if(_jQ){var _jR = [2,_cF,_jt];var _jS = _h(_jR,_jF);}else{var _jS = _h(_jt,_jF);}return _jS;});var _jT = [1,_jl,_jh];return _jT;};var _jU = T(function(){return A(_9l,[_9j,_j1]);});var _jV = function(_jW){return E(_jW);};var _jX = T(function(){return A(_9l,[_9j,_jV]);});var _jY = function(_jZ,_k0,_k1){var _k2 = T(function(){return A(_jZ,[_k0,_k1]);});var _k3 = T(function(){return A(_k4,[_jZ,_k2]);});return [1,_k2,_k3];};var _k4 = function(_k5,_k6,_k7){var _k8 = _jY(_k5,_k6,_k7);var _k9 = _k8[1];var _ka = _k8[2];var _kb = [1,_k9,_ka];return _kb;};var _kc = function(_kd,_g){return A(_k4,[_kd,_g]);};var _ke = [1,350];var _kf = [1,300];var _kg = [1,_kf,_ke];var _kh = function(_ki,_kj){var _kk = E(_ki);var _kl = _kk[1];var _km = E(_kj);var _kn = _km[1];var _ko = _kl+_kn;var _kp = [1,_ko];return _kp;};var _kq = function(_kr,_ks){var _kt = E(_kr);var _ku = _kt[1];var _kv = _kt[2];var _kw = E(_ks);var _kx = _kw[1];var _ky = _kw[2];var _kz = T(function(){return _kh(_kv,_ky);});var _kA = T(function(){return _kh(_ku,_kx);});var _kB = [1,_kA,_kz];return _kB;};var _kC = T(function(){return A(_kc,[_kq,_kg]);});var _kD = T(function(){return A(_6L,[_9k,_kC,_jX]);});var _kE = T(function(){return A(_9t,[_9j,_kD]);});var _kF = function(_kG){return [1,_kG,_0];};var _kH = T(function(){return A(_9l,[_9j,_kF]);});var _kI = function(_kJ){var _kK = E(_kJ);var _kL = _kK[1];var _kM = _kK[2];var _kN = E(_kM);var _kO = E(_kL);return _kO;};var _kP = T(function(){return A(_9l,[_9j,_kI]);});var _kQ = function(_kR){return [1,_kR,_0];};var _kS = T(function(){return A(_9l,[_9j,_kQ]);});var _kT = function(_kU){return E(_kU);};var _kV = T(function(){return A(_9l,[_9j,_kT]);});var _kW = function(_kX,_kY,_kZ){var _l0 = function(_l1,_l2){while(1){var _l3 = E(_l2);if(_l3[0]==1){var _l4 = E(_l1);}else{var _l5 = _l3[1];var _l6 = _l3[2];var _l7 = A(_kX,[_l1,_l5]);_l1=_l7;_l2=_l6;continue;var _l8 = die("Unreachable!");var _l4 = _l8;}return _l4;}};return _l0(_kY,_kZ);};var _l9 = function(_la,_lb,_lc){var _ld = T(function(){return _kW(_la,_lb,_lc);});var _le = T(function(){return A(_lf,[_la,_ld]);});return [1,_ld,_le];};var _lf = function(_lg,_lh,_li){var _lj = _l9(_lg,_lh,_li);var _lk = _lj[1];var _ll = _lj[2];var _lm = [1,_lk,_ll];return _lm;};var _ln = function(_kd,_g){return A(_lf,[_kd,_g]);};var _lo = function(_lp){var _lq = E(_lp);var _lr = _lq[1];var _ls = _lr>=0;if(_ls){var _lt = E(_lq);}else{var _lu = -_lr;var _lv = [1,_lu];var _lt = _lv;}return _lt;};var _lw = function(_lx,_ly){var _lz = E(_lx);var _lA = _lz[1];var _lB = _lz[2];var _lC = E(_ly);switch(_lC[0]){case 1:var _lD = T(function(){return _lo(_lA);});var _lE = [1,_lD,_lB];break;case 2:var _lF = T(function(){var _lG = E(_lA);var _lH = _lG[1];var _lI = _lH>=0;if(_lI){var _lJ = -_lH;var _lK = [1,_lJ];var _lL = _lK;}else{var _lM = -_lH;var _lN = -_lM;var _lO = [1,_lN];var _lL = _lO;}return _lL;});var _lE = [1,_lF,_lB];break;case 3:var _lP = T(function(){return _lo(_lB);});var _lE = [1,_lA,_lP];break;case 4:var _lQ = T(function(){var _lR = E(_lB);var _lS = _lR[1];var _lT = _lS>=0;if(_lT){var _lU = -_lS;var _lV = [1,_lU];var _lW = _lV;}else{var _lX = -_lS;var _lY = -_lX;var _lZ = [1,_lY];var _lW = _lZ;}return _lW;});var _lE = [1,_lA,_lQ];break;}return _lE;};var _m0 = [1,(-3)];var _m1 = [1,3];var _m2 = [1,_m1,_m0];var _m3 = T(function(){return A(_ln,[_lw,_m2]);});var _m4 = T(function(){return A(_6L,[_9k,_m3,_kV]);});var _m5 = T(function(){return A(_9t,[_9j,_m4]);});var _m6 = T(function(){return A(_6L,[_9k,_m5,_kS]);});var _m7 = T(function(){return A(_6L,[_9k,_kP,_m6]);});var _m8 = T(function(){return A(_6L,[_9k,_kH,_m7]);});var _m9 = T(function(){return A(_6L,[_9k,_kE,_m8]);});var _ma = T(function(){return A(_9l,[_9j,_kI]);});var _mb = T(function(){return A(_6L,[_9k,_ma,_m9]);});var _mc = function(_md){return [1,_md];};var _me = T(function(){return A(_9l,[_9j,_mc]);});var _mf = T(function(){return A(_6L,[_9k,_me,_mb]);});var _mg = T(function(){return A(_9l,[_9j,_kT]);});var _mh = T(function(){return A(_6L,[_9k,_mf,_mg]);});var _mi = T(function(){return A(_9t,[_9j,_mh]);});var _mj = function(_mk){var _ml = E(_mk);var _mm = _ml[1];var _mn = _ml[2];var _mo = [1,_mn,_mm];return _mo;};var _mp = T(function(){return A(_9l,[_9j,_mj]);});var _mq = function(_mr){return E(_mr);};var _ms = T(function(){return A(_9l,[_9j,_mq]);});var _mt = [1,_p,_p];var _mu = function(_mv,_mw,_mx){var _my = A(_mv,[_mw]);var _mz = _my[1];var _mA = _my[2];var _mB = E(_mz);if(_mB[0]==1){var _mC = E(_mx);}else{var _mD = T(function(){var _mE = E(_mx);var _mF = _mE[2];var _mG = E(_mF);return _mG;});var _mH = [2,_mA,_mD];var _mI = T(function(){var _mJ = E(_mx);var _mK = _mJ[1];var _mL = E(_mK);return _mL;});var _mM = [2,_mB,_mI];var _mC = [1,_mM,_mH];}return _mC;};var _mN = function(_mO){while(1){var _mP = E(_mO);if(_mP[0]==1){var _mQ = [1];}else{var _mR = _mP[1];var _mS = _mP[2];var _mT = E(_mR);if(_mT[0]==1){_mO=_mS;continue;var _mU = die("Unreachable!");}else{var _mV = _mT[1];var _mW = (function(_mS){return T(function(){return _mN(_mS);})})(_mS);var _mU = [2,_mV,_mW];}var _mQ = _mU;}return _mQ;}};var _mX = function(_mY,_mZ,_n0,_n1){var _n2 = E(_n0);if(_n2[0]==1){var _n3 = E(_mZ);}else{var _n4 = _n2[1];var _n5 = _n2[2];var _n6 = E(_n1);if(_n6[0]==1){var _n7 = E(_mZ);}else{var _n8 = _n6[1];var _n9 = _n6[2];var _na = T(function(){return _mX(_mY,_mZ,_n5,_n9);});var _n7 = A(_mY,[_n4,_n8,_na]);}var _n3 = _n7;}return _n3;};var _nb = function(_nc,_nd){var _ne = T(function(){return _mX(_mu,_mt,_nc,_nd);});var _nf = T(function(){var _ng = E(_ne);var _nh = _ng[2];var _ni = E(_nh);return _ni;});var _nj = function(_nk){var _nl = _nb(_nf,_nk);var _nm = _nl[1];var _nn = _nl[2];var _no = [1,_nm,_nn];return _no;};var _np = T(function(){var _nq = E(_ne);var _nr = _nq[1];var _ns = _mN(_nr);return _ns;});return [1,_np,_nj];};var _nt = function(_nu,_nv){var _nw = _nb(_nu,_nv);var _nx = _nw[1];var _ny = _nw[2];var _nz = [1,_nx,_ny];return _nz;};var _nA = function(_g){return A(_nt,[_g]);};var _nB = [1,1];var _nC = function(_nD,_nE){var _nF = E(_nD);if(_nF[0]==1){var _nG = [1];}else{var _nH = _nF[1];var _nI = E(_nH);var _nJ = _nI[1];var _nK = _nI[2];var _nL = E(_nK);var _nM = _nL[1];var _nN = E(_nM);if(_nN==1){var _nO = [1];}else{var _nP = [1,_nJ,_nB];var _nO = [2,_nP];}var _nG = _nO;}return _nG;};var _nQ = function(_nR){var _nS = [2,_nR];return A(_ln,[_nC,_nS]);};var _nT = [1,140];var _nU = [1,240];var _nV = [1,520];var _nW = [2,_nV,_p];var _nX = [1,440];var _nY = [2,_nX,_nW];var _nZ = [1,340];var _o0 = [2,_nZ,_nY];var _o1 = [2,_nU,_o0];var _o2 = [2,_nT,_o1];var _o3 = [2,_2R,_o2];var _o4 = [1,2];var _o5 = [1,260];var _o6 = [1,_o5,_nB];var _o7 = [2,_o6,_p];var _o8 = [1,220];var _o9 = [1,_o8,_nB];var _oa = [2,_o9,_o7];var _ob = [1,180];var _oc = [1,_ob,_nB];var _od = [2,_oc,_oa];var _oe = [1,_nT,_o4];var _of = [2,_oe,_od];var _og = [1,100];var _oh = [1,_og,_nB];var _oi = [2,_oh,_of];var _oj = function(_ok){var _ol = E(_ok);if(_ol[0]==1){var _om = [1];}else{var _on = _ol[1];var _oo = _ol[2];var _op = T(function(){return _oj(_oo);});var _oq = function(_or,_os,_ot){var _ou = T(function(){return _ov(_ot);});var _ow = [1,_on,_or];var _ox = [1,_ow,_os];return [2,_ox,_ou];};var _ov = function(_oy){var _oz = E(_oy);if(_oz[0]==1){var _oA = E(_op);}else{var _oB = _oz[1];var _oC = _oz[2];var _oD = E(_oB);var _oE = _oD[1];var _oF = _oD[2];var _oG = T(function(){return _ov(_oC);});var _oH = [1,_on,_oE];var _oI = [1,_oH,_oF];var _oJ = [2,_oI,_oG];var _oA = _oJ;}return _oA;};var _om = _oq(_4S,_o4,_oi);}return _om;};var _oK = T(function(){return _oj(_o3);});var _oL = T(function(){return _c6(_nQ,_oK);});var _oM = T(function(){return A(_nA,[_oL]);});var _oN = function(_oO){return E(_oO);};var _oP = T(function(){return A(_9l,[_9j,_oN]);});var _oQ = T(function(){return A(_6L,[_9k,_oM,_oP]);});var _oR = T(function(){return A(_9t,[_9j,_oQ]);});var _oS = function(_oT){var _oU = E(_oT);var _oV = _oU[1];var _oW = _oU[2];var _oX = [1,_oW,_oV];return _oX;};var _oY = T(function(){return A(_9l,[_9j,_oS]);});var _oZ = function(_p0){var _p1 = E(_p0);var _p2 = _p1[1];var _p3 = [1,_p2,_p1];return _p3;};var _p4 = T(function(){return A(_9l,[_9j,_oZ]);});var _p5 = function(_p6){return E(_p6);};var _p7 = T(function(){return A(_9l,[_9j,_p5]);});var _p8 = [1,_kg];var _p9 = T(function(){return A(_bR,[_p8]);});var _pa = T(function(){return A(_6L,[_9k,_p9,_p7]);});var _pb = T(function(){return A(_9t,[_9j,_pa]);});var _pc = function(_pd){var _pe = E(_pd);var _pf = _pe[1];var _pg = _pe[2];var _ph = E(_pg);var _pi = _ph[1];var _pj = _ph[2];var _pk = [1,_pi,_pj,_pf];return _pk;};var _pl = T(function(){return A(_9l,[_9j,_pc]);});var _pm = function(_pn){var _po = E(_pn);var _pp = _po[2];var _pq = [1,_pp,_po];return _pq;};var _pr = T(function(){return A(_9l,[_9j,_pm]);});var _ps = function(_pt){return E(_pt);};var _pu = T(function(){return A(_9l,[_9j,_ps]);});var _pv = T(function(){return A(_bR,[_oK]);});var _pw = T(function(){return A(_6L,[_9k,_pv,_pu]);});var _px = T(function(){return A(_9t,[_9j,_pw]);});var _py = function(_pz){var _pA = E(_pz);var _pB = _pA[1];var _pC = _pA[2];var _pD = E(_pC);var _pE = _pD[1];var _pF = _pD[2];var _pG = _pD[3];var _pH = [1,_pE,_pF,_pG,_pB];return _pH;};var _pI = T(function(){return A(_9l,[_9j,_py]);});var _pJ = function(_pK){var _pL = E(_pK);var _pM = _pL[1];var _pN = _pL[2];var _pO = _pL[3];var _pP = _pL[4];var _pQ = [1,_pO,_pP];var _pR = [1,_pM,_pN];var _pS = [1,_pR,_pQ];return _pS;};var _pT = T(function(){return A(_9l,[_9j,_pJ]);});var _pU = T(function(){var _pV = E(_9k);var _pW = _pV[2];var _pX = T(function(){var _pY = T(function(){var _pZ = T(function(){var _q0 = T(function(){var _q1 = T(function(){var _q2 = T(function(){var _q3 = T(function(){var _q4 = T(function(){var _q5 = T(function(){var _q6 = T(function(){var _q7 = T(function(){return A(_pW,[_mi,_jU]);});return A(_pW,[_mp,_q7]);});return A(_pW,[_ms,_q6]);});return A(_pW,[_oR,_q5]);});return A(_pW,[_oY,_q4]);});return A(_pW,[_p4,_q3]);});return A(_pW,[_pb,_q2]);});return A(_pW,[_pl,_q1]);});return A(_pW,[_pr,_q0]);});return A(_pW,[_px,_pZ]);});return A(_pW,[_pI,_pY]);});var _q8 = A(_pW,[_pT,_pX]);return _q8;});var _q9 = T(function(){return A(_9y,[_ae,_pU]);});var _qa = T(function(){return A(_9t,[_9j,_q9]);});var _qb = function(_qc){return [1,_qc,_qc];};var _qd = T(function(){return A(_9l,[_9j,_qb]);});var _qe = function(_qf){var _qg = E(_qf);var _qh = _qg[1];var _qi = _qg[2];var _qj = E(_qi);var _qk = E(_qh);return _qk;};var _ql = T(function(){return A(_9l,[_9j,_qe]);});var _qm = T(function(){return A(_9l,[_9j,_9q]);});var _qn = function(_qo){return E(_qo);};var _qp = T(function(){return A(_9l,[_9j,_qn]);});var _qq = function(_qr){return E(_qr);};var _qs = T(function(){return A(_9l,[_9j,_qq]);});var _qt = function(_qu){var _qv = E(_qu);var _qw = _qv[7];var _qx = E(_qw);return _qx;};var _qy = function(_qz){var _qA = E(_qz);var _qB = _qA[8];var _qC = E(_qB);return _qC;};var _qD = function(_qE){var _qF = E(_qE);var _qG = _qF[1];var _qH = E(_qG);return _qH;};var _qI = function(_qJ,_qK,_qL,_qM,_qN){var _qO = T(function(){return A(_qy,[_qK,_qM]);});var _qP = T(function(){return A(_qt,[_qK,_qL]);});var _qQ = function(_qR,_qS){var _qT = T(function(){var _qU = T(function(){return A(_qD,[_qJ,_qR,_qS]);});return A(_qP,[_qU]);});return A(_qO,[_qT]);};var _qV = function(_qW){var _qX = _jY(_qQ,_qN,_qW);var _qY = _qX[1];var _qZ = _qX[2];var _r0 = [1,_qY,_qZ];return _r0;};return E(_qV);};var _r1 = function(_r2,_r3,_r4,_r5){var _r6 = E(_r4);var _r7 = _r6[1];var _r8 = _r6[2];var _r9 = _qI(_r2,_r3,_r7,_r8,_r5);return _r9;};var _ra = function(_rb,_rc){var _rd = E(_rb);var _re = _rd[1];var _rf = E(_rc);var _rg = _rf[1];var _rh = _re==_rg;var _ri = _rh?false:true;return _ri;};var _rj = function(_rk,_rl){var _rm = E(_rk);var _rn = _rm[1];var _ro = E(_rl);var _rp = _ro[1];var _rq = _rn==_rp;return _rq;};var _rr = [1,_rj,_ra];var _rs = function(_rt,_ru){var _rv = E(_rt);var _rw = _rv[1];var _rx = E(_ru);var _ry = _rx[1];var _rz = _rw<_ry;return _rz;};var _rA = function(_rB,_rC){var _rD = E(_rB);var _rE = _rD[1];var _rF = E(_rC);var _rG = _rF[1];var _rH = _rE<=_rG;return _rH;};var _rI = function(_rJ,_rK){var _rL = E(_rJ);var _rM = _rL[1];var _rN = E(_rK);var _rO = _rN[1];var _rP = _rM>_rO;return _rP;};var _rQ = function(_rR,_rS){var _rT = E(_rR);var _rU = _rT[1];var _rV = E(_rS);var _rW = _rV[1];var _rX = _rU>=_rW;return _rX;};var _rY = function(_rZ,_s0){var _s1 = E(_rZ);var _s2 = _s1[1];var _s3 = E(_s0);var _s4 = _s3[1];var _s5 = _s2<_s4;if(_s5){var _s6 = [1];}else{var _s7 = _s2==_s4;var _s6 = _s7?[2]:[3];}return _s6;};var _s8 = function(_s9,_sa){var _sb = E(_s9);var _sc = _sb[1];var _sd = E(_sa);var _se = _sd[1];var _sf = _sc<=_se;var _sg = _sf?E(_sd):E(_sb);return _sg;};var _sh = function(_si,_sj){var _sk = E(_si);var _sl = _sk[1];var _sm = E(_sj);var _sn = _sm[1];var _so = _sl<=_sn;var _sp = _so?E(_sk):E(_sm);return _sp;};var _sq = [1,_rr,_rY,_rs,_rQ,_rI,_rA,_s8,_sh];var _sr = function(_ss){var _st = E(_ss);if(_st[0]==1){var _su = _st[1];var _sv = _st[2];var _sw = _sr(_sv);var _sx = (_su&65535)>>>0;var _sy = _sx&4294967295;var _sz = _sy;var _sA = Math.pow(2,16);var _sB = _su>>>16;var _sC = _sB&4294967295;var _sD = _sC;var _sE = _sD*_sA;var _sF = Math.pow(2,32);var _sG = _sw*_sF;var _sH = _sG+_sE;var _sI = _sH+_sz;var _sJ = _sI;}else{var _sJ = 0;}return _sJ;};var _sK = function(_sL){var _sM = E(_sL);switch(_sM[0]){case 1:var _sN = _sM[1];var _sO = _sr(_sN);break;case 2:var _sP = _sM[1];var _sQ = _sr(_sP);var _sR = -_sQ;var _sO = _sR;break;case 3:var _sO = 0;break;}return _sO;};var _sS = function(_sT){var _sU = _sK(_sT);var _sV = [1,_sU];return _sV;};var _sW = [1,0];var _sX = [1,1];var _sY = [1,(-1)];var _sZ = function(_t0){var _t1 = E(_t0);var _t2 = _t1[1];var _t3 = _t2==0;if(_t3){var _t4 = E(_sW);}else{var _t5 = _t2>0;var _t4 = _t5?E(_sX):E(_sY);}return _t4;};var _t6 = function(_t7,_t8){var _t9 = E(_t7);var _ta = _t9[1];var _tb = E(_t8);var _tc = _tb[1];var _td = _ta-_tc;var _te = [1,_td];return _te;};var _tf = function(_tg){var _th = E(_tg);var _ti = _th[1];var _tj = -_ti;var _tk = [1,_tj];return _tk;};var _tl = function(_tm,_tn){var _to = E(_tm);var _tp = _to[1];var _tq = E(_tn);var _tr = _tq[1];var _ts = _tp*_tr;var _tt = [1,_ts];return _tt;};var _tu = [1,_kh,_tl,_t6,_tf,_lo,_sZ,_sS];var _tv = [1,280];var _tw = [1,560];var _tx = [1,0];var _ty = [1,_tx,_tw];var _tz = T(function(){return A(_r1,[_tu,_sq,_ty,_tv]);});var _tA = T(function(){return A(_6L,[_9k,_tz,_qs]);});var _tB = T(function(){return A(_9t,[_9j,_tA]);});var _tC = function(_tD){return [1,_tD,_0];};var _tE = T(function(){return A(_9l,[_9j,_tC]);});var _tF = function(_tG){var _tH = E(_tG);var _tI = _tH[1];var _tJ = _tH[2];var _tK = E(_tJ);var _tL = E(_tI);return _tL;};var _tM = T(function(){return A(_9l,[_9j,_tF]);});var _tN = T(function(){return A(_9l,[_9j,_9q]);});var _tO = T(function(){return A(_9l,[_9j,_qn]);});var _tP = T(function(){return A(_9l,[_9j,_qn]);});var _tQ = false;var _tR = function(_tS){var _tT = function(_tU,_tV){var _tW = E(_tV);if(_tW[0]==1){var _tX = _tW[1];var _tY = E(_tX);var _tZ = _tY[1];var _u0 = E(_tS);var _u1 = _u0[1];var _u2 = _tZ==_u1;var _u3 = _u2?false:E(_tU);var _u4 = _u3;}else{var _u5 = _tW[1];var _u6 = E(_u5);var _u7 = _u6[1];var _u8 = E(_tS);var _u9 = _u8[1];var _ua = _u7==_u9;var _ub = _ua?true:E(_tU);var _u4 = _ub;}return _u4;};return A(_ln,[_tT,_tQ]);};var _uc = [1,39];var _ud = T(function(){return _tR(_uc);});var _ue = T(function(){return A(_6L,[_9k,_ud,_tP]);});var _uf = T(function(){return A(_9t,[_9j,_ue]);});var _ug = function(_uh){return E(_uh);};var _ui = T(function(){return A(_9l,[_9j,_ug]);});var _uj = function(_uk){var _ul = E(_uk);var _um = _ul[1];var _un = _ul[2];var _uo = [1,_un,_um];return _uo;};var _up = T(function(){return A(_9l,[_9j,_uj]);});var _uq = function(_ur){return [1,_ur,_ur];};var _us = T(function(){return A(_9l,[_9j,_uq]);});var _ut = T(function(){return A(_9l,[_9j,_qn]);});var _uu = [1,37];var _uv = T(function(){return _tR(_uu);});var _uw = T(function(){return A(_6L,[_9k,_uv,_ut]);});var _ux = T(function(){return A(_9t,[_9j,_uw]);});var _uy = T(function(){return A(_6L,[_9k,_ux,_us]);});var _uz = T(function(){return A(_6L,[_9k,_up,_uy]);});var _uA = T(function(){return A(_6L,[_9k,_ui,_uz]);});var _uB = T(function(){return A(_6L,[_9k,_uf,_uA]);});var _uC = function(_uD){var _uE = E(_uD);var _uF = _uE[1];var _uG = _uE[2];var _uH = [1,_uG,_uF];return _uH;};var _uI = T(function(){return A(_9l,[_9j,_uC]);});var _uJ = T(function(){return A(_6L,[_9k,_uI,_uB]);});var _uK = [1,(-5)];var _uL = function(_uM){var _uN = E(_uM);var _uO = _uN[1];var _uP = _uN[2];var _uQ = E(_uO);if(_uQ){var _uR = E(_uK);}else{var _uS = E(_uP);var _uR = _uS?E(_4P):E(_tx);}return _uR;};var _uT = T(function(){return A(_9l,[_9j,_uL]);});var _uU = T(function(){return A(_6L,[_9k,_uT,_uJ]);});var _uV = T(function(){return A(_6L,[_9k,_uU,_tO]);});var _uW = T(function(){return A(_9t,[_9j,_uV]);});var _uX = T(function(){return A(_6L,[_9k,_uW,_tN]);});var _uY = T(function(){return A(_6L,[_9k,_tM,_uX]);});var _uZ = T(function(){return A(_6L,[_9k,_tE,_uY]);});var _v0 = T(function(){return A(_6L,[_9k,_tB,_uZ]);});var _v1 = T(function(){return A(_9l,[_9j,_tF]);});var _v2 = T(function(){return A(_6L,[_9k,_v1,_v0]);});var _v3 = function(_md){return [1,_md];};var _v4 = T(function(){return A(_9l,[_9j,_v3]);});var _v5 = T(function(){return A(_6L,[_9k,_v4,_v2]);});var _v6 = T(function(){return A(_6L,[_9k,_v5,_qp]);});var _v7 = T(function(){return A(_9t,[_9j,_v6]);});var _v8 = T(function(){return A(_6L,[_9k,_v7,_qm]);});var _v9 = T(function(){return A(_6L,[_9k,_ql,_v8]);});var _va = T(function(){return A(_6L,[_9k,_qd,_v9]);});var _vb = T(function(){return A(_6L,[_9k,_qa,_va]);});var _vc = function(_vd){var _ve = E(_vd);var _vf = _ve[1];var _vg = _ve[2];var _vh = E(_vf);var _vi = _vh[1];var _vj = _vh[2];var _vk = [1,_vg,_vi,_vj];return _vk;};var _vl = T(function(){return A(_9l,[_9j,_vc]);});var _vm = T(function(){return A(_6L,[_9k,_vl,_vb]);});var _vn = function(_vo){var _vp = E(_vo);var _vq = _vp[1];var _vr = _vp[2];var _vs = _vp[3];var _vt = [1,_vq,_vr,_vs];return _vt;};var _vu = T(function(){return A(_9l,[_9j,_vn]);});var _vv = T(function(){return A(_6L,[_9k,_vu,_vm]);});var _vw = function(_vx){var _vy = E(_vx);var _vz = _vy[1];var _vA = _vy[2];var _vB = T(function(){var _vC = E(_vA);if(_vC[0]==1){var _vD = _vC[2];var _vE = E(_vD);var _vF = _vE[1];var _vG = E(_vF);var _vH = _vG[2];var _vI = E(_vH);var _vJ = _vI[1];var _vK = _vJ>400;var _vL = _vK?E(_cu):[1];var _vM = _vL;}else{var _vM = [1];}return _vM;});var _vN = T(function(){var _vO = T(function(){return _ck(_cv,_vz);});return A(_cf,[_vO,_vv]);});var _vP = [1,_vz,_vN,_vB];var _vQ = [1,_vP,_0];return _vQ;};var _vR = T(function(){return A(_9l,[_9j,_vw]);});var _vS = T(function(){var _vT = E(_9k);var _vU = _vT[2];var _vV = T(function(){var _vW = T(function(){var _vX = T(function(){var _vY = T(function(){var _vZ = T(function(){return A(_vU,[_b7,_vR]);});return A(_vU,[_bf,_vZ]);});return A(_vU,[_bi,_vY]);});return A(_vU,[_bV,_vX]);});return A(_vU,[_c2,_vW]);});var _w0 = A(_vU,[_c5,_vV]);return _w0;});var _w1 = T(function(){return A(_9y,[_ae,_vS]);});var _w2 = T(function(){return A(_9t,[_9j,_w1]);});var _w3 = T(function(){return A(_6L,[_9k,_w2,_9s]);});var _w4 = T(function(){return A(_9l,[_9j,_b8]);});var _w5 = T(function(){return A(_6L,[_9k,_w4,_w3]);});var _w6 = T(function(){return A(_9l,[_9j,_bj]);});var _w7 = T(function(){return A(_6L,[_9k,_w6,_w5]);});var _w8 = function(_w9){var _wa = nMV(_w7,_w9);var _wb = _wa[1];var _wc = _wa[2];var _wd = nMV(_p,_wb);var _we = _wd[1];var _wf = _wd[2];var _wg = function(_wh,_wi){var _wj = rMV(_wf,_wi);var _wk = _wj[1];var _wl = _wj[2];var _wm = T(function(){var _wn = [2,_wh];var _wo = [2,_wn,_p];return _h(_wl,_wo);});var _wp = wMV(_wf,_wm,_wk);var _wq = [1,_wp,_0];return _wq;};var _wr = A(_2x,[_2S,_wg,_we]);var _ws = _wr[1];var _wt = function(_wu,_wv){var _ww = rMV(_wf,_wv);var _wx = _ww[1];var _wy = _ww[2];var _wz = T(function(){var _wA = [1,_wu];var _wB = [2,_wA,_p];return _h(_wy,_wB);});var _wC = wMV(_wf,_wz,_wx);var _wD = [1,_wC,_0];return _wD;};var _wE = A(_2Q,[_2S,_wt,_ws]);var _wF = _wE[1];var _wG = function(_wH){var _wI = rMV(_wc,_wH);var _wJ = _wI[1];var _wK = _wI[2];var _wL = rMV(_wf,_wJ);var _wM = _wL[1];var _wN = _wL[2];var _wO = A(_f,[_wK,_wN]);var _wP = _wO[1];var _wQ = _wO[2];var _wR = wMV(_wf,_p,_wM);var _wS = _5B(_wP,_wR);var _wT = _wS[1];var _wU = wMV(_wc,_wQ,_wT);var _wV = [1,_wU,_0];return _wV;};var _wW = A(_C,[_2R,_wG,_wF]);return _wW;};var _wX = T(function(){return A(_a,[_w8]);});
E(E(_wX)(0));
