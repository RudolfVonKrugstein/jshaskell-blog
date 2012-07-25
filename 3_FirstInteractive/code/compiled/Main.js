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
        setCB(elem, 'keyup', args, function(k) {
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

var allObjects = {}

function jsSaveGlobalObject(name, obj) {
	allObjects[name] = obj;
	return [1,0];
}

function jsLoadGlobalObject(name) {
	return [1,0,allObjects[name]];
}


var _0 = [1];var _1 = function(_2){return [1,_2];};var _3 = function(_4,_5){var _6 = E(_5);var _7 = A(_4,[_6]);return _7;};var _8 = function(_9,_a){var _b = A(_3,[_1,_9]);var _c = _b[1];var _d = jsSetOnLoad(_c,_a);var _e = _d[1];var _f = [1,_e,_0];return _f;};var _g = function(_h,_i){return _8(_h,_i);};var _j = function(_k,_l,_m){var _n = A(_k,[_m]);var _o = _n[1];var _p = A(_l,[_o]);return _p;};var _q = function(_r,_s,_t){return _j(_r,_s,_t);};var _u = function(_v,_w,_x){var _y = A(_v,[_x]);var _z = _y[1];var _A = _y[2];var _B = A(_w,[_A,_z]);return _B;};var _C = function(_D,_E){return [1,_E,_D];};var _F = T(function(){return unCStr("Maybe.fromJust: Nothing");});var _G = T(function(){return err(_F);});var _H = function(_I,_J,_K){var _L = T(function(){var _M = A(_I,[_K]);var _N = _M[1];var _O = _M[2];var _P = T(function(){var _Q = E(_L);if(_Q[0]==1){var _R = E(_G);}else{var _S = _Q[1];var _R = E(_S);}return _R;});var _T = A(_J,[_P]);var _U = _T[1];var _V = _T[2];var _W = hs_eqWord64(_N,_U,realWorld);var _X = _W[2];var _Y = E(_X);if(_Y){var _Z = hs_eqWord64(_O,_V,realWorld);var _10 = _Z[2];var _11 = E(_10);var _12 = _11?[2,_K]:[1];var _13 = _12;}else{var _13 = [1];}return _13;});return E(_L);};var _14 = function(_15){var _16 = E(_15);var _17 = _16[1];var _18 = E(_17);return _18;};var _19 = T(function(){return unCStr("base");});var _1a = T(function(){return unCStr("GHC.IO.Exception");});var _1b = T(function(){return unCStr("IOException");});var _1c = [1,7238999624334008320,1.0769272474234763e19,_19,_1a,_1b];var _1d = [1];var _1e = [1,7238999624334008320,1.0769272474234763e19,_1c,_1d];var _1f = function(_1g){return E(_1e);};var _1h = function(_1i){var _1j = E(_1i);var _1k = _1j[1];var _1l = _1j[2];var _1m = _14(_1k);var _1n = _H(_1m,_1f,_1l);return _1n;};var _1o = function(_1p,_1q){var _1r = E(_1p);if(_1r[0]==1){var _1s = E(_1q);}else{var _1t = _1r[1];var _1u = _1r[2];var _1v = T(function(){return _1o(_1u,_1q);});var _1s = [2,_1t,_1v];}return _1s;};var _1w = T(function(){return unCStr(": ");});var _1x = T(function(){return unCStr("already exists");});var _1y = T(function(){return unCStr("does not exist");});var _1z = T(function(){return unCStr("protocol error");});var _1A = T(function(){return unCStr("failed");});var _1B = T(function(){return unCStr("invalid argument");});var _1C = T(function(){return unCStr("inappropriate type");});var _1D = T(function(){return unCStr("hardware fault");});var _1E = T(function(){return unCStr("unsupported operation");});var _1F = T(function(){return unCStr("timeout");});var _1G = T(function(){return unCStr("resource vanished");});var _1H = T(function(){return unCStr("interrupted");});var _1I = T(function(){return unCStr("resource busy");});var _1J = T(function(){return unCStr("resource exhausted");});var _1K = T(function(){return unCStr("end of file");});var _1L = T(function(){return unCStr("illegal operation");});var _1M = T(function(){return unCStr("permission denied");});var _1N = T(function(){return unCStr("user error");});var _1O = T(function(){return unCStr("unsatisified constraints");});var _1P = T(function(){return unCStr("system error");});var _1Q = function(_1R,_1S){var _1T = E(_1R);switch(_1T[0]){case 1:var _1U = _1o(_1x,_1S);break;case 2:var _1U = _1o(_1y,_1S);break;case 3:var _1U = _1o(_1I,_1S);break;case 4:var _1U = _1o(_1J,_1S);break;case 5:var _1U = _1o(_1K,_1S);break;case 6:var _1U = _1o(_1L,_1S);break;case 7:var _1U = _1o(_1M,_1S);break;case 8:var _1U = _1o(_1N,_1S);break;case 9:var _1U = _1o(_1O,_1S);break;case 10:var _1U = _1o(_1P,_1S);break;case 11:var _1U = _1o(_1z,_1S);break;case 12:var _1U = _1o(_1A,_1S);break;case 13:var _1U = _1o(_1B,_1S);break;case 14:var _1U = _1o(_1C,_1S);break;case 15:var _1U = _1o(_1D,_1S);break;case 16:var _1U = _1o(_1E,_1S);break;case 17:var _1U = _1o(_1F,_1S);break;case 18:var _1U = _1o(_1G,_1S);break;case 19:var _1U = _1o(_1H,_1S);break;}return _1U;};var _1V = T(function(){return unCStr(" (");});var _1W = [1,')'];var _1X = [1,'}'];var _1Y = T(function(){return unCStr("{handle: ");});var _1Z = function(_20,_21,_22,_23,_24,_25){var _26 = T(function(){var _27 = T(function(){var _28 = T(function(){var _29 = E(_23);if(_29[0]==1){var _2a = E(_25);}else{var _2b = T(function(){var _2c = [2,_1W,_25];return _1o(_29,_2c);});var _2a = _1o(_1V,_2b);}return _2a;});return _1Q(_21,_28);});var _2d = E(_22);if(_2d[0]==1){var _2e = E(_27);}else{var _2f = T(function(){return _1o(_1w,_27);});var _2e = _1o(_2d,_2f);}return _2e;});var _2g = E(_24);if(_2g[0]==1){var _2h = E(_20);if(_2h[0]==1){var _2i = E(_26);}else{var _2j = _2h[1];var _2k = E(_2j);if(_2k[0]==1){var _2l = _2k[1];var _2m = T(function(){var _2n = T(function(){return _1o(_1w,_26);});var _2o = [2,_1X,_2n];return _1o(_2l,_2o);});var _2p = _1o(_1Y,_2m);}else{var _2q = _2k[1];var _2r = T(function(){var _2s = T(function(){return _1o(_1w,_26);});var _2t = [2,_1X,_2s];return _1o(_2q,_2t);});var _2p = _1o(_1Y,_2r);}var _2i = _2p;}var _2u = _2i;}else{var _2v = _2g[1];var _2w = T(function(){return _1o(_1w,_26);});var _2u = _1o(_2v,_2w);}return _2u;};var _2x = function(_2y){var _2z = E(_2y);var _2A = _2z[1];var _2B = _2z[2];var _2C = _2z[3];var _2D = _2z[4];var _2E = _2z[6];var _2F = _1Z(_2A,_2B,_2C,_2D,_2E,_1d);return _2F;};var _2G = [1,','];var _2H = [1,']'];var _2I = [1,'['];var _2J = function(_2K,_2L){var _2M = E(_2K);if(_2M[0]==1){var _2N = unAppCStr("[]",_2L);}else{var _2O = _2M[1];var _2P = _2M[2];var _2Q = T(function(){var _2R = E(_2O);var _2S = _2R[1];var _2T = _2R[2];var _2U = _2R[3];var _2V = _2R[4];var _2W = _2R[6];var _2X = T(function(){var _2Y = [2,_2H,_2L];var _2Z = function(_30){var _31 = E(_30);if(_31[0]==1){var _32 = E(_2Y);}else{var _33 = _31[1];var _34 = _31[2];var _35 = T(function(){var _36 = E(_33);var _37 = _36[1];var _38 = _36[2];var _39 = _36[3];var _3a = _36[4];var _3b = _36[6];var _3c = T(function(){return _2Z(_34);});var _3d = _1Z(_37,_38,_39,_3a,_3b,_3c);return _3d;});var _32 = [2,_2G,_35];}return _32;};return _2Z(_2P);});var _3e = _1Z(_2S,_2T,_2U,_2V,_2W,_2X);return _3e;});var _2N = [2,_2I,_2Q];}return _2N;};var _3f = function(_3g,_3h,_3i){var _3j = E(_3h);var _3k = _3j[1];var _3l = _3j[2];var _3m = _3j[3];var _3n = _3j[4];var _3o = _3j[6];var _3p = _1Z(_3k,_3l,_3m,_3n,_3o,_3i);return _3p;};var _3q = [1,_3f,_2x,_2J];var _3r = T(function(){return [1,_1f,_3q,_3s,_1h];});var _3s = function(_3t){return [1,_3r,_3t];};var _3u = [1];var _3v = [8];var _3w = function(_3x){return [1,_3u,_3v,_1d,_3x,_3u,_3u];};var _3y = function(_3z,_3A){var _3B = T(function(){var _3C = T(function(){return _3w(_3z);});return _3s(_3C);});return die(_3B,_3A);};var _3D = function(_3E,_3F){return _3y(_3E,_3F);};var _3G = [1,_u,_q,_C,_3D];var _3H = function(_3I){var _3J = E(_3I);var _3K = _3J[2];var _3L = E(_3K);return _3L;};var _3M = function(_3N){var _3O = E(_3N);var _3P = _3O[3];var _3Q = E(_3P);return _3Q;};var _3R = function(_3S,_3T,_3U){var _3V = E(_3S);var _3W = _3V[1];var _3X = A(_3,[_1,_3T]);var _3Y = _3X[1];var _3Z = jsSetInterval(_3W,_3Y,_3U);var _40 = _3Z[1];var _41 = [1,_40,_0];return _41;};var _42 = function(_43,_h,_i){return _3R(_43,_h,_i);};var _44 = function(_45,_46){return A(_45,[_46]);};var _47 = [15,coercionToken];var _48 = "load";var _49 = [1,_48];var _4a = "mousemove";var _4b = [1,_4a];var _4c = "mouseover";var _4d = [1,_4c];var _4e = "mouseout";var _4f = [1,_4e];var _4g = "click";var _4h = [1,_4g];var _4i = "dblclick";var _4j = [1,_4i];var _4k = "mousedown";var _4l = [1,_4k];var _4m = "mouseup";var _4n = [1,_4m];var _4o = "keypress";var _4p = [1,_4o];var _4q = "keyup";var _4r = [1,_4q];var _4s = "keydown";var _4t = [1,_4s];var _4u = "unload";var _4v = [1,_4u];var _4w = "change";var _4x = [1,_4w];var _4y = "focus";var _4z = [1,_4y];var _4A = "blur";var _4B = [1,_4A];var _4C = function(_4D,_4E,_4F,_4G){var _4H = [1,_4F];var _4I = _4H[1];var _4J = function(_4K){var _4L = E(_4F);var _4M = jsSetCB(_4D,_4K,_4I,_4G);var _4N = _4M[1];var _4O = _4M[2];var _4P = T(function(){var _4Q = E(_4O);return _4Q?true:false;});var _4R = [1,_4N,_4P];return _4R;};var _4S = E(_4E);switch(_4S[0]){case 1:var _4T = E(_49);var _4U = _4T[1];var _4V = _4J(_4U);var _4W = _4V;break;case 2:var _4X = E(_4v);var _4Y = _4X[1];var _4Z = _4J(_4Y);var _4W = _4Z;break;case 3:var _50 = E(_4x);var _51 = _50[1];var _52 = _4J(_51);var _4W = _52;break;case 4:var _53 = E(_4z);var _54 = _53[1];var _55 = _4J(_54);var _4W = _55;break;case 5:var _56 = E(_4B);var _57 = _56[1];var _58 = _4J(_57);var _4W = _58;break;case 6:var _59 = E(_4b);var _5a = _59[1];var _5b = _4J(_5a);var _4W = _5b;break;case 7:var _5c = E(_4d);var _5d = _5c[1];var _5e = _4J(_5d);var _4W = _5e;break;case 8:var _5f = E(_4f);var _5g = _5f[1];var _5h = _4J(_5g);var _4W = _5h;break;case 9:var _5i = E(_4h);var _5j = _5i[1];var _5k = _4J(_5j);var _4W = _5k;break;case 10:var _5l = E(_4j);var _5m = _5l[1];var _5n = _4J(_5m);var _4W = _5n;break;case 11:var _5o = E(_4l);var _5p = _5o[1];var _5q = _4J(_5p);var _4W = _5q;break;case 12:var _5r = E(_4n);var _5s = _5r[1];var _5t = _4J(_5s);var _4W = _5t;break;case 13:var _5u = E(_4p);var _5v = _5u[1];var _5w = _4J(_5v);var _4W = _5w;break;case 14:var _5x = E(_4r);var _5y = _5x[1];var _5z = _4J(_5y);var _4W = _5z;break;case 15:var _5A = E(_4t);var _5B = _5A[1];var _5C = _4J(_5B);var _4W = _5C;break;}return _4W;};var _5D = function(_5E,_5F,_5G,_5H){var _5I = E(_5E);var _5J = _5I[1];var _5K = _4C(_5J,_5F,_5G,_5H);return _5K;};var _5L = function(_5M,_5N,_5O,_2){return _5D(_5M,_5N,_5O,_2);};var _5P = T(function(){return unCStr(" could be found!");});var _5Q = function(_5R){var _5S = T(function(){return _1o(_5R,_5P);});var _5T = unAppCStr("No element with ID ",_5S);var _5U = err(_5T);return _5U;};var _5V = function(_5W,_5X,_5Y){var _5Z = toJSStr(_5W);var _60 = _5Z[1];var _61 = jsFind(_60,_5Y);var _62 = _61[1];var _63 = _61[2];var _64 = [1,_63];var _65 = _64[1];var _66 = E(_65);if(_66[0]==1){var _67 = _5Q(_5W);}else{var _68 = _66[1];var _67 = A(_5X,[_68,_62]);}return _67;};var _69 = function(_6a,_6b,_6c){return _5V(_6a,_6b,_6c);};var _6d = function(_6e,_6f){var _6g = function(_6h){return A(_5L,[_6h,_47,_6f]);};var _6i = T(function(){return A(_69,[_6e]);});return A(_44,[_6i,_6g]);};var _6j = T(function(){return A(unCStr,["canvas1"]);});var _6k = function(_6l){var _6m = E(_6l);var _6n = _6m[1];var _6o = E(_6n);return _6o;};var _6p = function(_6q){var _6r = E(_6q);var _6s = _6r[1];var _6t = _6s>=0;if(_6t){var _6u = E(_6r);}else{var _6v = -_6s;var _6w = [1,_6v];var _6u = _6w;}return _6u;};var _6x = function(_6y){var _6z = E(_6y);if(_6z[0]==1){var _6A = _6z[1];var _6B = _6z[2];var _6C = _6x(_6B);var _6D = (_6A&65535)>>>0;var _6E = _6D&4294967295;var _6F = _6E;var _6G = Math.pow(2,16);var _6H = _6A>>>16;var _6I = _6H&4294967295;var _6J = _6I;var _6K = _6J*_6G;var _6L = Math.pow(2,32);var _6M = _6C*_6L;var _6N = _6M+_6K;var _6O = _6N+_6F;var _6P = _6O;}else{var _6P = 0;}return _6P;};var _6Q = function(_6R){var _6S = E(_6R);switch(_6S[0]){case 1:var _6T = _6S[1];var _6U = _6x(_6T);break;case 2:var _6V = _6S[1];var _6W = _6x(_6V);var _6X = -_6W;var _6U = _6X;break;case 3:var _6U = 0;break;}return _6U;};var _6Y = function(_6Z){var _70 = _6Q(_6Z);var _71 = [1,_70];return _71;};var _72 = [1,0];var _73 = [1,1];var _74 = [1,(-1)];var _75 = function(_76){var _77 = E(_76);var _78 = _77[1];var _79 = _78==0;if(_79){var _7a = E(_72);}else{var _7b = _78>0;var _7a = _7b?E(_73):E(_74);}return _7a;};var _7c = function(_7d,_7e){var _7f = E(_7d);var _7g = _7f[1];var _7h = E(_7e);var _7i = _7h[1];var _7j = _7g-_7i;var _7k = [1,_7j];return _7k;};var _7l = function(_7m){var _7n = E(_7m);var _7o = _7n[1];var _7p = -_7o;var _7q = [1,_7p];return _7q;};var _7r = function(_7s,_7t){var _7u = E(_7s);var _7v = _7u[1];var _7w = E(_7t);var _7x = _7w[1];var _7y = _7v+_7x;var _7z = [1,_7y];return _7z;};var _7A = function(_7B,_7C){var _7D = E(_7B);var _7E = _7D[1];var _7F = E(_7C);var _7G = _7F[1];var _7H = _7E*_7G;var _7I = [1,_7H];return _7I;};var _7J = [1,_7r,_7A,_7c,_7l,_6p,_75,_6Y];var _7K = function(_7L){var _7M = E(_7L);var _7N = _7M[1];var _7O = E(_7N);return _7O;};var _7P = function(_7Q){var _7R = E(_7Q);var _7S = _7R[3];var _7T = E(_7S);return _7T;};var _7U = function(_7V){var _7W = E(_7V);var _7X = _7W[1];var _7Y = E(_7X);return _7Y;};var _7Z = function(_80){var _81 = function(_82){var _83 = T(function(){return A(_7U,[_82]);});var _84 = T(function(){return _3M(_3G);});return A(_44,[_84,_83]);};var _85 = T(function(){var _86 = A(toJSStr,[_80]);var _87 = _86[1];var _88 = function(_89){var _8a = jsLoadGlobalObject(_87,_89);var _8b = _8a[1];var _8c = _8a[2];var _8d = [1,_8c];var _8e = [1,_8b,_8d];return _8e;};var _8f = E(_88);return _8f;});return A(_6k,[_3G,_85,_81]);};var _8g = function(_8h){return [1,_8h];};var _8i = function(_8j,_8k,_8l){var _8m = A(toJSStr,[_8j]);var _8n = _8m[1];var _8o = A(_8g,[_8k]);var _8p = _8o[1];var _8q = jsSaveGlobalObject(_8n,_8p,_8l);var _8r = _8q[1];var _8s = [1,_8r,_0];return _8s;};var _8t = function(_43,_h,_i){return _8i(_43,_h,_i);};var _8u = [1,3];var _8v = function(_8w){var _8x = function(_8y){var _8z = T(function(){var _8A = E(_8w);var _8B = _8A[1];var _8C = E(_8B);switch(_8C){case 37:var _8D = E(_8y);var _8E = T(function(){var _8F = T(function(){var _8G = E(_8D);var _8H = _8G[1];var _8I = E(_8H);return _8I;});return A(_7P,[_7J,_8F,_8u]);});var _8J = [1,_8E];var _8K = _8J;break;case 39:var _8L = E(_8y);var _8M = T(function(){var _8N = T(function(){var _8O = E(_8L);var _8P = _8O[1];var _8Q = E(_8P);return _8Q;});return A(_7K,[_7J,_8N,_8u]);});var _8R = [1,_8M];var _8K = _8R;break;default:var _8K = E(_8y);}return _8K;});var _8S = T(function(){return A(unCStr,["state"]);});return A(_8t,[_8S,_8z]);};var _8T = T(function(){var _8U = T(function(){return A(unCStr,["state"]);});return A(_7Z,[_8U]);});return A(_6k,[_3G,_8T,_8x]);};var _8V = function(_8W,_8X){var _8Y = E(_8W);var _8Z = _8Y[1];var _90 = jsClear(_8Z,_8X);var _91 = _90[1];var _92 = [1,_91,_0];return _92;};var _93 = function(_h,_i){return _8V(_h,_i);};var _94 = function(_95,_96,_97,_98,_99,_9a){var _9b = E(_95);var _9c = _9b[1];var _9d = E(_96);var _9e = _9d[1];var _9f = E(_97);var _9g = _9f[1];var _9h = E(_98);var _9i = _9h[1];var _9j = E(_99);var _9k = _9j[1];var _9l = jsFillRect(_9c,_9e,_9g,_9i,_9k,_9a);var _9m = _9l[1];var _9n = [1,_9m,_0];return _9n;};var _9o = function(_9p,_9q,_9r,_43,_h,_i){return _94(_9p,_9q,_9r,_43,_h,_i);};var _9s = function(_9t,_9u){var _9v = E(_9t);var _9w = _9v[1];var _9x = jsGetContext2D(_9w,_9u);var _9y = _9x[1];var _9z = _9x[2];var _9A = [1,_9z];var _9B = [1,_9y,_9A];return _9B;};var _9C = function(_h,_i){return _9s(_h,_i);};var _9D = function(_9E){return A(_69,[_9E,_9C]);};var _9F = function(_9G,_9H,_9I){var _9J = T(function(){return A(_9H,[_9I]);});return A(_9G,[_9J]);};var _9K = function(_9L,_9M,_9N){var _9O = E(_9L);var _9P = _9O[1];var _9Q = E(_9M);var _9R = _9Q[1];var _9S = jsSetFillColor(_9P,_9R,_9N);var _9T = _9S[1];var _9U = [1,_9T,_0];return _9U;};var _9V = function(_43,_h,_i){return _9K(_43,_h,_i);};var _9W = function(_9X){var _9Y = function(_h,_i){return _9V(_9X,_h,_i);};return A(_9F,[_9Y,toJSStr]);};var _9Z = T(function(){return A(unCStr,["green"]);});var _a0 = [1,20];var _a1 = [1,60];var _a2 = [1,380];var _a3 = function(_a4){var _a5 = function(_a6){var _a7 = T(function(){var _a8 = T(function(){var _a9 = T(function(){var _aa = E(_a4);var _ab = _aa[1];var _ac = E(_ab);return _ac;});return A(_9o,[_a6,_a9,_a2,_a1,_a0]);});var _ad = T(function(){return A(_9W,[_a6,_9Z]);});return A(_3H,[_3G,_ad,_a8]);});var _ae = T(function(){return A(_93,[_a6]);});return A(_3H,[_3G,_ae,_a7]);};var _af = T(function(){return A(_9D,[_6j]);});return A(_6k,[_3G,_af,_a5]);};var _ag = T(function(){var _ah = T(function(){return A(unCStr,["state"]);});return A(_7Z,[_ah]);});var _ai = T(function(){return A(_6k,[_3G,_ag,_a3]);});var _aj = T(function(){var _ak = T(function(){var _al = T(function(){return A(_3M,[_3G,_0]);});var _am = T(function(){return A(_6d,[_6j,_8v]);});return A(_3H,[_3G,_am,_al]);});var _an = T(function(){var _ao = [1,30];return A(_42,[_ao,_ai]);});return A(_3H,[_3G,_an,_ak]);});var _ap = [1,300];var _aq = [1,_ap];var _ar = T(function(){var _as = T(function(){return A(unCStr,["state"]);});return A(_8t,[_as,_aq]);});var _at = T(function(){return A(_3H,[_3G,_ar,_aj]);});var _au = T(function(){return A(_g,[_at]);});
E(E(_au)(0));
