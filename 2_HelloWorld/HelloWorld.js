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
        setCB(elem, 'keyup', function(k) {
            if(k == '\n') {
                A(cb,[0]);
            }
        });
    }
    return setCB(elem, evt, function() {A(cb,[0]);});
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

function objWrite(obj, text, _realWorld) {
	    obj.write(text); // Call the method
		    return [1,0]; // Return ()
}

function getDocument(_realWorld) {
	    return [1, 0, document];
}

var _0 = function(_1,_2,_3){var _4 = A(_1,[_3]);var _5 = _4[1];var _6 = A(_2,[_5]);return _6;};var _7 = function(_8,_9,_a){return _0(_8,_9,_a);};var _b = function(_c,_d,_e){var _f = A(_c,[_e]);var _g = _f[1];var _h = _f[2];var _i = A(_d,[_h,_g]);return _i;};var _j = function(_k,_l){return [1,_l,_k];};var _m = T(function(){return unCStr("Maybe.fromJust: Nothing");});var _n = T(function(){return err(_m);});var _o = function(_p,_q,_r){var _s = T(function(){var _t = A(_p,[_r]);var _u = _t[1];var _v = _t[2];var _w = T(function(){var _x = E(_s);if(_x[0]==1){var _y = E(_n);}else{var _z = _x[1];var _y = E(_z);}return _y;});var _A = A(_q,[_w]);var _B = _A[1];var _C = _A[2];var _D = hs_eqWord64(_u,_B,realWorld);var _E = _D[2];var _F = E(_E);if(_F){var _G = hs_eqWord64(_v,_C,realWorld);var _H = _G[2];var _I = E(_H);var _J = _I?[2,_r]:[1];var _K = _J;}else{var _K = [1];}return _K;});return E(_s);};var _L = function(_M){var _N = E(_M);var _O = _N[1];var _P = E(_O);return _P;};var _Q = T(function(){return unCStr("base");});var _R = T(function(){return unCStr("GHC.IO.Exception");});var _S = T(function(){return unCStr("IOException");});var _T = [1,7238999624334008320,1.0769272474234763e19,_Q,_R,_S];var _U = [1];var _V = [1,7238999624334008320,1.0769272474234763e19,_T,_U];var _W = function(_X){return E(_V);};var _Y = function(_Z){var _10 = E(_Z);var _11 = _10[1];var _12 = _10[2];var _13 = _L(_11);var _14 = _o(_13,_W,_12);return _14;};var _15 = function(_16,_17){var _18 = E(_16);if(_18[0]==1){var _19 = E(_17);}else{var _1a = _18[1];var _1b = _18[2];var _1c = T(function(){return _15(_1b,_17);});var _19 = [2,_1a,_1c];}return _19;};var _1d = T(function(){return unCStr(": ");});var _1e = T(function(){return unCStr("already exists");});var _1f = T(function(){return unCStr("does not exist");});var _1g = T(function(){return unCStr("protocol error");});var _1h = T(function(){return unCStr("failed");});var _1i = T(function(){return unCStr("invalid argument");});var _1j = T(function(){return unCStr("inappropriate type");});var _1k = T(function(){return unCStr("hardware fault");});var _1l = T(function(){return unCStr("unsupported operation");});var _1m = T(function(){return unCStr("timeout");});var _1n = T(function(){return unCStr("resource vanished");});var _1o = T(function(){return unCStr("interrupted");});var _1p = T(function(){return unCStr("resource busy");});var _1q = T(function(){return unCStr("resource exhausted");});var _1r = T(function(){return unCStr("end of file");});var _1s = T(function(){return unCStr("illegal operation");});var _1t = T(function(){return unCStr("permission denied");});var _1u = T(function(){return unCStr("user error");});var _1v = T(function(){return unCStr("unsatisified constraints");});var _1w = T(function(){return unCStr("system error");});var _1x = function(_1y,_1z){var _1A = E(_1y);switch(_1A[0]){case 1:var _1B = _15(_1e,_1z);break;case 2:var _1B = _15(_1f,_1z);break;case 3:var _1B = _15(_1p,_1z);break;case 4:var _1B = _15(_1q,_1z);break;case 5:var _1B = _15(_1r,_1z);break;case 6:var _1B = _15(_1s,_1z);break;case 7:var _1B = _15(_1t,_1z);break;case 8:var _1B = _15(_1u,_1z);break;case 9:var _1B = _15(_1v,_1z);break;case 10:var _1B = _15(_1w,_1z);break;case 11:var _1B = _15(_1g,_1z);break;case 12:var _1B = _15(_1h,_1z);break;case 13:var _1B = _15(_1i,_1z);break;case 14:var _1B = _15(_1j,_1z);break;case 15:var _1B = _15(_1k,_1z);break;case 16:var _1B = _15(_1l,_1z);break;case 17:var _1B = _15(_1m,_1z);break;case 18:var _1B = _15(_1n,_1z);break;case 19:var _1B = _15(_1o,_1z);break;}return _1B;};var _1C = T(function(){return unCStr(" (");});var _1D = [1,')'];var _1E = [1,'}'];var _1F = T(function(){return unCStr("{handle: ");});var _1G = function(_1H,_1I,_1J,_1K,_1L,_1M){var _1N = T(function(){var _1O = T(function(){var _1P = T(function(){var _1Q = E(_1K);if(_1Q[0]==1){var _1R = E(_1M);}else{var _1S = T(function(){var _1T = [2,_1D,_1M];return _15(_1Q,_1T);});var _1R = _15(_1C,_1S);}return _1R;});return _1x(_1I,_1P);});var _1U = E(_1J);if(_1U[0]==1){var _1V = E(_1O);}else{var _1W = T(function(){return _15(_1d,_1O);});var _1V = _15(_1U,_1W);}return _1V;});var _1X = E(_1L);if(_1X[0]==1){var _1Y = E(_1H);if(_1Y[0]==1){var _1Z = E(_1N);}else{var _20 = _1Y[1];var _21 = E(_20);if(_21[0]==1){var _22 = _21[1];var _23 = T(function(){var _24 = T(function(){return _15(_1d,_1N);});var _25 = [2,_1E,_24];return _15(_22,_25);});var _26 = _15(_1F,_23);}else{var _27 = _21[1];var _28 = T(function(){var _29 = T(function(){return _15(_1d,_1N);});var _2a = [2,_1E,_29];return _15(_27,_2a);});var _26 = _15(_1F,_28);}var _1Z = _26;}var _2b = _1Z;}else{var _2c = _1X[1];var _2d = T(function(){return _15(_1d,_1N);});var _2b = _15(_2c,_2d);}return _2b;};var _2e = function(_2f){var _2g = E(_2f);var _2h = _2g[1];var _2i = _2g[2];var _2j = _2g[3];var _2k = _2g[4];var _2l = _2g[6];var _2m = _1G(_2h,_2i,_2j,_2k,_2l,_U);return _2m;};var _2n = [1,','];var _2o = [1,']'];var _2p = [1,'['];var _2q = function(_2r,_2s){var _2t = E(_2r);if(_2t[0]==1){var _2u = unAppCStr("[]",_2s);}else{var _2v = _2t[1];var _2w = _2t[2];var _2x = T(function(){var _2y = E(_2v);var _2z = _2y[1];var _2A = _2y[2];var _2B = _2y[3];var _2C = _2y[4];var _2D = _2y[6];var _2E = T(function(){var _2F = [2,_2o,_2s];var _2G = function(_2H){var _2I = E(_2H);if(_2I[0]==1){var _2J = E(_2F);}else{var _2K = _2I[1];var _2L = _2I[2];var _2M = T(function(){var _2N = E(_2K);var _2O = _2N[1];var _2P = _2N[2];var _2Q = _2N[3];var _2R = _2N[4];var _2S = _2N[6];var _2T = T(function(){return _2G(_2L);});var _2U = _1G(_2O,_2P,_2Q,_2R,_2S,_2T);return _2U;});var _2J = [2,_2n,_2M];}return _2J;};return _2G(_2w);});var _2V = _1G(_2z,_2A,_2B,_2C,_2D,_2E);return _2V;});var _2u = [2,_2p,_2x];}return _2u;};var _2W = function(_2X,_2Y,_2Z){var _30 = E(_2Y);var _31 = _30[1];var _32 = _30[2];var _33 = _30[3];var _34 = _30[4];var _35 = _30[6];var _36 = _1G(_31,_32,_33,_34,_35,_2Z);return _36;};var _37 = [1,_2W,_2e,_2q];var _38 = T(function(){return [1,_W,_37,_39,_Y];});var _39 = function(_3a){return [1,_38,_3a];};var _3b = [1];var _3c = [8];var _3d = function(_3e){return [1,_3b,_3c,_U,_3e,_3b,_3b];};var _3f = function(_3g,_3h){var _3i = T(function(){var _3j = T(function(){return _3d(_3g);});return _39(_3j);});return die(_3i,_3h);};var _3k = function(_3l,_3m){return _3f(_3l,_3m);};var _3n = [1,_b,_7,_j,_3k];var _3o = function(_3p){var _3q = E(_3p);var _3r = _3q[1];var _3s = E(_3r);return _3s;};var _3t = function(_3u){var _3v = getDocument(_3u);var _3w = _3v[1];var _3x = _3v[2];var _3y = [1,_3x];var _3z = [1,_3w,_3y];return _3z;};var _3A = function(_3B){return _3t(_3B);};var _3C = function(_3D,_3E){return A(_3D,[_3E]);};var _3F = [1];var _3G = function(_3H,_3I,_3J){var _3K = E(_3H);var _3L = _3K[1];var _3M = E(_3I);var _3N = _3M[1];var _3O = objWrite(_3L,_3N,_3J);var _3P = _3O[1];var _3Q = [1,_3P,_3F];return _3Q;};var _3R = function(_3S,_3T,_3B){return _3G(_3S,_3T,_3B);};var _3U = function(_3V){var _3W = T(function(){var _3X = T(function(){return A(unCStr,["Hello World!"]);});return A(toJSStr,[_3X]);});var _3Y = function(_3T,_3B){return _3R(_3V,_3T,_3B);};return A(_3C,[_3Y,_3W]);};var _3Z = T(function(){return A(_3o,[_3n,_3A,_3U]);});
window.onload = function() {E(E(_3Z)(0));};
