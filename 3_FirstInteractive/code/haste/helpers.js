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

var allObjects = {}

function jsSaveGlobalObject(name, obj) {
	allObjects[name] = obj;
	return [1,0];
}

function jsLoadGlobalObject(name) {
	return [1,0,allObjects[name]];
}

