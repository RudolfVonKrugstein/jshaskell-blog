function jsGetContext2D(canvas) {
  return canvas.getContext("2d");
}

function jsBeginPath(context) {
	context.beginPath();
	return;
}

function jsClosePath(context) {
	context.closePath();
	return;
}

function jsFill(context) {
	context.fill();
	return;
}

function jsArc(context, x, y, r, minPhi, maxPhi, ccw) {
	context.arc(x,y,r,minPhi,maxPhi,ccw);
	return;
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

function jsSetInterval(msecs, cb) {
	window.setInterval(function() {A(cb,[0]);}, msecs);
	return;
}

function jsSetOnLoad(cb) {
	window.addEventListener('load', function() {A(cb,[0]);}, false);
	return;
}

var allObjects = {}

function jsSaveGlobalObject(name, obj) {
	allObjects[name] = obj;
	return;
}

function jsLoadGlobalObject(name) {
	return allObjects[name];
}

