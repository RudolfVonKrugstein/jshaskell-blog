var allObjects = {}

function jsSaveGlobalObject(name, obj) {
	 allObjects[name] = obj;
}

function jsLoadGlobalObject(name) {
	 return allObjects[name];
}
