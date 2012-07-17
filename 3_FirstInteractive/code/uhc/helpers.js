var allObjects = {}

function saveObject(name, obj) {
	 allObjects[name] = obj;
}

function loadObject(name) {
	 return allObjects[name];
}
