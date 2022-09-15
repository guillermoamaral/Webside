class Change extends Object {
	static type() {
		return this.name;
	}

	static initializeTypeMap() {
		this.typeMap = {};
		this.typeMap["AddMethod"] = AddMethod;
		this.typeMap["RemoveMethod"] = null;
		this.typeMap["ClassifyMethod"] = null;
		this.typeMap["RenameMethod"] = null;
		this.typeMap["AddClass"] = null;
		this.typeMap["CommentClass"] = null;
		this.typeMap["RemoveClass"] = null;
		this.typeMap["RenameClass"] = null;
		this.typeMap["AddInstanceVariable"] = null;
		this.typeMap["RemoveInstanceVariable"] = null;
		this.typeMap["RenameInstanceVariable"] = null;
		this.typeMap["MoveUpInstanceVariable"] = null;
		this.typeMap["MoveDownInstanceVariable"] = null;
		this.typeMap["AddClassVariable"] = null;
		this.typeMap["RemoveClassVariable"] = null;
		this.typeMap["RenameClassVariable"] = null;
		this.typeMap["RenameCategory"] = null;
		this.typeMap["RemoveCategory"] = null;
		this.typeMap["AddPackage"] = null;
		this.typeMap["RemovePackage"] = null;
		this.typeMap["RenamePackage"] = null;
	}

	static fromJson(json) {
		const subclass = this.classFromType(json.type);
		var change = new subclass();
		change.fromJson(json);
		return change;
	}

	static classFromType(type) {
		if (!this.typeMap) {
			this.initializeTypeMap();
		}
		return this.typeMap[type];
	}

	fromJson(json) {}

	type() {
		return this.constructor.type();
	}
}

class ClassChange extends Change {
	constructor() {
		super();
		this.className = null;
	}

	fromJson(json) {
		super.fromJson(json);
		this.className = json.className;
	}
}

class AddMethod extends ClassChange {
	constructor() {
		super();
		this.selector = null;
		this.category = null;
		this.sourceCode = null;
	}

	fromJson(json) {
		super.fromJson(json);
		this.selector = json.selector;
		this.category = json.category;
		this.sourceCode = json.sourceCode;
	}
}

export default Change;
