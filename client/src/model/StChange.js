class StChange extends Object {
	constructor() {
		super();
		this.label = "";
		this.package = null;
		this.timestamp = null;
		this.author = null;
		this.changeset = null;
	}

	static type() {
		return this.name;
	}

	static initializeTypeMap() {
		this.typeMap = {};
		this.typeMap["AddMethod"] = AddMethod;
		this.typeMap["RemoveMethod"] = RemoveMethod;
		this.typeMap["ClassifyMethod"] = ClassifyMethod;
		this.typeMap["RenameMethod"] = RenameMethod;
		this.typeMap["AddClass"] = AddClass;
		this.typeMap["CommentClass"] = CommentClass;
		this.typeMap["RemoveClass"] = RemoveClass;
		this.typeMap["RenameClass"] = RenameClass;
		this.typeMap["AddInstanceVariable"] = AddInstanceVariable;
		this.typeMap["RemoveInstanceVariable"] = RemoveInstanceVariable;
		this.typeMap["RenameInstanceVariable"] = RenameInstanceVariable;
		this.typeMap["MoveUpInstanceVariable"] = MoveUpInstanceVariable;
		this.typeMap["MoveDownInstanceVariable"] = MoveDownInstanceVariable;
		this.typeMap["AddClassVariable"] = AddClassVariable;
		this.typeMap["RemoveClassVariable"] = RemoveClassVariable;
		this.typeMap["RenameClassVariable"] = RenameClassVariable;
		this.typeMap["RenameCategory"] = RenameCategory;
		this.typeMap["RemoveCategory"] = RemoveCategory;
		this.typeMap["AddPackage"] = AddPackage;
		this.typeMap["RemovePackage"] = RemovePackage;
		this.typeMap["RenamePackage"] = RenamePackage;
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
		return this.typeMap[type] || StChange;
	}

	fromJson(json) {}

	asJson() {
		return {};
	}

	type() {
		return this.constructor.type();
	}

	sourceCode() {
		return "";
	}

	currentSourceCodeIn(api) {
		return "";
	}

	canBeApplied() {
		return this.sourceCode() !== this.currentSourceCode();
	}
}

class ClassChange extends StChange {
	constructor() {
		super();
		this.className = null;
	}

	fromJson(json) {
		super.fromJson(json);
		this.className = json.className;
	}

	asJson() {
		var json = super.asJson();
		json.className = this.className;
		return json;
	}
}

class MethodChange extends ClassChange {
	constructor() {
		super();
		this.selector = null;
	}

	fromJson(json) {
		super.fromJson(json);
		this.selector = json.selector;
	}

	asJson() {
		var json = super.asJson();
		json.selector = this.selector;
		return json;
	}
}

class AddMethod extends MethodChange {
	constructor() {
		super();
		this.category = null;
		this.sourceCode = null;
	}

	fromJson(json) {
		super.fromJson(json);
		this.category = json.category;
		this.sourceCode = json.sourceCode;
	}

	asJson() {
		var json = super.asJson();
		json.category = this.category;
		json.sourceCode = this.sourceCode;
		return json;
	}

	sourceCode() {
		return this.sourceCode;
	}

	async currentSourceCodeIn(api) {
		var current;
		try {
			const method = await api.getMethod(this.className, this.selector);
			current = method.sourceCode;
		} catch (error) {
			current = "";
		}
		return current;
	}
}

class RemoveMethod extends MethodChange {}

class ClassifyMethod extends MethodChange {
	constructor() {
		super();
		this.category = null;
	}

	fromJson(json) {
		super.fromJson(json);
		this.category = json.category;
	}

	asJson() {
		var json = super.asJson();
		json.category = this.category;
		return json;
	}
}

class RenameMethod extends MethodChange {
	constructor() {
		super();
		this.newSelector = null;
	}

	fromJson(json) {
		super.fromJson(json);
		this.newSelector = json.newSelector;
	}

	asJson() {
		var json = super.asJson();
		json.newSelector = this.newSelector;
		return json;
	}
}

class AddClass extends ClassChange {
	constructor() {
		super();
		this.definition = null;
	}

	fromJson(json) {
		super.fromJson(json);
		this.definition = json.definition;
	}

	asJson() {
		var json = super.asJson();
		json.definition = this.definition;
		return json;
	}

	sourceCode() {
		return this.definition;
	}

	async currentSourceCodeIn(api) {
		var current;
		try {
			const species = await api.getClass(this.className);
			current = species.definition;
		} catch (error) {
			current = "";
		}
		return current;
	}
}

class CommentClass extends ClassChange {
	constructor() {
		super();
		this.comment = null;
	}

	fromJson(json) {
		super.fromJson(json);
		this.comment = json.comment;
	}

	asJson() {
		var json = super.asJson();
		json.comment = this.comment;
		return json;
	}

	sourceCode() {
		return this.comment;
	}
}

class RemoveClass extends ClassChange {}

class RenameClass extends ClassChange {
	constructor() {
		super();
		this.newName = null;
	}

	fromJson(json) {
		super.fromJson(json);
		this.newName = json.newName;
	}

	asJson() {
		var json = super.asJson();
		json.newName = this.newName;
		return json;
	}
}

class VariableChange extends ClassChange {
	constructor() {
		super();
		this.variable = null;
	}

	fromJson(json) {
		super.fromJson(json);
		this.variable = json.variable;
	}

	asJson() {
		var json = super.asJson();
		json.variable = this.variable;
		return json;
	}
}

class AddInstanceVariable extends VariableChange {}

class RemoveInstanceVariable extends VariableChange {}

class RenameInstanceVariable extends VariableChange {
	constructor() {
		super();
		this.newName = null;
	}

	fromJson(json) {
		super.fromJson(json);
		this.newName = json.newName;
	}

	asJson() {
		var json = super.asJson();
		json.newName = this.newName;
		return json;
	}
}

class MoveUpInstanceVariable extends VariableChange {}

class MoveDownInstanceVariable extends VariableChange {
	constructor() {
		super();
		this.target = null;
	}

	fromJson(json) {
		super.fromJson(json);
		this.target = json.target;
	}

	asJson() {
		var json = super.asJson();
		json.target = this.target;
		return json;
	}
}

class AddClassVariable extends VariableChange {}

class RemoveClassVariable extends VariableChange {}

class RenameClassVariable extends VariableChange {
	constructor() {
		super();
		this.newName = null;
	}

	fromJson(json) {
		super.fromJson(json);
		this.newName = json.newName;
	}

	asJson() {
		var json = super.asJson();
		json.newName = this.newName;
		return json;
	}
}

class CategoryChange extends ClassChange {
	constructor() {
		super();
		this.category = null;
	}

	fromJson(json) {
		super.fromJson(json);
		this.category = json.category;
	}

	asJson() {
		var json = super.asJson();
		json.category = this.category;
		return json;
	}
}

class RenameCategory extends CategoryChange {
	constructor() {
		super();
		this.newName = null;
	}

	fromJson(json) {
		super.fromJson(json);
		this.newName = json.newName;
	}

	asJson() {
		var json = super.asJson();
		json.newName = this.newName;
		return json;
	}
}

class RemoveCategory extends CategoryChange {}

class PackageChange extends StChange {
	constructor() {
		super();
		this.name = null;
	}

	fromJson(json) {
		super.fromJson(json);
		this.name = json.name;
	}

	asJson() {
		var json = super.asJson();
		json.name = this.name;
		return json;
	}
}

class AddPackage extends PackageChange {}

class RemovePackage extends PackageChange {}

class RenamePackage extends PackageChange {
	constructor() {
		super();
		this.newName = null;
	}

	fromJson(json) {
		super.fromJson(json);
		this.newName = json.newName;
	}

	asJson() {
		var json = super.asJson();
		json.newName = this.newName;
		return json;
	}
}

export default StChange;
