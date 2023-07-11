class StChange extends Object {
	constructor() {
		super();
		this.label = "";
		this.package = null;
		this.timestamp = null;
		this.author = null;
		this.changeset = null;
		this.source = null;
		this.currentSource = null;
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

	fromJson(json) {
		this.label = json.label;
		this.package = json.package;
		this.timestamp = json.timestamp;
		this.author = json.author;
		this.source = json.sourceCode;
		this.currentSource = json.currentSourceCode;
	}

	asJson() {
		var json = {};
		json.type = this.type();
		json.label = this.label;
		json.package = this.package;
		json.timestamp = this.timestamp;
		json.author = this.author;
		json.sourceCode = this.source;
		return json;
	}

	type() {
		return this.constructor.name;
	}

	sourceCode() {
		return this.source;
	}

	currentSourceCode() {
		return this.currentSource;
	}

	updateCurrentSourceCode() {}

	isUpToDate() {
		return this.sourceCode() === this.currentSourceCode();
	}

	isClassChange() {
		return false;
	}

	isMethodChange() {
		return false;
	}

	canOverride(change) {
		return false;
	}
}

class MethodChange extends StChange {
	constructor() {
		super();
		this.className = null;
		this.selector = null;
	}

	fromJson(json) {
		super.fromJson(json);
		this.className = json.className;
		this.selector = json.selector;
	}

	asJson() {
		var json = super.asJson();
		json.className = this.className;
		json.selector = this.selector;
		return json;
	}

	isMethodChange() {
		return true;
	}

	async updateCurrentSourceCode() {
		try {
			const method = await this.changeset.system.method(
				this.className,
				this.selector
			);
			this.currentSource = method.source;
		} catch (error) {
			this.currentSource = "could not find method";
		}
	}

	canOverride(change) {
		return change.isMethodChange() && this.selector === change.selector;
	}
}

class AddMethod extends MethodChange {
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

	canOverride(change) {
		return super.canOverride(change) && this.package === change.package;
	}
}

class RemoveMethod extends MethodChange {
	isUpToDate() {
		return this.currentSource === "could not find method";
	}
}

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

	async updateCurrentSourceCode() {
		try {
			const method = await this.changeset.system.method(
				this.className,
				this.selector
			);
			this.currentSource = method.category;
		} catch (error) {
			this.currentSource = "could not find method";
		}
	}

	isUpToDate() {
		return this.category === this.currentSource;
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

	isClassChange() {
		return true;
	}

	canOverride(change) {
		return change.isClassChange() && this.className === change.className;
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

	isUpToDate() {
		// console.log(
		// 	this.currentSourceCode()
		// 		.replace(/[\r\n]/gm, "")
		// 		.replace(/\s+/g, " ")
		// );
		return (
			this.sourceCode().replace(/[\r\n]/gm, "") ===
			this.currentSourceCode().replace(/[\r\n]/gm, "")
		);
	}

	async updateCurrentSourceCode() {
		try {
			const species = await this.changeset.system.classNamed(
				this.className
			);
			this.currentSource = species.definition;
		} catch (error) {
			this.currentSource = "could not find class";
		}
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

export { StChange, AddClass, AddMethod };
