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
		this.changesSomething = false;
		this.canBeApplied = false;
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
		this.typeMap["AddClassCategory"] = AddClassCategory;
		this.typeMap["RenameClassCategory"] = RenameClassCategory;
		this.typeMap["RemoveClassCategory"] = RemoveClassCategory;
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
		this.changesSomething = json.changesSomething;
		this.canBeApplied = json.canBeApplied;
	}

	asJson() {
		var json = {};
		json.type = this.type();
		json.label = this.label;
		json.package = this.package;
		json.timestamp = this.timestamp;
		json.author = this.author;
		json.sourceCode = this.source;
		json.changesSomething = this.changesSomething;
		json.canBeApplied = this.canBeApplied;
		return json;
	}

	sourceCode() {
		return this.source;
	}

	currentSourceCode() {
		return this.currentSource;
	}

	isUpToDate() {
		return !this.changesSomething;
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

	async apply() {
		await this.changeset.applyChange(this);
	}

	async update() {
		await this.changeset.updateChange(this);
	}

	type() {
		return "Change";
	}
}

class MethodChange extends StChange {
	constructor() {
		super();
		this.className = null;
		this.selector = null;
	}

	type() {
		return "MethodChange";
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

	canOverride(change) {
		return change.isMethodChange() && this.selector === change.selector;
	}
}

class AddMethod extends MethodChange {
	constructor() {
		super();
		this.category = null;
	}

	type() {
		return "AddMethod";
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
	type() {
		return "RemoveMethod";
	}
}

class ClassifyMethod extends MethodChange {
	constructor() {
		super();
		this.category = null;
	}

	type() {
		return "ClassifyMethod";
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

	type() {
		return "RenameMethod";
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

	type() {
		return "ClassChange";
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
		this.superclass = null;
		this.instanceVariables = [];
		this.classVariables = [];
		this.poolDictionaries = [];
		this.definition = null;
	}

	type() {
		return "AddClass";
	}

	fromJson(json) {
		super.fromJson(json);
		this.superclass = json.superclass;
		this.instanceVariables = json.instanceVariables;
		this.classVariables = json.classVariables;
		this.poolDictionaries = json.poolDictionaries;
		this.definition = json.definition;
	}

	asJson() {
		var json = super.asJson();
		json.superclass = this.superclass;
		json.instanceVariables = this.instanceVariables;
		json.classVariables = this.classVariables;
		json.poolDictionaries = this.poolDictionaries;
		json.definition = this.definition;
		return json;
	}

	sourceCode() {
		return this.definition;
	}
}

class CommentClass extends ClassChange {
	constructor() {
		super();
		this.comment = null;
	}

	type() {
		return "CommentClass";
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

class RemoveClass extends ClassChange {
	type() {
		return "RemoveClass";
	}
}

class RenameClass extends ClassChange {
	constructor() {
		super();
		this.newName = null;
	}

	type() {
		return "RenameClass";
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

	type() {
		return "VariableChange";
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

class AddInstanceVariable extends VariableChange {
	type() {
		return "AddInstanceVariable";
	}
}

class RemoveInstanceVariable extends VariableChange {
	type() {
		return "RemoveInstanceVariable";
	}
}

class RenameInstanceVariable extends VariableChange {
	constructor() {
		super();
		this.newName = null;
	}

	type() {
		return "RenameInstanceVariable";
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

class MoveUpInstanceVariable extends VariableChange {
	type() {
		return "MoveUpInstanceVariable";
	}
}

class MoveDownInstanceVariable extends VariableChange {
	constructor() {
		super();
		this.target = null;
	}

	type() {
		return "MoveDownInstanceVariable";
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

class AddClassVariable extends VariableChange {
	type() {
		return "AddClassVariable";
	}
}

class RemoveClassVariable extends VariableChange {
	type() {
		return "RemoveClassVariable";
	}
}

class RenameClassVariable extends VariableChange {
	constructor() {
		super();
		this.newName = null;
	}

	type() {
		return "RenameClassVariable";
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

	type() {
		return "CategoryChange";
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

class ClassCategorCategory extends CategoryChange {
	constructor() {
		super();
		this.package = null;
	}

	type() {
		return "ClassCategorCategory";
	}

	fromJson(json) {
		super.fromJson(json);
		this.package = json.package;
	}

	asJson() {
		var json = super.asJson();
		json.package = this.package;
		return json;
	}
}

class AddClassCategory extends ClassCategorCategory {
	type() {
		return "AddClassCategory";
	}
}

class RenameClassCategory extends ClassCategorCategory {
	constructor() {
		super();
		this.newName = null;
	}

	type() {
		return "RenameClassCategory";
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

class RemoveClassCategory extends ClassCategorCategory {
	type() {
		return "RemoveClassCategory";
	}
}

class RenameCategory extends CategoryChange {
	constructor() {
		super();
		this.newName = null;
	}

	type() {
		return "RenameCategory";
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

class RemoveCategory extends CategoryChange {
	type() {
		return "RemoveCategory";
	}
}

class PackageChange extends StChange {
	constructor() {
		super();
		this.name = null;
	}

	type() {
		return "PackageChange";
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

class AddPackage extends PackageChange {
	type() {
		return "AddPackage";
	}
}

class RemovePackage extends PackageChange {
	type() {
		return "RemovePackage";
	}
}

class RenamePackage extends PackageChange {
	constructor() {
		super();
		this.newName = null;
	}

	type() {
		return "RenamePackage";
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
