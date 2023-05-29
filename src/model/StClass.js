class StClass extends StObject {
	constructor() {
		super();
		this.name = null;
		this.definition = null;
		this.superclass = null;
		this.metaclass = null;
		this.comment = "";
		this.variable = false;
		this.package = null;
	}

	fromJson(json) {
		super.fromJson(json);
		this.name = json.name;
		this.definition = json.definition;
		this.superclass = json.superclass;
		this.comment = json.comment;
		this.variable = json.variable;
		this.package = json.package;
	}

	isMeta() {
		return this.name.endsWith(" class");
	}
}

export default StClass;
