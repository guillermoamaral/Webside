class StVariable extends Object {
	constructor() {
		super();
		this.name = "";
		this.type = null;
	}

	fromJson(json) {
		this.name = json.name;
		this.type = json.type;
	}

	isInstanceVariable() {
		return this.type === "instance";
	}

	isClassVariable() {
		return this.type === "class";
	}
}

export default StVariable;
