class StObject extends Object {
	constructor() {
		super();
		this.id = null;
		this.class = null;
		this.indexable = false;
		this.size = null;
		this.printString = "";
	}

	fromJson(json) {
		this.id = json.id;
		this.class = json.class;
		this.indexable = json.indexable;
		this.size = json.size;
		this.printString = json.printString;
	}
}

export default StObject;
