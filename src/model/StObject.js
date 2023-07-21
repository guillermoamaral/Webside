class StObject extends Object {
	constructor() {
		super();
		this.id = null;
		this.class = null;
		this.hasNamedSlots = false,
		this.hasIndexedSlots = false;
		this.size = null;
		this.printString = "";
	}

	fromJson(json) {
		this.id = json.id;
		this.class = json.class;
		this.hasNamedSlots = json.hasNamedSlots;
		this.hasIndexedSlots = json.hasIndexedSlots;
		this.size = json.size;
		this.printString = json.printString;
	}
}

export default StObject;
