import Change from "./Change";

class Changeset extends Object {
	constructor() {
		super();
		this.api = null;
		this.changes = [];
	}

	static fromJson(json) {
		var changeset = new Changeset();
		changeset.fromJson(json);
		return changeset;
	}

	fromJson(json) {
		this.changes = json.map((j) => {
			const change = Change.fromJson(j);
			change.changeset = this;
			return change;
		});
	}

	api(api) {
		this.api = api;
	}
}

export default Changeset;
