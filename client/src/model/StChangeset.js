import StChange from "./StChange";

class StChangeset extends Object {
	constructor() {
		super();
		this.system = null;
		this.changes = [];
	}

	static fromJson(json) {
		var changeset = new StChangeset();
		changeset.fromJson(json);
		return changeset;
	}

	fromJson(json) {
		this.changes = json.map((j) => {
			const change = StChange.fromJson(j);
			change.changeset = this;
			return change;
		});
	}

	size() {
		return this.changes.length;
	}

	on(system) {
		this.system = system;
	}
}

export default StChangeset;
