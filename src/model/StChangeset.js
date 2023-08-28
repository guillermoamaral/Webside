import { StChange } from "./StChange";

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

	asJson() {
		return this.changes.map((ch) => ch.asJson());
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

	rejectChange(change) {
		if (!this.originalChanges) {
			this.originalChanges = this.changes;
		}
		this.changes = this.changes.filter((ch) => ch !== change);
	}

	async compress() {
		if (!this.originalChanges) {
			this.originalChanges = this.changes;
		}
		const compressed = await this.system.compressChanges(this.asJson());
		this.fromJson(compressed);
	}

	async update() {
		const updated = await this.system.updateChanges(this.asJson());
		this.fromJson(updated);
	}

	async rejectUpToDate() {
		if (!this.originalChanges) {
			this.originalChanges = this.changes;
		}
		await this.update();
		this.changes = this.changes.filter((ch) => {
			return !ch.isUpToDate();
		});
	}

	filterChanges(filters) {
		if (!this.originalChanges) {
			this.originalChanges = this.changes;
		}
		var filtered = [...this.changes];
		filters.forEach((f) => (filtered = filtered.filter(f.function)));
		this.changes = filtered;
	}
}

export default StChangeset;
