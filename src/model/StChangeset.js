import { StChange } from "./StChange";

class StChangeset extends Object {
	constructor(backend) {
		super();
		this.backend = backend;
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
		this.changes = [];
		json.forEach((j) => {
			const change = StChange.fromJson(j);
			this.addChange(change);
		});
	}

	addChange(change) {
		change.changeset = this;
		this.changes.push(change);
	}

	addChanges(changes) {
		changes.forEach((ch) => this.addChange(ch));
	}

	size() {
		return this.changes.length;
	}

	on(system) {
		this.backend = system;
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
		const compressed = await this.backend.compressChanges(this.asJson());
		this.fromJson(compressed);
	}

	async update() {
		const updated = await this.backend.updateChanges(this.asJson());
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
		if (!this.originalChanges) this.originalChanges = this.changes;
		let filtered = [...this.originalChanges];
		filters.forEach((f) => (filtered = filtered.filter(f.function)));
		this.changes = filtered;
	}

	async applyChange(change) {
		const applied = await this.backend.postChange(change.asJson());
		change.fromJson(applied);
	}

	async updateChange(change) {
		const updated = await this.backend.updateChanges([change.asJson()]);
		change.fromJson(updated[0]);
	}
}

export default StChangeset;
