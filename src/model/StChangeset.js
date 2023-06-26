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

	compress() {
		if (!this.originalChanges) {
			this.originalChanges = this.changes;
		}
		var compressed = [];
		this.changes.forEach((ch) => {
			compressed = compressed.filter((ch2) => !ch.canOverride(ch2));
			compressed.push(ch);
		});
		this.changes = compressed;
	}

	async rejectUpToDate() {
		if (!this.originalChanges) {
			this.originalChanges = this.changes;
		}
		await this.updateCurrentSourceCode();
		const newer = this.changes.filter((ch) => {
			return !ch.isUpToDate();
		});
		this.changes = newer;
	}

	filterChanges(filters) {
		if (!this.originalChanges) {
			this.originalChanges = this.changes;
		}
		var filtered = [...this.changes];
		filters.forEach((f) => (filtered = filtered.filter(f.function)));
		this.changes = filtered;
	}

	async updateCurrentSourceCode() {
		await Promise.all(
			this.changes.map(async (ch) => {
				await ch.updateCurrentSourceCode();
			})
		);
	}
}

export default StChangeset;
