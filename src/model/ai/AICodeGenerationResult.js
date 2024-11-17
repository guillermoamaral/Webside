import StChangeset from "../../model/StChangeset";

class AICodeGenerationResult {
	constructor() {
		this.thinking = null;
		this.issue = null;
		this.classes = [];
		this.methods = [];
		this.scripts = [];
	}

	addClass(suggestedClass) {
		this.classes.push(suggestedClass);
	}

	addMethod(suggestedMethod) {
		this.methods.push(suggestedMethod);
	}

	addScript(suggestedScript) {
		this.scripts.push(suggestedScript);
	}

	hasIssues() {
		return this.issue !== null;
	}

	text() {
		let text = "";
		if (this.thinking) text += `*Rationale*\n${this.thinking}\n`;
		if (this.issue) text += `*Issue*\n${this.issue}`;
		return text;
	}

	codeChunk() {
		let chunk = "";
		this.classes.forEach((c, i) => {
			if (i !== 0) chunk += `\n\n`;
			chunk += c.codeChunk();
		});
		this.methods.forEach((m, i) => {
			if (i !== 0) chunk += `\n\n`;
			chunk += m.sourceCode;
		});
		this.scripts.forEach((s, i) => {
			if (i !== 0) chunk += `\n\n`;
			chunk += s.sourceCode;
		});
		return chunk;
	}

	codeChunks() {
		const chunks = [];
		this.classes.forEach((c) => chunks.push(...c.codeChunks()));
		this.methods.forEach((m) => chunks.push(m.codeChunk()));
		this.scripts.forEach((s) => chunks.push(s.codeChunk()));
		return chunks;
	}

	changes() {
		const changes = [];
		this.classes.forEach((c) => changes.push(...c.changes()));
		changes.push(...this.methods.map((m) => m.change()));
		return changes;
	}

	changeset(backend) {
		const changeset = new StChangeset(backend);
		changeset.addChanges(this.changes());
		return changeset;
	}
}

export default AICodeGenerationResult;
