import StChangeset from "../../model/StChangeset";

class AICodeGenerationResult {
	constructor() {
		this.thinking = null;
		this.issue = null;
		this.classes = [];
		this.methods = [];
		this.scripts = [];
	}

	isEmpty() {
		return (
			this.classes.length === 0 &&
			this.methods.length === 0 &&
			this.scripts.length === 0
		);
	}

	hasOnlyClassNames() {
		if (
			this.classes.length === 0 ||
			this.methods.length !== 0 ||
			this.scripts.length !== 0
		)
			return false;
		return this.classes.filter((c) => !c.hasOnlyName()).length === 0;
	}

	hasOnlySignatures() {
		if (
			this.classes.length !== 0 ||
			this.methods.length === 0 ||
			this.scripts.length !== 0
		)
			return false;
		return this.methods.filter((m) => !m.hasOnlySignature()).length === 0;
	}

	classLinks() {
		let names = "";
		let first = true;
		this.classes.forEach((c) => {
			if (first) {
				first = false;
			} else {
				names += "\n\n";
			}
			names += `* [${c.name}](internalLink?classname=${c.name})`;
		});
		return names;
	}

	signatureLinks() {
		let signatures = "";
		let first = true;
		this.methods.forEach((m) => {
			if (first) {
				first = false;
			} else {
				signatures += "\n\n";
			}
			signatures += `* [${m.methodClass.name}>>#${m.selector}](internalLink?classname=${m.methodClass.name}&selector=${m.selector})`;
		});
		return signatures;
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
			chunk += m.source;
		});
		this.scripts.forEach((s, i) => {
			if (i !== 0) chunk += `\n\n`;
			chunk += s.source;
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
