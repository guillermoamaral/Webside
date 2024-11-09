import { AddMethod } from "../../model/StChange";

class AISuggestedMethod {
	constructor(species, sourceCode = "") {
		this.class = species;
		this.sourceCode = sourceCode;
	}

	compile() {
		// Not implemented yet
	}

	install() {
		// Not implemented yet
	}

	realClass() {
		return this.class ? this.class.realClass() : null;
	}

	canBeCompiled() {
		// Not implemented yet
		if (!this.class || !this.class.exists()) {
			return false;
		}
		return false;
	}

	selector() {
		// Not implemented yet
		// return AICodeAssistant.parseSelector(this.sourceCode);
	}

	codeChunk() {
		return `${this.class?.name + "\n" || ""}${this.sourceCode}`;
	}

	change() {
		const change = new AddMethod();
		change.className = this.class ? this.class.name : "Object";
		change.source = this.sourceCode;
		return change;
	}
}

export default AISuggestedMethod;
