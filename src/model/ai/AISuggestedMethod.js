import { AddMethod } from "../../model/StChange";

class AISuggestedMethod {
	constructor(species, selector, source = "") {
		this.methodClass = species;
		this.selector = selector;
		this.source = source;
	}

	hasOnlySignature() {
		return (
			this.methodClass &&
			this.selector &&
			(!this.source || this.source === "")
		);
	}

	compile() {
		// Not implemented yet
	}

	install() {
		// Not implemented yet
	}

	realClass() {
		return this.methodClass ? this.methodClass.realClass() : null;
	}

	canBeCompiled() {
		// Not implemented yet
		if (!this.methodClass || !this.methodClass.exists()) {
			return false;
		}
		return false;
	}

	codeChunk() {
		return `${this.methodClass?.name + "\n" || ""}${this.source}`;
	}

	change() {
		const change = new AddMethod();
		change.className = this.methodClass ? this.methodClass.name : "Object";
		change.source = this.source;
		return change;
	}
}

export default AISuggestedMethod;
