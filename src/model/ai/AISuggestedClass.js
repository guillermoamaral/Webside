import { AddClass } from "../../model/StChange";

class AISuggestedClass {
	constructor(name) {
		this.name = name;
		this.superclassName = null;
		this.instanceVariableNames = [];
		this.classVariableNames = [];
		this.methods = [];
	}

	hasOnlyName() {
		return (
			this.name &&
			this.name !== "" &&
			(!this.superclassName || this.superclassName === "") &&
			this.instanceVariableNames == [] &&
			this.classVariableNames == [] &&
			this.methods == []
		);
	}

	install() {
		// Not implemented yet
	}

	exists() {
		return this.realClass() !== undefined;
	}

	realClass() {
		// Not implemented yet
	}

	installMethods() {
		this.methods.forEach((method) => method.install());
	}

	canBeInstalled() {
		// Not implemented yet
		// return Smalltalk.globals.hasOwnProperty(this.superclassName);
	}

	addMethod(method) {
		this.methods.push(method);
		method.class = this;
	}

	codeChunk() {
		let chunk = `${this.superclassName || "Object"}\n\tsubclass: #${
			this.name
		}\n\tinstanceVariableNames:'`;
		this.instanceVariableNames.forEach((v, i) => {
			if (i !== 0) chunk += " ";
			chunk += v;
		});
		chunk += `'\n\tclassVariableNames:'`;
		this.classVariableNames.forEach((v, i) => {
			if (i !== 0) chunk += " ";
			chunk += v;
		});
		chunk += `'\n\n`;
		this.methods.forEach((m, i) => {
			if (i !== 0) chunk += `\n\n`;
			chunk += m.source;
		});
		return chunk;
	}

	codeChunks() {
		return [this.codeChunk(), ...this.methods.map((m) => m.codeChunk())];
	}

	changes() {
		const changes = [];
		const creation = new AddClass();
		creation.className = this.name;
		creation.superclass = this.superclassName;
		creation.instanceVariables = this.instanceVariableNames;
		creation.classVariables = this.classVariableNames;
		changes.push(creation);
		changes.push(...this.methods.map((m) => m.change()));
		return changes;
	}
}

export default AISuggestedClass;
