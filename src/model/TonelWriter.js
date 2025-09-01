import StParser from "./StParser";

class TonelWriterV3 extends Object {
	writeMethod(method) {
		const header = `{ #category : '${method.category}' }`;
		const bodyIndex = StParser.methodBodyIndex(
			method.selector,
			method.source
		);
		const methodHeader = `${method.methodClass} >> ${method.source
			.substring(0, bodyIndex)
			.trim()} [`;
		const body = method.source
			.substring(bodyIndex)
			.trim()
			.split("\n")
			.map((line) => "\t" + line.trim())
			.join("\n");
		return `${header}\n${methodHeader}\n${body}\n]`;
	}

	writeClassDefinition(species) {
		return `Class {
	  \t#name : '${species.name}',
	  \t#superclass : '${species.superclass}',
	  \t#category : '${species.category}',
	  \t#package : '${species.package}',
	  \t#instVars : [${species.instanceVariables.map((v) => `'${v}'`).join(", ")}]
	  \t#classVars : [${species.classVariables.map((v) => `'${v}'`).join(", ")}]
	  }`;
	}

	writeClass(species) {
		const classHeader = this.writeClassDefinition(species);
		const classMethodsText = species.classMethods
			.map((m) => this.writeMethod(m))
			.join("\n\n");
		const methodsText = species.methods
			.map((m) => this.writeMethod(m))
			.join("\n\n");
		return `${classHeader}\n\n${methodsText}\n\n${classMethodsText}`;
	}

	writePackage(pack) {
		return `Package {
	\t#name : '${pack.name}'
	}`;
	}
}

export default TonelWriterV3;
