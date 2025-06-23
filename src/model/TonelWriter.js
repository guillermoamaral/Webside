class TonelWriterV3 extends Object {
	constructor() {
		super();
	}

	writeMethod(method) {
		const header = `{ #category : '${method.category}' }`;
		const bodyIndex = this.methodBodyIndex(method);
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

	methodBodyIndex(method) {
		const binarySelectors = [
			"+",
			"-",
			"*",
			"/",
			"\\",
			"~",
			"<",
			">",
			"<=",
			">=",
			"=",
			"~=",
			"==",
			"~~",
			"@",
			"->",
			"&",
			"|",
			",",
			"<<",
			">>",
		];
		const { selector, source } = method;
		let index;
		if (selector.includes(":")) {
			const keywords = selector
				.split(":")
				.filter(Boolean)
				.map((k) => k + ":");
			for (const keyword of keywords) {
				const found = source.indexOf(keyword, index);
				if (found === -1) {
					throw new Error(`Keyword ${keyword} not found in source`);
				}
				index = found + keyword.length;
			}
			while (index < source.length && /\s/.test(source[index])) index++;
			while (index < source.length && /\S/.test(source[index])) index++;
			while (
				index < source.length &&
				(source[index] === "\r" || source[index] === "\n")
			)
				index++;
			return index;
		}

		if (binarySelectors.includes(selector)) {
			index = source.indexOf(selector);
			if (index === -1) {
				throw new Error(
					`Binary selector ${selector} not found in source`
				);
			}
			index = index + selector.length;
			while (index < source.length && /\s/.test(source[index])) index++;
			while (index < source.length && /\S/.test(source[index])) index++;
			while (
				index < source.length &&
				(source[index] === "\r" || source[index] === "\n")
			)
				index++;
			return index;
		}

		// Unary
		index = source.indexOf(selector);
		if (index === -1) {
			throw new Error(`Unary selector ${selector} not found in source`);
		}
		return index + selector.length;
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
