class AISuggestedScript {
	constructor(sourceCode = "") {
		this.sourceCode = sourceCode;
	}

	codeChunk() {
		return this.sourceCode;
	}
}

export default AISuggestedScript;
