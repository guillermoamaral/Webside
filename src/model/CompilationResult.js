class CompilationResult extends Object {
	constructor(method, error) {
		super();
		this.method = method;
		this.error = error;
	}

	hasError() {
		return this.error !== null && this.error !== undefined;
	}
}

export default CompilationResult;
