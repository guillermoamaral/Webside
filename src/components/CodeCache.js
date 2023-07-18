class CodeCache {
	constructor(backend) {
		this.backend = backend;
		this.cache = {};
	}

	async classNamed(classname) {
		const cached = this.cache[classname];
		if (cached) return cached;
		const retrieved = await this.backend.classNamed(classname);
		this.cache[classname] = retrieved;
		retrieved.metaclass = await this.backend.classNamed(retrieved.class);
		return retrieved;
	}

	invalidateClass(classname) {
		delete this.cache[classname];
	}

	async subclasses(classname) {
		const cached = this.classNamed(classname);
		if (cached.subclasses) return cached.subclasses;
		const retrieved = await this.backend.subclasses(classname);
		cached.subclasses = retrieved;
		return retrieved;
	}

	async variables(classname) {
		const cached = this.classNamed(classname);
		if (cached.variables) return cached.variables;
		const retrieved = await this.backend.variables(classname);
		cached.variables = retrieved;
		return retrieved;
	}

	async categories(classname) {
		const cached = this.classNamed(classname);
		if (cached.categories) return cached.categories;
		const retrieved = await this.backend.categories(classname);
		cached.categories = retrieved;
		return retrieved;
	}

	async methods(classname, sorted) {
		const cached = this.classNamed(classname);
		if (cached.methods) return cached.methods;
		const retrieved = await this.backend.methods(classname, sorted);
		cached.methods = retrieved;
		return retrieved;
	}

	async methodsAccessing(classname, variable, type, sorted) {
		const cached = this.classNamed(classname);
		if (
			cached.accessors &&
			cached.accessors[variable] &&
			cached.accessors[variable][type]
		)
			return cached.accessors[variable][type];
		const retrieved = await this.backend.methodsAccessing(
			classname,
			variable,
			type,
			sorted
		);
		if (!cached.accessors) cached.accessors = {};
		if (!cached.accessors[variable]) cached.accessors[variable] = {};
		cached.accessors[variable][type] = retrieved;
		return retrieved;
	}

	async method(classname, selector) {
		const cached = this.classNamed(classname);
		if (cached.methods) {
			const method = cached.methods.find((m) => m.selector === selector);
			if (method) return method;
		}
		const retrieved = this.backend.method(classname, selector);
		if (!cached.methods) cached.methods = [];
		cached.methods.push(method);
		return retrieved;
	}
}

export default CodeCache;
