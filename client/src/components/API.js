import axios from "axios";

class APIError extends Error {
	constructor(description, url, request, status, reason, data) {
		const explanation = reason && reason.lenght > 0 ? " due to " + reason : "";
		const message = '"' + description + " from " + url + explanation + '"';
		super(message);
		this.name = "APIError";
		this.url = url;
		this.request = request;
		this.status = status;
		this.reason = reason;
		this.data = data;
	}
}

class API {
	constructor(uri, author, reportError, reportChange) {
		this.baseUri = uri;
		this.reportError = reportError.bind();
		this.reportChange = reportChange.bind();
		this.author = author;
	}

	baseUri(uri) {
		this.baseUri = uri;
	}

	author(author) {
		this.author = author;
	}

	async get(uri, description) {
		try {
			const response = await axios.get(this.baseUri + uri);
			return response.data;
		} catch (error) {
			this.handleError("Cannot get " + (description || uri), uri, error);
		}
	}

	async post(uri, payload, description) {
		try {
			const response = await axios.post(this.baseUri + uri, payload);
			return response.data;
		} catch (error) {
			this.handleError("Cannot " + (description || " post " + uri), uri, error);
		}
	}

	async delete(uri, description) {
		try {
			const response = await axios.delete(this.baseUri + uri);
			return response.data;
		} catch (error) {
			this.handleError("Cannot delete " + (description || uri), uri, error);
		}
	}

	handleError(description, uri, error) {
		var status, reason, data;
		if (error.response) {
			status = error.response.status;
			reason = error.response.statusText;
			data = error.response.data;
		} else if (error.request) {
			reason = error.message;
		}
		const exception = new APIError(
			description,
			this.baseUri + uri,
			error.request,
			status,
			reason,
			data
		);
		throw exception;
	}

	async getDialect() {
		return await this.get("/dialect", "dialect");
	}

	// Code...
	async getPackageNames() {
		return await this.get("/packages?names=true", "package names");
	}

	async getPackage(packagename) {
		return await this.get("/packages/" + packagename, "package " + packagename);
	}

	async getPackageClasses(packagename, extended = false) {
		return await this.get(
			"/packages/" + packagename + "/classes?tree=true&extended=" + extended,
			"classes from package " + packagename
		);
	}

	async getClassTree(root, depth) {
		const tree = await this.get(
			"/classes?names=true&root=" + root + "&tree=true&depth=" + depth,
			"class tree from " + root
		);
		return tree[0];
	}

	async getClassTree2(root, depth) {
		const species = typeof root === "string" ? await this.getClass(root) : root;
		if (depth === 0) {
			return species;
		}
		species.subclasses = await this.getSubclasses(species.name);
		await Promise.all(
			species.subclasses.map(async (c) => {
				await this.getClassTree2(c, depth - 1);
			})
		);
		return species;
	}

	async getClassNames() {
		return await this.get("/classes?names=true", "class names");
	}

	async getClass(classname) {
		return await this.get("/classes/" + classname, "class " + classname);
	}

	async getSubclasses(classname) {
		return await this.get(
			"/classes/" + classname + "/subclasses",
			"subclasses of class " + classname
		);
	}

	async getInstanceVariables(classname) {
		return await this.get(
			"/classes/" + classname + "/instance-variables",
			"instance variables of class " + classname
		);
	}

	async getVariables(classname) {
		return await this.get(
			"/classes/" + classname + "/variables",
			"variables of class " + classname
		);
	}

	async getCategories(classname) {
		return await this.get(
			"/classes/" + classname + "/categories",
			"categories of class " + classname
		);
	}

	async getMethods(classname, sorted = false) {
		const methods = await this.get(
			"/classes/" + classname + "/methods?marks=true",
			"methods of class " + classname
		);
		if (sorted) {
			methods.sort((a, b) => (a.selector <= b.selector ? -1 : 1));
		}
		return methods;
	}

	async getMethodsAccessing(classname, variable, type, sorted = false) {
		const methods = await this.get(
			"/classes/" + classname + "/methods?" + type + "=" + variable,
			"methods of class " + classname + " using " + variable
		);
		if (sorted) {
			methods.sort((a, b) => (a.selector <= b.selector ? -1 : 1));
		}
		return methods;
	}

	async getMethod(classname, selector) {
		const encoded = encodeURIComponent(selector);
		const methods = await this.get(
			"/classes/" +
				classname +
				"/methods?selector=" +
				encoded +
				"&bytecodes=true&disassembly=true&ast=true",
			"method " + classname + ">>#" + selector
		);
		return methods.length === 0 ? null : methods[0];
	}

	async getSenders(selector) {
		return await this.get(
			"/methods?sending=" + selector,
			"senders of " + selector
		);
	}

	async getLocalSenders(selector, classname) {
		return await this.get(
			"/methods?sending=" + selector + "&scope=" + classname,
			"senders of " + selector + " in class " + classname
		);
	}

	async getClassReferences(classname) {
		return await this.get(
			"/methods?referencingClass=" + classname,
			"references to class " + classname
		);
	}

	async getStringReferences(string) {
		return await this.get(
			"/methods?referencingString=" + string,
			"references to string " + string
		);
	}

	async getImplementors(selector) {
		return await this.get(
			"/methods?selector=" + selector,
			"implementors of " + selector
		);
	}

	async getLocalImplementors(selector, classname) {
		return await this.get(
			"/methods?selector=" + selector + "&scope=" + classname,
			"local implementors of " + selector + " in class " + classname
		);
	}

	methodTemplate() {
		// Retrieve from back-end to support custom templates for each dialect...
		return {
			selector: "<new>",
			template: true,
			source: 'messagePattern\r\t"comment"\r\t| temporaries |\r\tstatements',
		};
	}

	// Debugging...
	async getDebuggers() {
		return await this.get("/debuggers", "debuggers");
	}

	async createDebugger(id) {
		const evaluation = { evaluation: id };
		return await this.post(
			"/debuggers",
			evaluation,
			"create debugger on evaluation " + id
		);
	}

	async getDebuggerFrames(id) {
		return await this.get(
			"/debuggers/" + id + "/frames",
			"frames of debugger " + id
		);
	}

	async getDebuggerFrame(id, index) {
		return await this.get(
			"/debuggers/" + id + "/frames/" + index,
			"frame " + index + " in debugger " + id
		);
	}

	async getFrameBindings(id, index) {
		return await this.get(
			"/debuggers/" + id + "/frames/" + index + "/bindings",
			"bindings of frame " + index + " in debugger " + id
		);
	}

	async stepIntoDebugger(id, index) {
		return await this.post(
			"/debuggers/" + id + "/frames/" + index + "/stepinto",
			"step into on frame " + index + " of debugger " + id
		);
	}

	async stepOverDebugger(id, index) {
		return await this.post(
			"/debuggers/" + id + "/frames/" + index + "/stepover",
			"step over on frame " + index + " of debugger " + id
		);
	}

	async restartDebugger(id, index, update = false) {
		return await this.post(
			"/debuggers/" + id + "/frames/" + index + "/restart?update=" + update,
			"restart on frame " + index + " of debugger " + id
		);
	}

	async resumeDebugger(id) {
		return await this.post(
			"/debuggers/" + id + "/resume",
			"resume debugger " + id
		);
	}

	async terminateDebugger(id) {
		return await this.post(
			"/debuggers/" + id + "/terminate",
			"terminate debugger " + id
		);
	}

	async deleteDebugger(id) {
		return await this.delete("/debuggers/" + id, "debugger " + id);
	}

	// Workspaces...
	async getWorkspaces() {
		return await this.get("/workspaces", "workspaces");
	}

	async createWorkspace() {
		return await this.post("/workspaces", "create workspace");
	}

	async deleteWorkspace(id) {
		return await this.delete("/workspaces/" + id, "workspace " + id);
	}

	// Changes...
	async getChanges() {
		return await this.get("/changes", "changes");
	}

	newChange(type) {
		return {
			type: type,
			author: this.author,
		};
	}

	async postChange(change, description) {
		const applied = await this.post("/changes", change, description);
		this.reportChange(applied);
		return applied;
	}

	// Change helpers...
	async defineClass(classname, packagename, definition) {
		const change = this.newChange("ClassDefinition");
		change.class = classname;
		change.package = packagename;
		change.definition = definition;
		return await this.postChange(change, "define class " + classname);
	}

	async commentClass(classname, comment) {
		const change = this.newChange("ClassCommentDefinition");
		change.class = classname;
		change.comment = comment;
		return await this.postChange(change, "comment class " + classname);
	}

	async removeClass(classname) {
		const change = this.newChange("ClassRemove");
		change.class = classname;
		return await this.postChange(change, "remove class " + classname);
	}

	async renameClass(classname, newName, renameReferences = true) {
		const change = this.newChange("ClassRename");
		change.class = classname;
		change.newName = newName;
		change.renameReferences = renameReferences;
		return await this.postChange(change, "rename class " + classname);
	}

	async addInstanceVariable(classname, variable) {
		const change = this.newChange("InstanceVariableAddition");
		change.class = classname;
		change.variable = variable;
		return await this.postChange(
			change,
			"add instance variable " + variable + " to " + classname
		);
	}

	async addClassVariable(classname, variable) {
		const change = this.newChange("ClassVariableAddition");
		change.class = classname;
		change.variable = variable;
		return await this.postChange(
			change,
			"add class variable " + variable + " to " + classname
		);
	}

	async renameInstanceVariable(classname, variable, newName) {
		const change = this.newChange("InstanceVariableRename");
		change.class = classname;
		change.variable = variable;
		change.newName = newName;
		return await this.postChange(
			change,
			"rename instance variable " +
				variable +
				" to " +
				newName +
				" of class " +
				classname
		);
	}

	async renameClassVariable(classname, variable, newName) {
		const change = this.newChange("ClassVariableRename");
		change.class = classname;
		change.variable = variable;
		change.newName = newName;
		return await this.postChange(
			change,
			"rename class variable " +
				variable +
				" to " +
				newName +
				" of class " +
				classname
		);
	}

	async removeInstanceVariable(classname, variable) {
		const change = this.newChange("InstanceVariableRemove");
		change.class = classname;
		change.variable = variable;
		return await this.postChange(
			change,
			"remove instance variable " + variable + " from class " + classname
		);
	}

	async removeClassVariable(classname, variable) {
		const change = this.newChange("ClassVariableRemove");
		change.class = classname;
		change.variable = variable;
		return await this.postChange(
			change,
			"remove class variable " + variable + " from class " + classname
		);
	}

	async moveInstanceVariableUp(classname, variable) {
		const change = this.newChange("InstanceVariableMoveUp");
		change.class = classname;
		change.variable = variable;
		return await this.postChange(
			change,
			"move up variable " + variable + " from class " + classname
		);
	}

	async moveInstanceVariableDown(classname, variable, target) {
		const change = this.newChange("InstanceVariableMoveDown");
		change.class = classname;
		change.variable = variable;
		change.target = target;
		return await this.postChange(
			change,
			"move down variable " + variable + " from class " + classname
		);
	}

	async renameCategory(classname, category, newName) {
		const change = this.newChange("CategoryRename");
		change.class = classname;
		change.category = category;
		change.newName = newName;
		return await this.postChange(
			change,
			"rename category " +
				category +
				" to " +
				newName +
				" of class " +
				classname
		);
	}

	async removeCategory(classname, category) {
		const change = this.newChange("CategoryRemove");
		change.class = classname;
		change.category = category;
		return await this.postChange(
			change,
			"remove category " + category + " from class " + classname
		);
	}

	async compileMethod(classname, packagename, category, source) {
		const change = this.newChange("MethodDefinition");
		change.class = classname;
		change.package = packagename;
		change.category = category;
		change.sourceCode = source;
		return await this.postChange(
			change,
			"compile " + source + " in " + classname
		);
	}

	async removeMethod(classname, selector) {
		const change = this.newChange("MethodRemove");
		change.class = classname;
		change.selector = selector;
		return await this.postChange(
			change,
			"remove methodd " + classname + ">>#" + selector
		);
	}

	async classifyMethod(classname, selector, category) {
		const change = this.newChange("MethodClassification");
		change.class = classname;
		change.selector = selector;
		change.category = category;
		return await this.postChange(
			change,
			"classify methodd " + classname + ">>#" + selector + " under " + category
		);
	}

	async renameSelector(classname, selector, newSelector) {
		const change = this.newChange("SelectorRename");
		change.class = classname;
		change.selector = selector;
		change.newSelector = newSelector;
		return await this.postChange(
			change,
			"rename selector " + selector + " to " + newSelector
		);
	}

	// Evaluations...
	async evaluateExpression(expression, sync = false, pin = false, context) {
		const evaluation = {
			expression: expression,
			context: context,
			sync: sync,
			pin: pin,
		};
		return await this.post(
			"/evaluations",
			evaluation,
			"evaluate " + expression
		);
	}

	async cancelEvaluation(id) {
		return await this.delete(
			"/evaluations/" + id,
			"cancel evaluation with id " + id
		);
	}

	async getEvaluations() {
		return await this.get("/evaluations", "retrieve evaluations");
	}

	async debugExpression(expression, context) {
		const evaluation = {
			expression: expression,
			context: context,
			debug: true,
			pin: false,
		};
		return await this.post("/evaluations", evaluation, "debug " + expression);
	}

	async profileExpression(expression, context) {
		const evaluation = {
			expression: expression,
			context: context,
			profile: true,
			pin: false,
		};
		return await this.post("/evaluations", evaluation, "profile " + expression);
	}

	// Objects...
	async getObjects() {
		return await this.get("/objects", "objects");
	}

	async getObject(id) {
		return await this.get("/objects/" + id, "object with id " + id);
	}

	async unpinObject(id) {
		return await this.delete("/objects/" + id, "unpin object with id " + id);
	}

	async unpinAllObjects() {
		return await this.delete("/objects/", "unpin all objects");
	}

	async getObjectNamedSlots(id, path) {
		const response = await this.getObjectSlot(id, path + "/named-slots");
		return response;
	}

	async getObjectIndexedSlots(id, path) {
		const response = await this.getObjectSlot(id, path + "/indexed-slots");
		return response;
	}

	async getObjectInstanceVariables(id, path) {
		const response = await this.getObjectSlot(id, path + "/instance-variables");
		return response;
	}

	async getObjectSlot(id, path) {
		return await this.get(
			"/objects/" + id + path,
			path + " of object with id " + id
		);
	}

	async pinObjectSlot(id, path) {
		const uri = "/objects/" + id + path;
		const body = { uri: uri };
		return await this.post("/objects", body, "pin slot at URI /objects" + uri);
	}

	// Tests...
	async getTestRuns() {
		return await this.get("/test-runs", "test runs");
	}

	async runTestSuite(suite) {
		return await this.post("/test-runs", suite, "run test suite " + suite);
	}

	async runTest(classname, selector) {
		const suite = {
			methods: [{ class: classname, selector: selector }],
		};
		return await this.runTestSuite(suite);
	}

	async runTestClass(classname) {
		const suite = {
			classes: [classname],
		};
		return await this.runTestSuite(suite);
	}

	async runTestPackage(packagename) {
		const suite = {
			packages: [packagename],
		};
		return await this.runTestSuite(suite);
	}

	async getTestRunStatus(id) {
		return await this.get(
			"/test-runs/" + id + "/status",
			"status of test run " + id
		);
	}

	async getTestRunResults(id) {
		return await this.get(
			"/test-runs/" + id + "/results",
			"results of test run " + id
		);
	}

	async runTestRun(id) {
		return await this.post("/test-runs/" + id + "/run", "run test run " + id);
	}

	async stopTestRun(id) {
		return await this.post("/test-runs/" + id + "/stop", "stop test run " + id);
	}

	async deleteTestRun(id) {
		return await this.delete("/test-runs/" + id, "test run " + id);
	}

	async debugTest(id, classname, selector) {
		const test = {
			class: classname,
			selector: selector,
		};
		return await this.post(
			"/test-runs/" + id + "/debug",
			test,
			"debug test " + selector + " in " + classname
		);
	}

	// Profiling...
	async getProfilerTreeResults(id) {
		return await this.get(
			"/profilers/" + id + "/tree",
			"tree results of profiler " + id
		);
	}

	async getProfilerRankingResults(id) {
		return await this.get(
			"/profilers/" + id + "/ranking",
			"ranking results of profiler " + id
		);
	}

	//Native debugging...
	async getNativeDebugger(id) {
		return await this.get("/native-debuggers/" + id, "native debugger " + id);
	}

	async getNativeDebuggerFrames(id) {
		return await this.get(
			"/native-debuggers/" + id + "/frames",
			"frames of native debugger " + id
		);
	}

	async getNativeDebuggerRegisters(id) {
		return await this.get(
			"/native-debuggers/" + id + "/registers",
			"registers of native debugger " + id
		);
	}

	async getNativeDebuggerSpaces(id) {
		return await this.get(
			"/native-debuggers/" + id + "/spaces",
			"spaces of native debugger " + id
		);
	}

	async getNativeDebuggerFrame(id, index) {
		return await this.get(
			"/native-debuggers/" + id + "/frames/" + index,
			"frame " + index + " in native debugger " + id
		);
	}

	async resumeNativeDebugger(id) {
		return await this.post(
			"/native-debuggers/" + id + "/resume",
			"resume native debugger " + id
		);
	}

	async suspendNativeDebugger(id) {
		return await this.post(
			"/native-debuggers/" + id + "/suspend",
			"suspend native debugger " + id
		);
	}

	async pinNativeDebuggerRegister(id, register) {
		return await this.post(
			"/native-debuggers/" + id + "/registers/" + register + "/pin",
			"pin object pointed by register " + register + " of native debugger " + id
		);
	}

	//Memory stats...
	async getMemoryStats(last) {
		const query = last ? "?last=" + last : "";
		return await this.get("/memory-stats" + query, "memory stats");
	}
}

export default API;
