import axios from "axios";

class APIError extends Error {
	constructor(description, url, request, status, reason, data) {
		const explanation =
			reason && reason.lenght > 0 ? " due to " + reason : "";
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
		this.backend = uri;
		this.reportError = reportError ? reportError.bind() : null;
		this.reportChange = reportChange ? reportChange.bind() : null;
		this.author = author;
	}

	setBackend(uri) {
		this.backend = uri;
	}

	author(author) {
		this.author = author;
	}

	async get(uri, description) {
		try {
			const response = await axios.get(this.backend + uri);
			return response.data;
		} catch (error) {
			this.handleError("Cannot get " + (description || uri), uri, error);
		}
	}

	async post(uri, payload, description) {
		try {
			const response = await axios.post(this.backend + uri, payload);
			return response.data;
		} catch (error) {
			this.handleError(
				"Cannot " + (description || " post " + uri),
				uri,
				error
			);
		}
	}

	async put(uri, payload, description) {
		try {
			const response = await axios.put(this.backend + uri, payload);
			return response.data;
		} catch (error) {
			this.handleError(
				"Cannot " + (description || " put " + uri),
				uri,
				error
			);
		}
	}

	async delete(uri, description) {
		try {
			const response = await axios.delete(this.backend + uri);
			return response.data;
		} catch (error) {
			this.handleError(
				"Cannot delete " + (description || uri),
				uri,
				error
			);
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
			this.backend + uri,
			error.request,
			status,
			reason,
			data
		);
		throw exception;
	}

	// General...
	async dialect() {
		return await this.get("/dialect", "dialect");
	}

	async saveImage() {
		return await this.post("/save", null, "save image");
	}

	// Code...
	async packageNames() {
		return await this.get("/packages?names=true", "package names");
	}

	async packageNamed(packagename) {
		return await this.get(
			"/packages/" + encodeURIComponent(packagename),
			"package "
		);
	}

	async packageClasses(packagename, extended = false) {
		return await this.get(
			"/packages/" +
				packagename +
				"/classes?tree=true&extended=" +
				extended,
			"classes from package " + packagename
		);
	}

	async classTree(root, depth, onlyNames = false) {
		const tree = await this.get(
			"/classes?names=" +
				onlyNames +
				"&root=" +
				root +
				"&tree=true&depth=" +
				depth,
			"class tree from " + root
		);
		return tree[0];
	}

	async classTree2(root, depth) {
		const species =
			typeof root === "string" ? await this.classNamed(root) : root;
		if (depth === 0) {
			return species;
		}
		species.subclasses = await this.subclasses(species.name);
		await Promise.all(
			species.subclasses.map(async (c) => {
				await this.classTree2(c, depth - 1);
			})
		);
		return species;
	}

	async classNames() {
		return await this.get("/classes?names=true", "class names");
	}

	async classNamed(classname) {
		return await this.get("/classes/" + classname, "class " + classname);
	}

	async subclasses(classname) {
		return await this.get(
			"/classes/" + classname + "/subclasses",
			"subclasses of class " + classname
		);
	}

	async instanceVariables(classname) {
		return await this.get(
			"/classes/" + classname + "/instance-variables",
			"instance variables of class " + classname
		);
	}

	async variables(classname) {
		return await this.get(
			"/classes/" + classname + "/variables",
			"variables of class " + classname
		);
	}

	async categories(classname) {
		return await this.get(
			"/classes/" + classname + "/categories",
			"categories of class " + classname
		);
	}

	async usualCategories() {
		return await this.get("/usual-categories", "usual categories ");
	}

	async usedCategories(classname) {
		return await this.get(
			"/classes/" + classname + "/used-categories",
			"categories used in " + classname + " hierarchy"
		);
	}

	async methods(classname, sorted = false) {
		const methods = await this.get(
			"/classes/" + classname + "/methods?marks=true",
			"methods of class " + classname
		);
		if (sorted) {
			methods.sort((a, b) => (a.selector <= b.selector ? -1 : 1));
		}
		return methods;
	}

	async methodsAccessing(classname, variable, type, sorted = false) {
		const methods = await this.get(
			"/classes/" + classname + "/methods?" + type + "=" + variable,
			"methods of class " + classname + " using " + variable
		);
		if (sorted) {
			methods.sort((a, b) => (a.selector <= b.selector ? -1 : 1));
		}
		return methods;
	}

	async method(classname, selector) {
		const encoded = encodeURIComponent(selector);
		const methods = await this.get(
			"/classes/" +
				classname +
				"/methods?selector=" +
				encoded +
				"&bytecodes=true&disassembly=true&ast=true&annotations=true",
			"method " + classname + ">>#" + selector
		);
		return methods.length === 0 ? null : methods[0];
	}

	async senders(selector) {
		return await this.get(
			"/methods?sending=" + selector,
			"senders of " + selector
		);
	}

	async localSenders(selector, classname) {
		return await this.get(
			"/methods?sending=" + selector + "&hierarchy=" + classname,
			"senders of " + selector + " in class " + classname
		);
	}

	async classReferences(classname) {
		return await this.get(
			"/methods?referencingClass=" + classname,
			"references to class " + classname
		);
	}

	async stringReferences(string) {
		return await this.get(
			"/methods?referencingString=" + string,
			"references to string " + string
		);
	}

	async implementors(selector) {
		return await this.get(
			"/methods?selector=" + selector,
			"implementors of " + selector
		);
	}

	async localImplementors(selector, classname) {
		return await this.get(
			"/methods?selector=" + selector + "&hierarchy=" + classname,
			"local implementors of " + selector + " in class " + classname
		);
	}

	async methodsMatching(pattern) {
		return await this.get(
			"/methods?selectorMatching=" + pattern,
			"methods with selector matching " + pattern
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
	async debuggers() {
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

	async debuggerFrames(id) {
		return await this.get(
			"/debuggers/" + id + "/frames",
			"frames of debugger " + id
		);
	}

	async debuggerFrame(id, index) {
		return await this.get(
			"/debuggers/" + id + "/frames/" + index,
			"frame " + index + " in debugger " + id
		);
	}

	async frameBindings(id, index) {
		return await this.get(
			"/debuggers/" + id + "/frames/" + index + "/bindings",
			"bindings of frame " + index + " in debugger " + id
		);
	}

	async stepIntoDebugger(id, index) {
		return await this.post(
			"/debuggers/" + id + "/frames/" + index + "/stepinto",
			null,
			"step into on frame " + index + " of debugger " + id
		);
	}

	async stepOverDebugger(id, index) {
		return await this.post(
			"/debuggers/" + id + "/frames/" + index + "/stepover",
			null,
			"step over on frame " + index + " of debugger " + id
		);
	}

	async stepThroughDebugger(id, index) {
		return await this.post(
			"/debuggers/" + id + "/frames/" + index + "/stepthrough",
			null,
			"step through on frame " + index + " of debugger " + id
		);
	}

	async restartDebugger(id, index, update = false) {
		return await this.post(
			"/debuggers/" +
				id +
				"/frames/" +
				index +
				"/restart?update=" +
				update,
			null,
			"restart on frame " + index + " of debugger " + id
		);
	}

	async resumeDebugger(id) {
		return await this.post(
			"/debuggers/" + id + "/resume",
			null,
			"resume debugger " + id
		);
	}

	async terminateDebugger(id) {
		return await this.post(
			"/debuggers/" + id + "/terminate",
			null,
			"terminate debugger " + id
		);
	}

	async deleteDebugger(id) {
		return await this.delete("/debuggers/" + id, "debugger " + id);
	}

	// Workspaces...
	async workspaces() {
		return await this.get("/workspaces", "workspaces");
	}

	async createWorkspace() {
		return await this.post("/workspaces", null, "create workspace");
	}

	async workspace(id) {
		return await this.get("/workspaces/" + id, "workspace " + id);
	}

	async saveWorkspace(workspace) {
		return await this.put("/workspaces/" + workspace.id, workspace);
	}

	async deleteWorkspace(id) {
		return await this.delete("/workspaces/" + id, "workspace " + id);
	}

	// Changes...
	async lastChanges() {
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
		if (this.reportChange) {
			this.reportChange(applied);
		}
		return applied;
	}

	async downloadChangeset(changes) {
		return await this.post(
			"/changesets/download",
			changes,
			"download changeset"
		);
	}

	async uploadChangeset(changeset) {
		return await this.post(
			"/changesets/upload",
			changeset,
			"upload changeset"
		);
	}

	// Change helpers...
	async createPackage(packagename) {
		const change = this.newChange("AddPackage");
		change.name = packagename;
		return await this.postChange(change, "create package " + packagename);
	}

	async removePackage(packagename) {
		const change = this.newChange("RemovePackage");
		change.name = packagename;
		return await this.postChange(change, "remove package " + packagename);
	}

	async renamePackage(packagename, newName) {
		const change = this.newChange("RenamePackage");
		change.name = packagename;
		change.newName = newName;
		return await this.postChange(change, "rename package " + packagename);
	}

	async defineClass(classname, superclassname, packagename, definition) {
		const change = this.newChange("AddClass");
		change.className = classname;
		change.superclass = superclassname;
		change.package = packagename;
		change.definition = definition;
		return await this.postChange(change, "define class " + classname);
	}

	async commentClass(classname, comment) {
		const change = this.newChange("CommentClass");
		change.className = classname;
		change.comment = comment;
		return await this.postChange(change, "comment class " + classname);
	}

	async removeClass(classname) {
		const change = this.newChange("RemoveClass");
		change.className = classname;
		return await this.postChange(change, "remove class " + classname);
	}

	async renameClass(classname, newName, renameReferences = true) {
		const change = this.newChange("RenameClass");
		change.className = classname;
		change.newName = newName;
		change.renameReferences = renameReferences;
		return await this.postChange(change, "rename class " + classname);
	}

	async addInstanceVariable(classname, variable) {
		const change = this.newChange("AddInstanceVariable");
		change.className = classname;
		change.variable = variable;
		return await this.postChange(
			change,
			"add instance variable " + variable + " to " + classname
		);
	}

	async addClassVariable(classname, variable) {
		const change = this.newChange("AddClassVariable");
		change.className = classname;
		change.variable = variable;
		return await this.postChange(
			change,
			"add class variable " + variable + " to " + classname
		);
	}

	async renameInstanceVariable(classname, variable, newName) {
		const change = this.newChange("RenameInstanceVariable");
		change.className = classname;
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
		const change = this.newChange("RenameClassVariable");
		change.className = classname;
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
		const change = this.newChange("RemoveInstanceVariable");
		change.className = classname;
		change.variable = variable;
		return await this.postChange(
			change,
			"remove instance variable " + variable + " from class " + classname
		);
	}

	async removeClassVariable(classname, variable) {
		const change = this.newChange("RemoveClassVariable");
		change.className = classname;
		change.variable = variable;
		return await this.postChange(
			change,
			"remove class variable " + variable + " from class " + classname
		);
	}

	async moveInstanceVariableUp(classname, variable) {
		const change = this.newChange("MoveUpInstanceVariable");
		change.className = classname;
		change.variable = variable;
		return await this.postChange(
			change,
			"move up variable " + variable + " from class " + classname
		);
	}

	async moveInstanceVariableDown(classname, variable, target) {
		const change = this.newChange("MoveDownInstanceVariable");
		change.className = classname;
		change.variable = variable;
		change.target = target;
		return await this.postChange(
			change,
			"move down variable " + variable + " from class " + classname
		);
	}

	async renameCategory(classname, category, newName) {
		const change = this.newChange("RenameCategory");
		change.className = classname;
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
		const change = this.newChange("RemoveCategory");
		change.className = classname;
		change.category = category;
		return await this.postChange(
			change,
			"remove category " + category + " from class " + classname
		);
	}

	async compileMethod(classname, packagename, category, source) {
		const change = this.newChange("AddMethod");
		change.className = classname;
		change.package = packagename;
		change.category = category;
		change.sourceCode = source;
		return await this.postChange(
			change,
			"compile " + source + " in " + classname
		);
	}

	async removeMethod(classname, selector) {
		const change = this.newChange("RemoveMethod");
		change.className = classname;
		change.selector = selector;
		return await this.postChange(
			change,
			"remove methodd " + classname + ">>#" + selector
		);
	}

	async classifyMethod(classname, selector, category) {
		const change = this.newChange("ClassifyMethod");
		change.className = classname;
		change.selector = selector;
		change.category = category;
		return await this.postChange(
			change,
			"classify methodd " +
				classname +
				">>#" +
				selector +
				" under " +
				category
		);
	}

	async renameSelector(classname, selector, newSelector) {
		const change = this.newChange("RenameMethod");
		change.className = classname;
		change.selector = selector;
		change.newSelector = newSelector;
		return await this.postChange(
			change,
			"rename selector " + selector + " to " + newSelector
		);
	}

	// Evaluations...
	async evaluateExpression(
		expression,
		sync = false,
		pin = false,
		context,
		assignee
	) {
		const evaluation = {
			expression: expression,
			context: context,
			sync: sync,
			pin: pin,
			assignee: assignee,
		};
		return await this.issueEvaluation(evaluation);
	}

	async issueEvaluation(evaluation) {
		return await this.post(
			"/evaluations",
			evaluation,
			"evaluate " + evaluation.expression
		);
	}

	async cancelEvaluation(id) {
		return await this.delete(
			"/evaluations/" + id,
			"cancel evaluation with id " + id
		);
	}

	async evaluations() {
		return await this.get("/evaluations", "retrieve evaluations");
	}

	async debugExpression(expression, context) {
		const evaluation = {
			expression: expression,
			context: context,
			debug: true,
			pin: false,
		};
		return await this.post(
			"/evaluations",
			evaluation,
			"debug " + expression
		);
	}

	async profileExpression(expression, context) {
		const evaluation = {
			expression: expression,
			context: context,
			profile: true,
			pin: false,
		};
		return await this.post(
			"/evaluations",
			evaluation,
			"profile " + expression
		);
	}

	// Objects...
	async objects() {
		return await this.get("/objects", "objects");
	}

	async objectWithId(id) {
		return await this.get("/objects/" + id, "object with id " + id);
	}

	async unpinObject(id) {
		return await this.delete(
			"/objects/" + id,
			"unpin object with id " + id
		);
	}

	async unpinAllObjects() {
		return await this.delete("/objects", "unpin all objects");
	}

	async objectNamedSlots(id, path) {
		const response = await this.objectSlot(id, path + "/named-slots");
		return response;
	}

	async objectIndexedSlots(id, path) {
		const response = await this.objectSlot(id, path + "/indexed-slots");
		return response;
	}

	async objectInstanceVariables(id, path) {
		const response = await this.objectSlot(
			id,
			path + "/instance-variables"
		);
		return response;
	}

	async objectSlot(id, path) {
		return await this.get(
			"/objects/" + id + path,
			path + " of object with id " + id
		);
	}

	async pinObjectSlot(id, path) {
		const uri = "/objects/" + id + path;
		const body = { uri: uri };
		return await this.post(
			"/objects",
			body,
			"pin slot at URI /objects" + uri
		);
	}

	// Tests...
	async testRuns() {
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

	async testRunStatus(id) {
		return await this.get(
			"/test-runs/" + id + "/status",
			"status of test run " + id
		);
	}

	async testRunResults(id) {
		return await this.get(
			"/test-runs/" + id + "/results",
			"results of test run " + id
		);
	}

	async runTestRun(id) {
		return await this.post(
			"/test-runs/" + id + "/run",
			null,
			"run test run " + id
		);
	}

	async stopTestRun(id) {
		return await this.post(
			"/test-runs/" + id + "/stop",
			null,
			"stop test run " + id
		);
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
	async profilerTreeResults(id) {
		return await this.get(
			"/profilers/" + id + "/tree",
			"tree results of profiler " + id
		);
	}

	async profilerRankingResults(id) {
		return await this.get(
			"/profilers/" + id + "/ranking",
			"ranking results of profiler " + id
		);
	}

	//Native debugging...
	async nativeDebugger(id) {
		return await this.get(
			"/native-debuggers/" + id,
			"native debugger " + id
		);
	}

	async nativeDebuggerFrames(id) {
		return await this.get(
			"/native-debuggers/" + id + "/frames",
			"frames of native debugger " + id
		);
	}

	async nativeDebuggerRegisters(id) {
		return await this.get(
			"/native-debuggers/" + id + "/registers",
			"registers of native debugger " + id
		);
	}

	async nativeDebuggerSpaces(id) {
		return await this.get(
			"/native-debuggers/" + id + "/spaces",
			"spaces of native debugger " + id
		);
	}

	async nativeDebuggerFrame(id, index) {
		return await this.get(
			"/native-debuggers/" + id + "/frames/" + index,
			"frame " + index + " in native debugger " + id
		);
	}

	async resumeNativeDebugger(id) {
		return await this.post(
			"/native-debuggers/" + id + "/resume",
			null,
			"resume native debugger " + id
		);
	}

	async suspendNativeDebugger(id) {
		return await this.post(
			"/native-debuggers/" + id + "/suspend",
			null,
			"suspend native debugger " + id
		);
	}

	async pinNativeDebuggerRegister(id, register) {
		return await this.post(
			"/native-debuggers/" + id + "/registers/" + register + "/pin",
			null,
			"pin object pointed by register " +
				register +
				" of native debugger " +
				id
		);
	}

	//Memory stats...
	async memoryStats(last) {
		const query = last ? "?last=" + last : "";
		return await this.get("/memory-stats" + query, "memory stats");
	}
}

export default API;
