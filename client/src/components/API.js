import axios from "axios";

class APIError extends Error {
	constructor(message, request, status, reason, data) {
		super(message);
		this.name = "APIError";
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

	handleError(message, error) {
		var request, status, reason, data;
		request = error.request;
		if (error.response) {
			status = error.response.status;
			reason = error.response.statusText;
			data = error.response.data;
		} else if (error.request) {
			reason = "Could not send request due to " + error.message;
		}
		const exception = new APIError(
			'"' + message + '"',
			request,
			status,
			reason,
			data
		);
		throw exception;
	}

	async getDialect() {
		try {
			const response = await axios.get(this.baseUri + "/dialect");
			return response.data;
		} catch (error) {
			this.handleError("Cannot get dialect ", error);
		}
	}

	// Code...
	async getProjectTree(root) {
		try {
			const response = await axios.get(
				this.baseUri + "/projects?root=" + root + "&tree=true"
			);
			return response.data;
		} catch (error) {
			this.handleError("Cannot fetch project tree from " + root, error);
		}
	}

	async getProjectNames() {
		try {
			const response = await axios.get(this.baseUri + "/projects?names=true");
			return response.data;
		} catch (error) {
			this.handleError("Cannot fetch project names", error);
		}
	}

	async getProjectClasses(projectname) {
		try {
			const response = await axios.get(
				this.baseUri + "/projects/" + projectname + "/classes?tree=true"
			);
			return response.data;
		} catch (error) {
			this.handleError(
				"Cannot fetch classes from project " + projectname,
				error
			);
		}
	}

	async getClassTree(root, depth) {
		try {
			const response = await axios.get(
				this.baseUri +
					"/classes?names=true&root=" +
					root +
					"&tree=true&depth=" +
					depth
			);
			return response.data;
		} catch (error) {
			this.handleError("Cannot fetch class tree from " + root, error);
		}
	}

	async getClassNames() {
		try {
			const response = await axios.get(this.baseUri + "/classes?names=true");
			return response.data;
		} catch (error) {
			this.handleError("Cannot fetch class names", error);
		}
	}

	async getClass(classname) {
		try {
			const response = await axios.get(
				this.baseUri + "/classes/" + classname + "/"
			);
			return response.data;
		} catch (error) {
			this.handleError("Cannot fetch class " + classname, error);
		}
	}

	async getSubclasses(classname) {
		try {
			const response = await axios.get(
				this.baseUri + "/classes/" + classname + "/subclasses"
			);
			return response.data;
		} catch (error) {
			this.handleError("Cannot fecth subclasses of class " + classname, error);
		}
	}

	async getInstanceVariables(classname) {
		try {
			const response = await axios.get(
				this.baseUri + "/classes/" + classname + "/instance-variables"
			);
			return response.data;
		} catch (error) {
			this.handleError(
				"Cannot fecth instance variables of class " + classname,
				error
			);
		}
	}

	async getVariables(classname) {
		try {
			const response = await axios.get(
				this.baseUri + "/classes/" + classname + "/variables"
			);
			return response.data;
		} catch (error) {
			this.handleError("Cannot fecth variables of class " + classname, error);
		}
	}

	async getCategories(classname) {
		try {
			const response = await axios.get(
				this.baseUri + "/classes/" + classname + "/categories"
			);
			return response.data;
		} catch (error) {
			this.handleError("Cannot fecth categories of class " + classname, error);
		}
	}

	async getMethods(classname, sorted = false) {
		try {
			var url = this.baseUri + "/classes/" + classname + "/methods?marks=true";
			const response = await axios.get(url);
			const methods = response.data;
			if (sorted) {
				methods.sort((a, b) => (a.selector <= b.selector ? -1 : 1));
			}
			return methods;
		} catch (error) {
			this.handleError("Cannot fecth methods of class " + classname, error);
		}
	}

	async getMethodsAccessing(classname, variable, type, sorted = false) {
		try {
			var url =
				this.baseUri +
				"/classes/" +
				classname +
				"/methods?" +
				type +
				"=" +
				variable;
			const response = await axios.get(url);
			const methods = response.data;
			if (sorted) {
				methods.sort((a, b) => (a.selector <= b.selector ? -1 : 1));
			}
			return methods;
		} catch (error) {
			this.handleError(
				"Cannot fecth methods of class " + classname + " using " + variable,
				error
			);
		}
	}

	async getMethod(classname, selector) {
		try {
			const encoded = encodeURIComponent(selector);
			const response = await axios.get(
				this.baseUri +
					"/classes/" +
					classname +
					"/methods?selector=" +
					encoded +
					"&bytecodes=true&disassembly=true&ast=true"
			);
			return response.data.length === 0 ? null : response.data[0];
		} catch (error) {
			this.handleError(
				"Cannot fetch method " + classname + ">>#" + selector,
				error
			);
		}
	}

	async getSenders(selector) {
		try {
			const response = await axios.get(
				this.baseUri + "/methods?sending=" + selector
			);
			return response.data;
		} catch (error) {
			this.handleError("Cannot fetch senders of " + selector, error);
		}
	}

	async getLocalSenders(selector, classname) {
		try {
			const response = await axios.get(
				this.baseUri + "/methods?sending=" + selector + "&scope=" + classname
			);
			return response.data;
		} catch (error) {
			this.handleError(
				"Cannot fetch senders of " + selector + " in class " + classname,
				error
			);
		}
	}

	async getClassReferences(classname) {
		try {
			const response = await axios.get(
				this.baseUri + "/methods?referencingClass=" + classname
			);
			return response.data;
		} catch (error) {
			this.handleError("Cannot fetch references to class " + classname, error);
		}
	}

	async getStringReferences(string) {
		try {
			const response = await axios.get(
				this.baseUri + "/methods?referencingString=" + string
			);
			return response.data;
		} catch (error) {
			this.handleError("Cannot fetch references to string " + string, error);
		}
	}

	async getImplementors(selector) {
		try {
			const response = await axios.get(
				this.baseUri + "/methods?selector=" + selector
			);
			return response.data;
		} catch (error) {
			this.handleError("Cannot fetch implementors of " + selector, error);
		}
	}

	async getLocalImplementors(selector, classname) {
		try {
			const response = await axios.get(
				this.baseUri + "/methods?selector=" + selector + "&scope=" + classname
			);
			return response.data;
		} catch (error) {
			this.handleError(
				"Cannot fetch local implementors of " +
					selector +
					" in class " +
					classname,
				error
			);
		}
	}

	// Debugging...
	async getDebuggers() {
		try {
			const response = await axios.get(this.baseUri + "/debuggers");
			return response.data;
		} catch (error) {
			this.handleError("Cannot retrieve debuggers", error);
		}
	}

	async createDebugger(id) {
		try {
			const evaluation = { evaluation: id };
			const response = await axios.post(
				this.baseUri + "/debuggers",
				evaluation
			);
			return response.data;
		} catch (error) {
			this.handleError("Cannot create debugger on evaluation " + id, error);
		}
	}

	async getDebuggerFrames(id) {
		try {
			const response = await axios.get(
				this.baseUri + "/debuggers/" + id + "/frames"
			);
			return response.data;
		} catch (error) {
			this.handleError("Cannot fetch frames of debugger " + id, error);
		}
	}

	async getDebuggerFrame(id, index) {
		try {
			const response = await axios.get(
				this.baseUri + "/debuggers/" + id + "/frames/" + index
			);
			return response.data;
		} catch (error) {
			this.handleError(
				"Cannot fetch frame " + index + " in debugger " + id,
				error
			);
		}
	}

	async getFrameBindings(id, index) {
		try {
			const response = await axios.get(
				this.baseUri + "/debuggers/" + id + "/frames/" + index + "/bindings"
			);
			return response.data;
		} catch (error) {
			this.handleError(
				"Cannot fetch bindings of frame " + index + " in debugger " + id,
				error
			);
		}
	}

	async stepIntoDebugger(id, index) {
		try {
			const response = await axios.post(
				this.baseUri + "/debuggers/" + id + "/frames/" + index + "/stepinto"
			);
			return response.data;
		} catch (error) {
			this.handleError(
				"Cannot step into on frame " + index + " of debugger " + id,
				error
			);
		}
	}

	async stepOverDebugger(id, index) {
		try {
			const response = await axios.post(
				this.baseUri + "/debuggers/" + id + "/frames/" + index + "/stepover"
			);
			return response.data;
		} catch (error) {
			this.handleError(
				"Cannot step over on frame " + index + " of debugger " + id,
				error
			);
		}
	}

	async restartDebugger(id, index, update = false) {
		try {
			const response = await axios.post(
				this.baseUri +
					"/debuggers/" +
					id +
					"/frames/" +
					index +
					"/restart?update=" +
					update
			);
			return response.data;
		} catch (error) {
			this.handleError(
				"Cannot restart on frame " + index + " of debugger " + id,
				error
			);
		}
	}

	async resumeDebugger(id) {
		try {
			const response = await axios.post(
				this.baseUri + "/debuggers/" + id + "/resume"
			);
			return response.data;
		} catch (error) {
			this.handleError("Cannot resume debugger " + id, error);
		}
	}

	async terminateDebugger(id) {
		try {
			const response = await axios.post(
				this.baseUri + "/debuggers/" + id + "/terminate"
			);
			return response.data;
		} catch (error) {
			this.handleError("Cannot terminate debugger " + id, error);
		}
	}

	async deleteDebugger(id) {
		try {
			const response = await axios.delete(this.baseUri + "/debuggers/" + id);
			return response.data;
		} catch (error) {
			this.handleError("Cannot delete debugger " + id, error);
		}
	}

	// Workspaces...
	async getWorkspaces() {
		try {
			const response = await axios.get(this.baseUri + "/workspaces");
			return response.data;
		} catch (error) {
			this.handleError("Cannot retrieve workspaces", error);
		}
	}

	async createWorkspace() {
		try {
			const response = await axios.post(this.baseUri + "/workspaces");
			return response.data;
		} catch (error) {
			this.handleError("Cannot create workspace", error);
		}
	}

	async deleteWorkspace(id) {
		try {
			const response = await axios.delete(this.baseUri + "/workspaces/" + id);
			return response.data;
		} catch (error) {
			this.handleError("Cannot delete workspace ", error);
		}
	}

	// Changes...
	async getChanges() {
		try {
			const response = await axios.get(this.baseUri + "/changes");
			return response.data;
		} catch (error) {
			this.handleError("Cannot fetch changes ", error);
		}
	}

	newChange(type) {
		return {
			type: type,
			author: this.author,
		};
	}

	async postChange(change, description) {
		try {
			const response = await axios.post(this.baseUri + "/changes", change);
			this.reportChange(change);
			return response.data;
		} catch (error) {
			this.handleError("Cannot " + description, error);
		}
	}

	// Change helpers...
	async defineClass(classname, project, definition) {
		const change = this.newChange("ClassDefinition");
		change.class = classname;
		change.project = project;
		change.definition = definition;
		return await this.postChange(change, "define class " + classname);
	}

	async commentClass(classname, comment) {
		const change = this.newChange("ClassCommentDefinition");
		change.class = classname;
		change.comment = comment;
		return await this.postChange(change, "comment class " + classname);
	}

	async deleteClass(classname) {
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

	async deleteInstanceVariable(classname, variable) {
		const change = this.newChange("InstanceVariableRemove");
		change.class = classname;
		change.variable = variable;
		return await this.postChange(
			change,
			"remove instance variable " + variable + " from class " + classname
		);
	}

	async deleteClassVariable(classname, variable) {
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

	async deleteCategory(classname, category) {
		const change = this.newChange("CategoryRemove");
		change.class = classname;
		change.category = category;
		return await this.postChange(
			change,
			"remove category " + category + " from class " + classname
		);
	}

	async compileMethod(classname, project, category, source) {
		const change = this.newChange("MethodDefinition");
		change.class = classname;
		change.project = project;
		change.category = category;
		change.sourceCode = source;
		return await this.postChange(
			change,
			"compile " + source + " in " + classname
		);
	}

	async deleteMethod(classname, selector) {
		const change = this.newChange("MethodRemove");
		change.class = classname;
		change.selector = selector;
		return await this.postChange(
			change,
			"remove methodd " + classname + ">>#" + selector
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
		try {
			const evaluation = {
				expression: expression,
				context: context,
				sync: sync,
				pin: pin,
			};
			const response = await axios.post(
				this.baseUri + "/evaluations",
				evaluation
			);
			return response.data;
		} catch (error) {
			this.handleError("Cannot evaluate " + expression, error);
		}
	}

	async debugExpression(expression, context) {
		try {
			const evaluation = {
				expression: expression,
				context: context,
				debug: true,
				pin: false,
			};
			const response = await axios.post(
				this.baseUri + "/evaluations",
				evaluation
			);
			return response.data;
		} catch (error) {
			this.handleError("Cannot debug " + expression, error);
		}
	}

	async profileExpression(expression, context) {
		try {
			const evaluation = {
				expression: expression,
				context: context,
				profile: true,
				pin: false,
			};
			const response = await axios.post(
				this.baseUri + "/evaluations",
				evaluation
			);
			return response.data;
		} catch (error) {
			this.handleError("Cannot profile " + expression, error);
		}
	}

	// Objects...
	async getObjects() {
		try {
			const response = await axios.get(this.baseUri + "/objects");
			return response.data;
		} catch (error) {
			this.handleError("Cannot fetch objects", error);
		}
	}

	async getObject(id) {
		try {
			const response = await axios.get(this.baseUri + "/objects/" + id);
			return response.data;
		} catch (error) {
			this.handleError("Cannot fetch object with id " + id, error);
		}
	}

	async unpinObject(id) {
		try {
			const response = await axios.delete(this.baseUri + "/objects/" + id);
			return response.data;
		} catch (error) {
			this.handleError("Cannot unpin object with id " + id, error);
		}
	}

	async getObjectInstanceVariables(id, path) {
		try {
			const response = await axios.get(
				this.baseUri + "/objects/" + id + path + "/instance-variables"
			);
			return response.data;
		} catch (error) {
			this.handleError(
				"Cannot fecth instance variables of object with id " + id,
				error
			);
		}
	}

	async getSlot(id, path) {
		try {
			const response = await axios.get(this.baseUri + "/objects/" + id + path);
			return response.data;
		} catch (error) {
			this.handleError(
				"Cannot fecth slot " + path + " of object with id " + id,
				error
			);
		}
	}

	// Tests...
	async getTestRuns() {
		try {
			const response = await axios.get(this.baseUri + "/test-runs");
			return response.data;
		} catch (error) {
			this.handleError("Cannot retrieve test runs", error);
		}
	}

	async runTest(classname, selector) {
		try {
			const run = {
				methods: [{ class: classname, selector: selector }],
			};
			const response = await axios.post(this.baseUri + "/test-runs", run);
			return response.data;
		} catch (error) {
			this.handleError(
				"Cannot run test " + selector + " in " + classname,
				error
			);
		}
	}

	async runTestClass(classname) {
		try {
			const run = {
				classes: [classname],
			};
			const response = await axios.post(this.baseUri + "/test-runs", run);
			return response.data;
		} catch (error) {
			this.handleError("Cannot run test class " + classname, error);
		}
	}

	async runTestProject(projectname) {
		try {
			const run = {
				project: projectname,
			};
			const response = await axios.post(this.baseUri + "/test-runs", run);
			return response.data;
		} catch (error) {
			this.handleError("Cannot run test project " + projectname, error);
		}
	}

	async getTestRunStatus(id) {
		try {
			const response = await axios.get(
				this.baseUri + "/test-runs/" + id + "/status"
			);
			return response.data;
		} catch (error) {
			this.handleError("Cannot get the status of test run " + id, error);
		}
	}

	async getTestRunResults(id) {
		try {
			const response = await axios.get(
				this.baseUri + "/test-runs/" + id + "/results"
			);
			return response.data;
		} catch (error) {
			this.handleError("Cannot get the results of test run " + id, error);
		}
	}

	async runTestRun(id) {
		try {
			const response = await axios.post(
				this.baseUri + "/test-runs/" + id + "/run"
			);
			return response.data;
		} catch (error) {
			this.handleError("Cannot run test run " + id, error);
		}
	}

	async stopTestRun(id) {
		try {
			const response = await axios.post(
				this.baseUri + "/test-runs/" + id + "/stop"
			);
			return response.data;
		} catch (error) {
			this.handleError("Cannot stop test run " + id, error);
		}
	}

	async deleteTestRun(id) {
		try {
			const response = await axios.delete(this.baseUri + "/test-runs/" + id);
			return response.data;
		} catch (error) {
			this.handleError("Cannot delete test run " + id, error);
		}
	}

	async debugTest(id, classname, selector) {
		try {
			const test = {
				class: classname,
				selector: selector,
			};
			const response = await axios.post(
				this.baseUri + "/test-runs/" + id + "/debug",
				test
			);
			return response.data;
		} catch (error) {
			this.handleError(
				"Cannot debug test " + selector + " in " + classname,
				error
			);
		}
	}

	// Profiling...
	async getProfilerTreeResults(id) {
		try {
			const response = await axios.get(
				this.baseUri + "/profilers/" + id + "/tree"
			);
			return response.data;
		} catch (error) {
			this.handleError("Cannot fetch tree results of profiler " + id, error);
		}
	}

	async getProfilerRankingResults(id) {
		try {
			const response = await axios.get(
				this.baseUri + "/profilers/" + id + "/ranking"
			);
			return response.data;
		} catch (error) {
			this.handleError("Cannot fetch ranking results of profiler " + id, error);
		}
	}

	//Native debugging...
	async getNativeDebugger(id) {
		try {
			const response = await axios.get(
				this.baseUri + "/native-debuggers/" + id
			);
			return response.data;
		} catch (error) {
			this.handleError("Cannot fetch native debugger " + id, error);
		}
	}

	async getNativeDebuggerFrames(id) {
		try {
			const response = await axios.get(
				this.baseUri + "/native-debuggers/" + id + "/frames"
			);
			return response.data;
		} catch (error) {
			this.handleError("Cannot fetch frames of native debugger " + id, error);
		}
	}

	async getNativeDebuggerRegisters(id) {
		try {
			const response = await axios.get(
				this.baseUri + "/native-debuggers/" + id + "/registers"
			);
			return response.data;
		} catch (error) {
			this.handleError(
				"Cannot fetch registers of native debugger " + id,
				error
			);
		}
	}

	async getNativeDebuggerSpaces(id) {
		try {
			const response = await axios.get(
				this.baseUri + "/native-debuggers/" + id + "/spaces"
			);
			return response.data;
		} catch (error) {
			this.handleError("Cannot fetch spaces of native debugger " + id, error);
		}
	}

	async getNativeDebuggerFrame(id, index) {
		try {
			const response = await axios.get(
				this.baseUri + "/native-debuggers/" + id + "/frames/" + index
			);
			return response.data;
		} catch (error) {
			this.handleError(
				"Cannot fetch frame " + index + " in native debugger " + id,
				error
			);
		}
	}

	async resumeNativeDebugger(id) {
		try {
			const response = await axios.post(
				this.baseUri + "/native-debuggers/" + id + "/resume"
			);
			return response.data;
		} catch (error) {
			this.handleError("Cannot resume native debugger " + id, error);
		}
	}

	async suspendNativeDebugger(id) {
		try {
			const response = await axios.post(
				this.baseUri + "/native-debuggers/" + id + "/suspend"
			);
			return response.data;
		} catch (error) {
			this.handleError("Cannot suspend native debugger " + id, error);
		}
	}

	async pinNativeDebuggerRegister(id, register) {
		try {
			const response = await axios.post(
				this.baseUri +
					"/native-debuggers/" +
					id +
					"/registers/" +
					register +
					"/pin"
			);
			return response.data;
		} catch (error) {
			this.handleError(
				"Cannot pin object pointed by register " +
					register +
					" of native debugger " +
					id,
				error
			);
		}
	}

	//Memory stats...
	async getMemoryStats(last) {
		const query = last ? "?last=" + last : "";
		try {
			const response = await axios.get(this.baseUri + "/memory-stats" + query);
			return response.data;
		} catch (error) {
			this.handleError("Cannot fetch memory stats", error);
		}
	}
}

export default API;
