import axios from "axios";

class BackendTestFailure extends Error {
	constructor(description) {
		super(description);
		this.description = description;
	}
}

class BackendTestError extends BackendTestFailure {
	constructor(description, error) {
		super(description);
		this.error = error;
	}
}

class BackendTestResult {
	constructor(state = "Not run", description = "") {
		this.state = state;
		this.description = description;
	}

	failure(description) {
		this.state = "Failed";
		this.description = description;
	}

	error(description) {
		this.state = "Error";
		this.description = description;
	}

	passed() {
		this.state = "Passed";
		this.description = "";
	}
}

class BackendTestSuite {
	constructor(url) {
		this.url = url;
		this.state = "stopped";
		this.initializeTests();
	}

	initializeTests() {
		this.tests = Object.getOwnPropertyNames(BackendTest.prototype)
			.filter((n) => n.startsWith("test"))
			.map((n) => new BackendTest(n, this.url));
	}

	async runTest(test) {
		try {
			await test[test.name]();
		} catch (error) {
			if (error instanceof BackendTestFailure) {
				test.result.failure(error.description);
			} else {
				test.result.error(error.description || error.message);
			}
			return;
		}
		test.result.passed();
	}

	async run() {
		this.tests.forEach((r) => r.reset());
		this.state = "running";
		this.ran = 0;
		this.count = this.tests.length;
		await Promise.all(
			this.tests.map(async (test) => {
				if (this.state !== "stopped") {
					await this.runTest(test);
					this.ran++;
				}
			})
		);
		this.state = "stopped";
	}

	stop() {
		this.state = "stopped";
	}
}

class BackendTest {
	constructor(name, url) {
		this.name = name;
		this.url = url;
		this.log = [];
		this.result = new BackendTestResult();
	}

	async get(uri) {
		let data;
		try {
			let request = { method: "GET", uri: uri };
			this.log.push(request);
			const response = await axios.get(this.url + uri);
			data = response.data;
			request.response = {
				status: response.status,
				data: data,
			};
		} catch (error) {
			throw new BackendTestError(error.message, error);
		}
		return data;
	}

	async post(uri, payload) {
		let data;
		try {
			let request = { method: "POST", uri: uri, payload: payload };
			this.log.push(request);
			const response = await axios.post(this.url + uri, payload);
			data = response.data;
			request.response = {
				status: response.status,
				data: data,
			};
		} catch (error) {
			throw new BackendTestError(error.message, error);
		}
		return data;
	}

	async delete(uri) {
		let data;
		try {
			let request = { method: "DELETE", uri: uri };
			this.log.push(request);
			const response = await axios.delete(this.url + uri);
			data = response.data;
			request.response = {
				status: response.status,
				data: data,
			};
		} catch (error) {
			throw new BackendTestError(error.message, error);
		}
		return data;
	}

	section() {
		const prefix = this.name.split("_")[0];
		return prefix.substring(4, prefix.length);
	}

	description() {
		const suffix = this.name.split("_")[1];
		let description = "Test";
		suffix.match(/[A-Z][a-z]+/g).forEach((w) => {
			description += " " + w;
		});
		return description;
	}

	state() {
		return this.result.state;
	}

	resultDescription() {
		return this.result.description;
	}

	reset() {
		this.result = new BackendTestResult();
		this.log = [];
	}

	// Services...

	async wait(milliseconds) {
		await new Promise((resolve) => setTimeout(resolve, milliseconds));
	}

	assert(condition, description = "failure") {
		if (!condition) {
			throw new BackendTestFailure(description);
		}
	}

	deny(condition, description) {
		this.assert(!condition, description);
	}

	assertNotNull(data, name = "response data") {
		this.assert(data, name + " should be defined");
	}

	assertEquals(value, expected, name = "response data") {
		this.assert(
			value === expected,
			name +
				" should be " +
				expected.toString() +
				" and it is " +
				value.toString()
		);
	}

	assertStartsWith(value, prefix, name = "response data") {
		this.assert(
			value.startsWith(prefix),
			name +
				"(" +
				value.toString() +
				") should should start with " +
				prefix.toString()
		);
	}

	assertIsString(data, name = "response data") {
		this.assert(typeof data === "string", name + " should be a string");
	}

	assertIncludes(collection, element, name = "response data") {
		this.assert(
			collection.includes(element),
			name + " must contain " + element.toString()
		);
	}

	assertNotEmpty(collection, name = "response data") {
		this.assert(collection.length > 0, name + " should not be empty");
	}

	assertIsEmpty(collection, name = "response data") {
		this.assert(collection.length === 0, name + " should not be empty");
	}

	assertIsArray(data, name = "response data") {
		this.assert(Array.isArray(data), name + " should be an array");
	}

	assertAllSatisfy(data, condition, description) {
		data.forEach((element) =>
			this.assert(
				condition(element),
				"all elements should satisfy that " + description
			)
		);
	}

	assertAnySatisfy(data, condition, description) {
		this.assert(
			data.find((element) => condition(element)),
			"at least one element should satisfy that " + description
		);
	}

	assertNoneSatisfy(data, condition, description) {
		data.forEach((element) =>
			this.assert(
				!condition(element),
				"no element should satisfy that " + description
			)
		);
	}

	// General tests...

	async testGeneral_Dialect() {
		const dialect = await this.get("/dialect");
		this.assertIsString(dialect);
	}

	async testGeneral_Version() {
		const version = await this.get("/version");
		this.assertIsString(version);
	}

	// Code tests...

	async testCode_Accessors() {
		let change = {
			type: "AddClass",
			className: "TestAccessors",
			instanceVariables: ["xxx"],
			superclass: "Object",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		change = {
			type: "AddMethod",
			className: "TestAccessors",
			category: "WebsideTest",
			sourceCode: "a xxx + 1",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		change = {
			type: "AddMethod",
			className: "TestAccessors",
			category: "WebsideTest",
			sourceCode: "b xxx := 2",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		try {
			const readers = await this.get(
				"/methods?class=TestAccessors&accessing=xxx"
			);
			this.assertIsArray(readers);
			this.assertNotEmpty(readers);
			this.assertAllSatisfy(
				readers,
				(m) => m.source.includes("xxx"),
				"method.source includes 'xxx'"
			);
			const writers = await this.get(
				"/methods?class=TestAccessors&assigning=xxx"
			);
			this.assertIsArray(writers);
			this.assertNotEmpty(writers);
			this.assertAllSatisfy(
				writers,
				(m) => m.source.includes("xxx :="),
				"method.source includes 'xxx :='"
			);
		} finally {
			change = {
				type: "RemoveMethod",
				className: "TestAccessors",
				selector: "a",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
			change = {
				type: "RemoveMethod",
				className: "TestAccessors",
				selector: "b",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
			change = {
				type: "RemoveClass",
				className: "TestAccessors",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
		}
	}

	async testCode_Ast() {
		let change = {
			type: "AddClass",
			className: "TestAst",
			instanceVariables: ["a"],
			superclass: "Object",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		change = {
			type: "AddMethod",
			className: "TestAst",
			category: "WebsideTest",
			sourceCode: "a ^a",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		try {
			const method = await this.get(
				"/classes/TestAst/methods/a?ast=true"
			);
			const ast = method.ast;
			this.assertNotNull(ast, "method.ast");
			const traverse = (node, block) => {
				block(node);
				if (node.children) {
					node.children.forEach((child) => traverse(child, block));
				}
			};
			let ret;
			traverse(ast, (n) => {
				if (n.type === "Return") ret = n;
			});
			this.assert(ret, "method.ast should contain a return node");
			let x;
			traverse(ret, (n) => {
				if (n.value === "a") x = n;
			});
			this.assert(x, "return node should include variable node for 'a'");
		} finally {
			change = {
				type: "RemoveMethod",
				className: "TestAst",
				selector: "a",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
			change = {
				type: "RemoveClass",
				className: "TestAst",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
		}
	}

	async testCode_Categories() {
		let change = {
			type: "AddClass",
			className: "TestCategories",
			superclass: "Object",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		change = {
			type: "AddMethod",
			className: "TestCategories",
			category: "testCategories",
			sourceCode: "a",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		try {
			const categories = await this.get(
				"/classes/TestCategories/categories"
			);
			this.assertIsArray(categories);
			this.assertNotEmpty(categories);
			categories.forEach((c) => this.assertIsString(c, "category"));
			this.assertIncludes(categories, "testCategories");
		} finally {
			change = {
				type: "RemoveMethod",
				className: "TestCategories",
				selector: "a",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
			change = {
				type: "RemoveClass",
				className: "TestCategories",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
		}
	}

	async testCode_Class() {
		let change = {
			type: "AddClass",
			className: "TestClass",
			instanceVariables: ["xxx"],
			classVariables: ["AAA"],
			superclass: "Object",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		try {
			let definition = await this.get("/classes/TestClass");
			this.assertEquals(definition.name, "TestClass", "class.name");
			this.assertIncludes(
				definition.definition,
				"xxx",
				"class.definition"
			);
			this.assertIncludes(
				definition.definition,
				"AAA",
				"class.definition"
			);
		} finally {
			change = {
				type: "RemoveClass",
				className: "TestClass",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
		}
	}

	async testCode_Classes() {
		let change = {
			type: "AddClass",
			className: "TestClasses",
			superclass: "Object",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		change = {
			type: "AddClass",
			className: "TestClasses2",
			superclass: "TestClasses",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		change = {
			type: "AddClass",
			className: "TestClasses3",
			superclass: "TestClasses2",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		try {
			const classes = await this.get("/classes?root=TestClasses");
			this.assertIsArray(classes);
			this.assert(classes.find((c) => c.name === "TestClasses2"));
			const names = await this.get(
				"/classes?root=TestClasses&names=true"
			);
			this.assertNotEmpty(names);
			names.forEach((n) => this.assertIsString(n));
			const tree = await this.get("/classes?root=TestClasses&tree=true");
			this.assertIsArray(tree);
			this.assertEquals(tree.length, 1, "tree length");
			const root = tree[0];
			this.assertEquals(root.name, "TestClasses", "root.name");
			this.assert(root.subclasses.find((c) => c.name === "TestClasses2"));
			const subclass = root.subclasses.find(
				(c) => c.name === "TestClasses2"
			);
			this.assert(
				subclass.subclasses.find((c) => c.name === "TestClasses3")
			);
		} finally {
			change = {
				type: "RemoveClass",
				className: "TestClasses3",
			};
			await this.post("/changes", change);
			change = {
				type: "RemoveClass",
				className: "TestClasses2",
			};
			await this.post("/changes", change);
			change = {
				type: "RemoveClass",
				className: "TestClasses",
			};
			await this.post("/changes", change);
		}
	}

	async testCode_ClassVariables() {
		let change = {
			type: "AddClass",
			className: "TestClassVariables",
			classVariables: ["Aaa"],
			superclass: "Object",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		try {
			const variables = await this.get(
				"/classes/TestClassVariables/class-variables"
			);
			this.assert(
				variables.find((v) => v.name === "Aaa" && v.type === "class")
			);
		} finally {
			change = {
				type: "RemoveClass",
				className: "TestClassVariables",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
		}
	}

	async testCode_Implementors() {
		let change = {
			type: "AddClass",
			className: "TestImplementors",
			superclass: "Object",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		change = {
			type: "AddMethod",
			className: "TestImplementors",
			sourceCode: "testImplementors",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		change = {
			type: "AddClass",
			className: "TestImplementors2",
			superclass: "Object",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		change = {
			type: "AddMethod",
			className: "TestImplementors2",
			sourceCode: "testImplementors",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		try {
			const implementors = await this.get(
				"/methods?selector=testImplementors"
			);
			this.assertIsArray(implementors);
			this.assert(
				implementors.length >= 2,
				"data.length should be equal or greater than 2"
			);
			this.assertAllSatisfy(
				implementors,
				(m) => m.selector === "testImplementors",
				"method.selector equals 'testImplementors'"
			);
			this.assertAnySatisfy(
				implementors,
				(m) => m.methodClass === "TestImplementors",
				"method.methodClass equals 'TestImplementors'"
			);
			const local = await this.get(
				"/methods?selector=testImplementors&class=TestImplementors"
			);
			this.assertIsArray(local);
			this.assertEquals(local.length, 1, "response.length");
			this.assertEquals(
				local[0].selector,
				"testImplementors",
				"method.selector"
			);
			this.assertEquals(
				local[0].methodClass,
				"TestImplementors",
				"method.methodClass"
			);
		} finally {
			change = {
				type: "RemoveMethod",
				className: "TestImplementors",
				selector: "testImplementors",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
			change = {
				type: "RemoveMethod",
				className: "TestImplementors2",
				selector: "testImplementors",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
			change = {
				type: "RemoveClass",
				className: "TestImplementors",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
			change = {
				type: "RemoveClass",
				className: "TestImplementors2",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
		}
	}

	async testCode_InstanceVariables() {
		let change = {
			type: "AddClass",
			className: "TestInstanceVariables",
			instanceVariables: ["x"],
			superclass: "Object",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		try {
			const variables = await this.get(
				"/classes/TestInstanceVariables/instance-variables"
			);
			this.assertAnySatisfy(
				variables,
				(v) => v.name === "x" && v.type === "instance",
				"variable.name equals 'x' and variable.type equals 'instance'"
			);
		} finally {
			change = {
				type: "RemoveClass",
				className: "TestInstanceVariables",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
		}
	}

	async testCode_MatchingSelectors() {
		let change = {
			type: "AddClass",
			className: "TestMatchingSelectors",
			superclass: "Object",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		change = {
			type: "AddMethod",
			className: "TestMatchingSelectors",
			sourceCode: "testMatchingSelectors",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		try {
			const matching = await this.get(
				"/methods?selectorMatching=testMatchi"
			);
			this.assertIsArray(matching);
			this.assertNotEmpty(matching);
			this.assertAnySatisfy(
				matching,
				(m) => m.selector === "testMatchingSelectors",
				"method.selector equals 'testMatchingSelectors'"
			);
		} finally {
			change = {
				type: "RemoveMethod",
				className: "TestMatchingSelectors",
				selector: "testMatchingSelectors",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
			change = {
				type: "RemoveClass",
				className: "TestMatchingSelectors",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
		}
	}

	async testCode_Method() {
		let change = {
			type: "AddClass",
			className: "TestMethod",
			superclass: "Object",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		change = {
			type: "AddMethod",
			className: "TestMethod",
			sourceCode: "testMethod ^'this is testMethod'",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		try {
			const method = await this.get(
				"/classes/TestMethod/methods/testMethod"
			);
			this.assertEquals(method.selector, "testMethod");
			this.assertIncludes(
				method.source,
				"this is testMethod",
				"method.source"
			);
		} finally {
			change = {
				type: "RemoveMethod",
				className: "TestMethod",
				selector: "testMethod",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
			change = {
				type: "RemoveClass",
				className: "TestMethod",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
		}
	}

	async testCode_MethodCount() {
		let change = {
			type: "AddClass",
			className: "TestMethodCount",
			superclass: "Object",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		for (let i = 1; i <= 3; i++) {
			change = {
				type: "AddMethod",
				className: "TestMethodCount",
				sourceCode: "testMethodCount" + i + " ^" + i,
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
		}
		try {
			const count = await this.get(
				"/classes/TestMethodCount/methods?count=true"
			);
			this.assertEquals(count, 3, "data.length");
		} finally {
			for (let i = 1; i <= 3; i++) {
				change = {
					type: "RemoveMethod",
					className: "TestMethodCount",
					selector: "testMethodCount" + i,
					author: "Webside_BackendTest",
					package: "Webside_BackendTest",
				};
				await this.post("/changes", change);
			}
			change = {
				type: "RemoveClass",
				className: "TestMethodCount",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
		}
	}

	async testCode_Methods() {
		let change = {
			type: "AddClass",
			className: "TestMethods",
			superclass: "Object",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		for (let i = 1; i <= 2; i++) {
			change = {
				type: "AddMethod",
				className: "TestMethods",
				sourceCode: "testMethods" + i + " ^" + i,
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
		}
		try {
			const methods = await this.get("/classes/TestMethods/methods");
			this.assertIsArray(methods);
			this.assertNotEmpty(methods);
			this.assertAllSatisfy(
				methods,
				(m) => m.methodClass === "TestMethods",
				"method.methodClass is 'TestMethods'"
			);
		} finally {
			for (let i = 1; i <= 2; i++) {
				change = {
					type: "RemoveMethod",
					className: "TestMethods",
					selector: "testMethods" + i,
					author: "Webside_BackendTest",
					package: "Webside_BackendTest",
				};
				await this.post("/changes", change);
			}
			change = {
				type: "RemoveClass",
				className: "TestMethods",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
		}
	}

	async testCode_MethodTemplate() {
		const template = await this.get("/methodtemplate");
		this.assertNotNull(template, "template");
		this.assert(template.template, "template.template should be true");
		this.assertNotNull(template.selector, "template.selector");
		this.assertNotNull(template.source, "template.source");
	}

	async testCode_Package() {
		let change = {
			type: "AddPackage",
			name: "TestPackage",
			author: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		change = {
			type: "AddClass",
			className: "TestPackage",
			superclass: "Object",
			package: "TestPackage",
			author: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		try {
			const pack = await this.get("/packages/TestPackage");
			this.assertNotNull(pack);
			this.assertIsArray(pack.classes, "package.classes");
			this.assertAnySatisfy(
				pack.classes,
				(c) => c === "TestPackage",
				"class equals 'TestPackage'"
			);
			this.assertNotNull(pack.methods, "package.methods");
			if (pack.categories)
				this.assertIsArray(pack.categories, "package.categories");
		} finally {
			change = {
				type: "RemoveClass",
				className: "TestPackage",
				author: "Webside_BackendTest",
				package: "TestPackage",
			};
			await this.post("/changes", change);
			change = {
				type: "RemovePackage",
				name: "TestPackage",
				author: "Webside_BackendTest",
			};
			await this.post("/changes", change);
		}
	}

	async testCode_PackageNames() {
		let change = {
			type: "AddPackage",
			name: "TestPackageNames",
			author: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		try {
			const names = await this.get("/packages?names=true");
			this.assertIsArray(names);
			this.assertNotEmpty(names);
			this.assertAllSatisfy(
				names,
				(n) => typeof n === "string",
				"it is string"
			);
			this.assertAnySatisfy(
				names,
				(n) => n === "TestPackageNames",
				"it is 'TestPackageNames'"
			);
		} finally {
			change = {
				type: "RemovePackage",
				name: "TestPackageNames",
				author: "Webside_BackendTest",
			};
			await this.post("/changes", change);
		}
	}

	async testCode_Packages() {
		let change = {
			type: "AddPackage",
			name: "TestPackages",
			author: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		change = {
			type: "AddClass",
			className: "TestPackages",
			superclass: "Object",
			package: "TestPackages",
			author: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		try {
			const packages = await this.get("/packages");
			this.assertIsArray(packages);
			this.assertNotEmpty(packages);
			const pack = packages.find((p) => p.name === "TestPackages");
			this.assertNotNull(pack);
			this.assertIsArray(pack.classes, "package.classes");
			this.assertNotNull(pack.methods, "package.methods");
			if (pack.categories)
				this.assertIsArray(pack.categories, "package.categories");
		} finally {
			change = {
				type: "RemoveClass",
				className: "TestPackages",
				author: "Webside_BackendTest",
				package: "TestPackages",
			};
			await this.post("/changes", change);
			change = {
				type: "RemovePackage",
				name: "TestPackages",
				author: "Webside_BackendTest",
			};
			await this.post("/changes", change);
		}
	}

	async testCode_Selectors() {
		let change = {
			type: "AddClass",
			className: "TestSelectors",
			superclass: "Object",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		for (let i = 1; i <= 2; i++) {
			change = {
				type: "AddMethod",
				className: "TestSelectors",
				sourceCode: "testSelectors" + i + " ^" + i,
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
		}
		try {
			const selectors = await this.get(
				"/classes/TestSelectors/selectors"
			);
			this.assertIsArray(selectors);
			this.assertEquals(selectors.length, 2, "selectors.length");
			this.assertAllSatisfy(
				selectors,
				(s) => s.startsWith("testSelectors"),
				"selector statrs with 'testMethods'"
			);
		} finally {
			for (let i = 1; i <= 2; i++) {
				change = {
					type: "RemoveMethod",
					className: "TestSelectors",
					selector: "testSelectors" + i,
					author: "Webside_BackendTest",
					package: "Webside_BackendTest",
				};
				await this.post("/changes", change);
			}
			change = {
				type: "RemoveClass",
				className: "TestSelectors",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
		}
	}

	async testCode_Senders() {
		let change = {
			type: "AddClass",
			className: "TestSenders",
			superclass: "Object",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		change = {
			type: "AddMethod",
			className: "TestSenders",
			sourceCode: "testSenders",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		change = {
			type: "AddMethod",
			className: "TestSenders",
			sourceCode: "testSenders2 self testSenders",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		change = {
			type: "AddClass",
			className: "TestSenders2",
			superclass: "Object",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		change = {
			type: "AddMethod",
			className: "TestSenders2",
			sourceCode: "testSenders TestSenders new testSenders",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		try {
			const senders = await this.get("/methods?sending=testSenders");
			this.assertIsArray(senders);
			this.assertNotEmpty(senders);
			this.assertAllSatisfy(
				senders,
				(m) => m.source.includes("testSenders"),
				"method.source includes 'testSenders'"
			);
			this.assertAnySatisfy(
				senders,
				(m) => m.methodClass === "TestSenders",
				"method.methodClass equals 'TestSenders'"
			);
			const local = await this.get(
				"/methods?sending=testSenders&class=TestSenders"
			);
			this.assertIsArray(local);
			this.assertNotEmpty(local);
			this.assertAllSatisfy(
				local,
				(m) =>
					m.source.includes("testSenders") &&
					m.methodClass === "TestSenders",
				"method.source includes 'testSenders' and method.methodClass equals 'TestSenders'"
			);
		} finally {
			change = {
				type: "RemoveMethod",
				className: "TestSenders",
				selector: "testSenders",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
			change = {
				type: "RemoveMethod",
				className: "TestSenders",
				selector: "testSenders2",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
			change = {
				type: "RemoveMethod",
				className: "TestSenders2",
				selector: "testSenders",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
			change = {
				type: "RemoveClass",
				className: "TestSenders",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
			change = {
				type: "RemoveClass",
				className: "TestSenders2",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
		}
	}

	async testCode_Subclasses() {
		let change = {
			type: "AddClass",
			className: "TestSubclasses",
			superclass: "Object",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		change = {
			type: "AddClass",
			className: "TestSubclasses2",
			superclass: "TestSubclasses",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		try {
			const subclasses = await this.get(
				"/classes/TestSubclasses/subclasses"
			);
			this.assertIsArray(subclasses);
			this.assertAnySatisfy(
				subclasses,
				(c) => c.name === "TestSubclasses2",
				"class.name equals 'TestSubclasses2'"
			);
		} finally {
			change = {
				type: "RemoveClass",
				className: "TestSubclasses2",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
			change = {
				type: "RemoveClass",
				className: "TestSubclasses",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
		}
	}

	async testCode_Superclasses() {
		let change = {
			type: "AddClass",
			className: "TestSuperclasses",
			superclass: "Object",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		change = {
			type: "AddClass",
			className: "TestSuperclasses2",
			superclass: "TestSuperclasses",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		try {
			const subclasses = await this.get(
				"/classes/TestSuperclasses2/superclasses"
			);
			this.assertIsArray(subclasses);
			this.assertAnySatisfy(
				subclasses,
				(c) => c.name === "TestSuperclasses",
				"class.name equals 'TestSuperclasses'"
			);
		} finally {
			change = {
				type: "RemoveClass",
				className: "TestSuperclasses2",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
			change = {
				type: "RemoveClass",
				className: "TestSuperclasses",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
		}
	}

	async testCode_Variables() {
		let change = {
			type: "AddClass",
			className: "TestVariables",
			instanceVariables: ["abc"],
			classVariables: ["Xyz"],
			superclass: "Object",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		try {
			const variables = await this.get(
				"/classes/TestVariables/variables"
			);
			this.assertAnySatisfy(
				variables,
				(v) => v.name === "abc" && v.type === "instance",
				"variable.name equals 'abc' and variable.type equals 'instance'"
			);
			this.assertAnySatisfy(
				variables,
				(v) => v.name === "Xyz" && v.type === "class",
				"variable.name equals 'Xyz' and variable.type equals 'class'"
			);
		} finally {
			change = {
				type: "RemoveClass",
				className: "TestVariables",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
		}
	}

	async testCode_UsedCategories() {
		let change = {
			type: "AddClass",
			className: "TestUsedCategories",
			superclass: "Object",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		change = {
			type: "AddMethod",
			className: "TestUsedCategories",
			category: "testUsedCategories",
			sourceCode: "a",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		change = {
			type: "AddClass",
			className: "TestUsedCategories2",
			superclass: "TestUsedCategories",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		try {
			const categories = await this.get(
				"/classes/TestUsedCategories2/used-categories"
			);
			this.assertIsArray(categories);
			this.assertNotEmpty(categories);
			categories.forEach((c) => this.assertIsString(c, "category"));
			this.assertIncludes(
				categories,
				"testUsedCategories",
				"used categories"
			);
		} finally {
			change = {
				type: "RemoveClass",
				className: "TestUsedCategories2",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
			change = {
				type: "RemoveMethod",
				className: "TestUsedCategories",
				selector: "a",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
			change = {
				type: "RemoveClass",
				className: "TestUsedCategories",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
		}
	}

	// Changes tests...

	async testChanges_AddClass() {
		let change = {
			type: "AddClass",
			className: "TestAddClass",
			superclass: "Object",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		try {
			let species = await this.get("/classes/TestAddClass");
			this.assertNotNull(species, "class not defined");
			this.assertEquals(species.name, "TestAddClass", "class.name");
		} finally {
			change = {
				type: "RemoveClass",
				className: "TestAddClass",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
		}
	}

	async testChanges_AddClassVariable() {
		let change = {
			type: "AddClass",
			className: "TestAddClassVariable",
			superclass: "Object",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		change = {
			type: "AddClassVariable",
			className: "TestAddClassVariable",
			variable: "X",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		try {
			let variables = await this.get(
				"/classes/TestAddClassVariable/class-variables"
			);
			this.assertAnySatisfy(
				variables,
				(v) => v.name === "X",
				"variable.name"
			);
		} finally {
			change = {
				type: "RemoveClass",
				className: "TestAddClassVariable",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
		}
	}

	async testChanges_AddInstanceVariable() {
		let change = {
			type: "AddClass",
			className: "TestAddInstanceVariable",
			superclass: "Object",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		change = {
			type: "AddInstanceVariable",
			className: "TestAddInstanceVariable",
			variable: "xxx",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		try {
			let variables = await this.get(
				"/classes/TestAddInstanceVariable/instance-variables"
			);
			this.assertEquals(variables.length, 1, "data.length");
			this.assertEquals(variables[0].name, "xxx", "variable.name");
		} finally {
			change = {
				type: "RemoveClass",
				className: "TestAddInstanceVariable",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
		}
	}

	async testChanges_AddMethod() {
		let change = {
			type: "AddClass",
			className: "TestAddMethod",
			instanceVariables: ["a"],
			superclass: "Object",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		change = {
			type: "AddMethod",
			className: "TestAddMethod",
			category: "WebsideTest",
			sourceCode: "testAddMethod\n^a + 1",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		try {
			let method = await this.get(
				"/classes/TestAddMethod/methods/testAddMethod"
			);
			this.assertNotNull(method);
			this.assertEquals(
				method.selector,
				"testAddMethod",
				"method.selector"
			);
			this.assertEquals(
				method.category,
				"WebsideTest",
				"method.category"
			);
			this.assertStartsWith(
				method.source,
				"testAddMethod",
				"method.source"
			);
			this.assertIncludes(method.source, "a + 1", "method.source");
		} finally {
			change = {
				type: "RemoveMethod",
				className: "TestAddMethod",
				selector: "testAddMethod",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
			change = {
				type: "RemoveClass",
				className: "TestAddMethod",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
		}
	}

	async testChanges_AddPackage() {
		let change = {
			type: "AddPackage",
			name: "TestAddPackage",
			author: "Webside",
		};
		await this.post("/changes", change);
		try {
			let pack = await this.get("/packages/TestAddPackage");
			this.assertNotNull(pack);
			this.assertEquals(pack.name, "TestAddPackage", "package.name");
		} finally {
			change = {
				type: "RemovePackage",
				name: "TestAddPackage",
			};
			this.post("/changes", change);
		}
	}

	async testChanges_Changes() {
		let change = {
			type: "AddClass",
			className: "TestChanges",
			superclass: "Object",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		try {
			const changes = await this.get("/changes");
			this.assertNotEmpty(changes);
			this.assertAnySatisfy(
				changes,
				(ch) =>
					ch.type === "AddClass" && ch.className === "TestChanges",
				"change.type equals 'AddClass' and change.className equals 'TestChanges'"
			);
		} finally {
			change = {
				type: "RemoveClass",
				className: "TestChanges",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
		}
	}

	async testChanges_ClassifyMethod() {
		let change = {
			type: "AddClass",
			className: "TestClassifyMethod",
			superclass: "Object",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		change = {
			type: "AddMethod",
			className: "TestClassifyMethod",
			sourceCode: "testClassifyMethod",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		change = {
			type: "ClassifyMethod",
			className: "TestClassifyMethod",
			selector: "testClassifyMethod",
			category: "testClassifyMethod",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		try {
			const method = await this.get(
				"/classes/TestClassifyMethod/methods/testClassifyMethod"
			);
			this.assertEquals(
				method.category,
				"testClassifyMethod",
				"method.category"
			);
		} finally {
			change = {
				type: "RemoveMethod",
				className: "TestClassifyMethod",
				selector: "testClassifyMethod",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
			change = {
				type: "RemoveClass",
				className: "TestClassifyMethod",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
		}
	}

	async testChanges_CodeSuggestion() {
		let change = {
			type: "AddClass",
			className: "TestCodeSuggestion",
			superclass: "Object",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		change = {
			type: "AddMethod",
			className: "TestCodeSuggestion",
			sourceCode: "testCodeSuggestion\n^t + 1",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		let error;
		try {
			try {
				await this.post("/changes", change);
			} catch (e) {
				error = e.error;
			}
			this.assertNotNull(error);
			this.assertNotNull(error.response, "response");
			this.assertEquals(error.response.status, 409, "response.status");
			this.assertNotNull(error.response.data, "response.data");
			const suggestions = error.response.data.suggestions;
			this.assertNotNull(suggestions, "response.data.suggestions");
			this.assertIsArray(suggestions, "response.data.suggestions");
			this.assertNotEmpty(suggestions, "response.data.suggestions");
			await this.post("/changes", suggestions[0].changes[0]);
			const method = await this.get(
				"/classes/TestCodeSuggestion/methods/testCodeSuggestion"
			);
			this.assertNotNull(method);
			this.assertEquals(
				method.selector,
				"testCodeSuggestion",
				"method.selector"
			);
			this.assertIncludes(method.source, "| t |", "method.source");
		} finally {
			change = {
				type: "RemoveMethod",
				className: "TestCodeSuggestion",
				selector: "testCodeSuggestion",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
			change = {
				type: "RemoveClass",
				className: "TestCodeSuggestion",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
		}
	}

	async testChanges_CommentClass() {
		let change = {
			type: "AddClass",
			className: "TestCommentClass",
			superclass: "Object",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		change = {
			type: "CommentClass",
			className: "TestCommentClass",
			comment: "Testing class comment",
		};
		await this.post("/changes", change);
		try {
			const species = this.get("/classes/TestCommentClass");
			this.assertNotNull(species);
		} finally {
			change = {
				type: "RemoveClass",
				className: "TestCommentClass",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
		}
	}

	async testChanges_MoveDownInstanceVariable() {
		let change = {
			type: "AddClass",
			className: "TestMoveDownInstanceVariable",
			superclass: "Object",
			instanceVariables: ["x"],
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		change = {
			type: "AddClass",
			className: "TestMoveDownInstanceVariable2",
			superclass: "TestMoveDownInstanceVariable",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		change = {
			type: "MoveDownInstanceVariable",
			className: "TestMoveDownInstanceVariable",
			variable: "x",
			target: "TestMoveDownInstanceVariable2",
		};
		await this.post("/changes", change);
		try {
			let variables = await this.get(
				"/classes/TestMoveDownInstanceVariable/instance-variables"
			);
			this.assertIsEmpty(variables);
			variables = await this.get(
				"/classes/TestMoveDownInstanceVariable2/instance-variables"
			);
			this.assertEquals(variables.length, 1);
			this.assertEquals(variables[0].name, "x", "variable.name");
		} finally {
			change = {
				type: "RemoveClass",
				className: "TestMoveDownInstanceVariable2",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
			change = {
				type: "RemoveClass",
				className: "TestMoveDownInstanceVariable",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
		}
	}

	async testChanges_MoveUpInstanceVariable() {
		let change = {
			type: "AddClass",
			className: "TestMoveUpInstanceVariable",
			superclass: "Object",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		change = {
			type: "AddClass",
			className: "TestMoveUpInstanceVariable2",
			superclass: "TestMoveUpInstanceVariable",
			instanceVariables: ["x"],
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		change = {
			type: "MoveUpInstanceVariable",
			className: "TestMoveUpInstanceVariable2",
			variable: "x",
			target: "TestMoveUpInstanceVariable",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		try {
			let variables = await this.get(
				"/classes/TestMoveUpInstanceVariable2/instance-variables"
			);
			this.assertAllSatisfy(
				variables,
				(v) => v.class !== "TestMoveUpInstanceVariable2",
				"v.class"
			);
			variables = await this.get(
				"/classes/TestMoveUpInstanceVariable/instance-variables"
			);
			this.assertAnySatisfy(
				variables,
				(v) =>
					v.class === "TestMoveUpInstanceVariable" && v.name === "x",
				"v.class equals 'TestMoveUpInstanceVariable' and v.name equals 'x'"
			);
		} finally {
			change = {
				type: "RemoveClass",
				className: "TestMoveUpInstanceVariable2",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
			change = {
				type: "RemoveClass",
				className: "TestMoveUpInstanceVariable",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
		}
	}

	async testChanges_RemoveCategory() {
		let change = {
			type: "AddClass",
			className: "TestRemoveCategory",
			superclass: "Object",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		change = {
			type: "AddMethod",
			className: "TestRemoveCategory",
			category: "testRemoveCategory",
			sourceCode: "testRemoveCategory",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		try {
			let categories = await this.get(
				"/classes/TestRemoveCategory/categories"
			);
			this.assertIncludes(categories, "testRemoveCategory");
			change = {
				type: "RemoveMethod",
				className: "TestRemoveCategory",
				selector: "testRemoveCategory",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
			change = {
				type: "RemoveCategory",
				className: "TestRemoveCategory",
				category: "testRemoveCategory",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
			categories = await this.get(
				"/classes/TestRemoveCategory/categories"
			);
			this.deny(
				categories.includes("testRemoveCategory"),
				"categories should not include deleted category"
			);
		} finally {
			let remotion = {
				type: "RemoveClass",
				className: "TestRemoveCategory",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", remotion);
		}
	}

	async testChanges_RemoveClass() {
		let change = {
			type: "AddClass",
			className: "TestRemoveClass",
			superclass: "Object",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		try {
			change = { type: "RemoveClass", className: "TestRemoveClass" };
			await this.post("/changes", change);
			let species;
			try {
				species = await this.get("/classes/TestRemoveClass");
			} catch (ignored) {}
			this.assert(!species);
		} finally {
			change = {
				type: "RemoveClass",
				className: "TestRemoveClass",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			try {
				await this.post("/changes", change);
			} catch (ignored) {}
		}
	}

	async testChanges_RemoveInstanceVariable() {
		let change = {
			type: "AddClass",
			className: "TestRemoveInstanceVariable",
			superclass: "Object",
			instanceVariables: ["x"],
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		change = {
			type: "RemoveInstanceVariable",
			className: "TestRemoveInstanceVariable",
			variable: "x",
		};
		await this.post("/changes", change);
		try {
			const variables = await this.get(
				"/classes/TestRemoveInstanceVariable/instance-variables"
			);
			this.assertIsEmpty(variables);
		} finally {
			change = {
				type: "RemoveClass",
				className: "TestRemoveInstanceVariable",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			this.post("/changes", change);
		}
	}

	async testChanges_RemoveMethod() {
		let change = {
			type: "AddClass",
			className: "TestRemoveMethod",
			superclass: "Object",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		change = {
			type: "AddMethod",
			className: "TestRemoveMethod",
			sourceCode: "testRemoveMethod",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		change = {
			type: "RemoveMethod",
			className: "TestRemoveMethod",
			selector: "testRemoveMethod",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		try {
			const methods = await this.get("/classes/TestRemoveMethod/methods");
			this.assertIsEmpty(methods);
		} finally {
			change = {
				type: "RemoveClass",
				className: "TestRemoveMethod",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
		}
	}

	async testChanges_RenameCategory() {
		let change = {
			type: "AddClass",
			className: "TestRenameCategory",
			superclass: "Object",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		change = {
			type: "AddMethod",
			className: "TestRenameCategory",
			category: "testRenameCategory",
			sourceCode: "testRenameCategory",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		change = {
			type: "RenameCategory",
			className: "TestRenameCategory",
			category: "testRenameCategory",
			newName: "testRenameCategory2",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		try {
			const categories = await this.get(
				"/classes/TestRenameCategory/categories"
			);
			this.assertIncludes(categories, "testRenameCategory2");
			this.deny(
				categories.includes("testRenameCategory", "categories"),
				"categories should not include testRenameCategory"
			);
		} finally {
			change = {
				type: "RemoveClass",
				className: "TestRenameCategory",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
		}
	}

	async testChanges_RenameClass() {
		let change = {
			type: "AddClass",
			className: "TestRenameClass",
			superclass: "Object",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		change = {
			type: "RenameClass",
			className: "TestRenameClass",
			newName: "TestRenameClass2",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		try {
			let species, error;
			try {
				species = await this.get("/classes/TestRenameClass");
			} catch (e) {
				error = e;
			}
			this.assert(!species, "TestRenameClass should have been renamed");
			this.assertNotNull(
				error,
				"TestRenameClass should have been renamed"
			);
			species = await this.get("/classes/TestRenameClass2");
			this.assertNotNull(species, "TestRenameClass2 should be present");
		} finally {
			change = {
				type: "RemoveClass",
				className: "TestRenameClass2",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
		}
	}

	async testChanges_RenameInstanceVariable() {
		let change = {
			type: "AddClass",
			className: "TestRenameInstanceVariable",
			superclass: "Object",
			instanceVariables: ["x"],
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		change = {
			type: "RenameInstanceVariable",
			className: "TestRenameInstanceVariable",
			variable: "x",
			newName: "y",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		try {
			const variables = await this.get(
				"/classes/TestRenameInstanceVariable/instance-variables"
			);
			this.assertEquals(variables.length, 1, "variables.length");
			this.assertEquals(variables[0].name, "y", "variable.name");
		} finally {
			change = {
				type: "RemoveClass",
				className: "TestRenameInstanceVariable",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
		}
	}

	async testChanges_RenameMethod() {
		let change = {
			type: "AddClass",
			className: "TestRenameMethod",
			superclass: "Object",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		change = {
			type: "AddMethod",
			className: "TestRenameMethod",
			sourceCode: "testRenameMethod",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		change = {
			type: "RenameMethod",
			className: "TestRenameMethod",
			selector: "testRenameMethod",
			newSelector: "testRenameMethod2",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		try {
			const methods = await this.get("/classes/TestRenameMethod/methods");
			this.assertNoneSatisfy(
				methods,
				(m) => m.selector === "testRenameMethod",
				"m.selector equals 'testRenameMethod'"
			);
			this.assertAnySatisfy(
				methods,
				(m) => m.selector === "testRenameMethod2",
				"m.selector equals 'testRenameMethod2'"
			);
		} finally {
			change = {
				type: "RemoveMethod",
				className: "TestRenameMethod",
				selector: "testRenameMethod2",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
			change = {
				type: "RemoveClass",
				className: "TestRenameMethod",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
		}
	}

	async testChanges_RenamePackage() {
		let change = {
			type: "AddPackage",
			name: "TestRenamePackage",
			author: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		change = {
			type: "RenamePackage",
			name: "TestRenamePackage",
			newName: "TestRenamePackage2",
			author: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		try {
			const pack = await this.get("/packages/TestRenamePackage2");
			this.assertNotNull(pack);
			this.assertEquals(pack.name, "TestRenamePackage2", "pack.name");
		} finally {
			change = {
				type: "RemovePackage",
				name: "TestRenamePackage2",
				author: "Webside_BackendTest",
			};
			await this.post("/changes", change);
		}
	}

	// Evaluation tests...

	async testEvaluations_ActiveEvaluations() {
		let evaluation = {
			expression: "(Delay forSeconds: 1000) wait. 26",
			sync: false,
		};
		const result = await this.post("/evaluations", evaluation);
		try {
			const active = await this.get("/evaluations");
			this.assertIsArray(active);
			this.assertAnySatisfy(
				active,
				(e) => e.id === result.id,
				"evaluation.id equals " + result.id
			);
		} finally {
			await this.delete("/evaluations/" + result.id);
		}
	}

	async testEvaluations_AsynchronousEvaluation() {
		let payload = {
			expression: "3 + 4",
			sync: false,
		};
		let evaluation = await this.post("/evaluations", payload);
		let id;
		try {
			this.assertEquals(
				this.log[this.log.length - 1].response.status,
				201,
				"HTTP response code"
			);
			id = evaluation.id;
			this.assertEquals(evaluation.state, "pending", "evaluation.state");
			let attempts = 0;
			do {
				evaluation = await this.get("/evaluations/" + id);
				if (attempts <= 5 && evaluation.state !== "finished") {
					this.assert(
						["pending", "evaluating"].includes(evaluation.state),
						"evaluation.state should be either 'pending' or 'evaluating'"
					);
					attempts++;
					await this.wait(200);
				} else {
					break;
				}
			} while (true);
			this.assertEquals(evaluation.state, "finished", "evaluation.state");
			const object = await this.get("/objects/" + id);
			this.assertNotNull(object);
			this.assertEquals(object.id, id, "object.id");
			this.assertEquals(object.class, "SmallInteger", "object.class");
			this.assertEquals(object.printString, "7", "object.printString");
		} finally {
			try {
				this.delete("/objects/" + id);
			} catch (ignored) {}
		}
	}

	async testEvaluations_AsynchronousEvaluationError() {
		let payload = {
			expression: "3 + ",
			sync: false,
		};
		let evaluation = await this.post("/evaluations", payload);
		let id;
		try {
			this.assertIncludes(
				["pending", "failed"],
				evaluation.state,
				"non-compilable expression might fail immediately"
			);
			id = evaluation.id;
			let attempts = 0;
			do {
				evaluation = await this.get("/evaluations/" + id);
				if (attempts <= 3 && evaluation.state !== "failed") {
					this.assert(
						["pending", "evaluating"].includes(evaluation.state),
						"evaluation.state should be either 'pending' or 'evaluating'"
					);
					attempts++;
					await this.wait(200);
				} else {
					break;
				}
			} while (true);
			this.assertEquals(evaluation.state, "failed", "evaluation.state");
			this.assertNotNull(evaluation.error, "evaluation.error");
			this.assertIsString(
				evaluation.error.description,
				"error should have a description"
			);
		} finally {
			try {
				await this.delete("/evaluations/" + id);
			} catch (ignored) {}
		}
	}

	async testEvaluations_CancelAsynchronousEvaluation() {
		let payload = {
			expression: "(Delay forSeconds: 10) wait",
			sync: false,
		};
		const evaluation = await this.post("/evaluations", payload);
		await this.delete("/evaluations/" + evaluation.id);
		const active = await this.get("/evaluations");
		this.assertAllSatisfy(
			active,
			(e) => e.id !== evaluation.id,
			"evaluation differs from cancelled evaluation"
		);
	}

	async testEvaluations_DebugExpression() {
		let evaluation = {
			expression: "3 factorial",
			debug: true,
		};
		const id = await this.post("/evaluations", evaluation);
		this.assertNotNull(id);
		await this.post("/debuggers/" + id + "/terminate");
		const debuggers = await this.get("/debuggers");
		this.assertAllSatisfy(
			debuggers,
			(d) => d.id !== id,
			"debugger.id should not be equal to the one of just terminated debugger"
		);
		if (debuggers.some((d) => d.id === id)) {
			await this.delete("/debuggers/" + id);
		}
	}

	async testEvaluations_DebuggerContext() {
		let payload = {
			expression: "3 halt factorial",
			sync: true,
		};
		let error, id;
		try {
			await this.post("/evaluations", payload);
		} catch (e) {
			error = e.error.response.data;
			this.assertNotNull(error.evaluation, "error.evaluation");
		}
		payload = { evaluation: error.evaluation };
		const _debugger = await this.post("/debuggers", payload);
		id = _debugger.id;
		await this.post("/debuggers/" + id + "/frames/1/stepinto");
		payload = {
			expression: "self",
			sync: true,
			pin: false,
			context: {
				debugger: id,
				frame: 1,
			},
		};
		const receiver = await this.post("/evaluations", payload);
		this.assertEquals(receiver.class, "SmallInteger", "receiver.class");
		this.assertEquals(receiver.printString, "3", "receiver.printString");
		await this.delete("/debuggers/" + id);
	}

	async testEvaluations_ClassVariableInDebuggerContext() {
		let change = {
			type: "AddClass",
			className: "TestClassVariableInDebuggerContext",
			classVariables: ["Abc"],
			superclass: "Object",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		change = {
			type: "AddMethod",
			className: "TestClassVariableInDebuggerContext",
			sourceCode: "abc Abc",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		let payload = {
			expression: "TestClassVariableInDebuggerContext new halt abc",
			sync: true,
			pin: true,
		};
		let error;
		try {
			await this.post("/evaluations", payload);
		} catch (e) {
			error = e.error.response.data;
			this.assertNotNull(error.evaluation, "error.evaluation");
		}
		payload = { evaluation: error.evaluation };
		const _debugger = await this.post("/debuggers", payload);
		await this.post("/debuggers/" + _debugger.id + "/frames/1/stepinto");
		try {
			payload = {
				expression: "Abc",
				sync: true,
				pin: false,
				context: {
					debugger: _debugger.id,
					frame: 1,
				},
			};
			const receiver = await this.post("/evaluations", payload);
			this.assertEquals(
				receiver.printString,
				"nil",
				"receiver.printString"
			);
		} finally {
			await this.delete("/debuggers/" + _debugger.id);
			change = {
				type: "RemoveMethod",
				className: "TestClassVariableInDebuggerContext",
				selector: "abc",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
			change = {
				type: "RemoveClass",
				className: "TestClassVariableInDebuggerContext",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
		}
	}

	async testEvaluations_ObjectContext() {
		let change = {
			type: "AddClass",
			className: "TestObjectContext",
			instanceVariables: ["abc"],
			superclass: "Object",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		change = {
			type: "AddMethod",
			className: "TestObjectContext",
			sourceCode: "abc: value abc := value",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		let payload = {
			expression: "TestObjectContext new abc: 26",
			sync: true,
			pin: true,
		};
		const object = await this.post("/evaluations", payload);
		payload = {
			expression: "abc",
			sync: true,
			pin: false,
			context: {
				object: object.id,
			},
		};
		const abc = await this.post("/evaluations", payload);
		try {
			this.assertEquals(abc.class, "SmallInteger");
			this.assertEquals(abc.printString, "26");
		} finally {
			try {
				await this.delete("/objects/" + object.id);
			} catch (ignored) {}
			change = {
				type: "RemoveMethod",
				className: "TestObjectContext",
				selector: "abc:",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
			change = {
				type: "RemoveClass",
				className: "TestObjectContext",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
		}
	}

	async testEvaluations_ObjectSlotContext() {
		let change = {
			type: "AddClass",
			className: "TestObjectSlotContext",
			instanceVariables: ["abc"],
			superclass: "Object",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		change = {
			type: "AddMethod",
			className: "TestObjectSlotContext",
			sourceCode: "abc: value abc := value",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		let payload = {
			expression: "TestObjectSlotContext new abc: 26",
			sync: true,
			pin: true,
		};
		const object = await this.post("/evaluations", payload);
		payload = {
			expression: "self",
			sync: true,
			pin: false,
			context: {
				object: object.id + "/abc",
			},
		};
		const abc = await this.post("/evaluations", payload);
		try {
			this.assertEquals(abc.class, "SmallInteger", "object.class");
			this.assertEquals(abc.printString, "26", "object.printString");
		} finally {
			try {
				await this.delete("/objects/" + object.id);
			} catch (ignored) {}
			change = {
				type: "RemoveMethod",
				className: "TestObjectSlotContext",
				selector: "abc:",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
			change = {
				type: "RemoveClass",
				className: "TestObjectSlotContext",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
		}
	}

	async testEvaluations_PauseAndDebugAsynchronousEvaluation() {
		let payload = {
			expression: "(Delay forSeconds: 30) wait",
			sync: false,
		};
		let evaluation = await this.post("/evaluations", payload);
		this.assertEquals(evaluation.state, "pending", "evaluation.state");
		let attempts = 0;
		while (attempts <= 3 && evaluation.state !== "evaluating") {
			attempts++;
			await this.wait(200);
			evaluation = await this.get("/evaluations/" + evaluation.id);
		}
		this.assertEquals(evaluation.state, "evaluating", "evaluation.state");
		await this.post("/evaluations/" + evaluation.id + "/pause");
		evaluation = await this.get("/evaluations/" + evaluation.id);
		this.assertEquals(evaluation.state, "paused", "evaluation.state");
		let target = {
			evaluation: evaluation.id,
		};
		let _debugger = await this.post("/debuggers", target);
		this.assertNotNull(_debugger);
		try {
			if (_debugger !== null) {
				await this.delete("/debuggers/" + _debugger.id);
			}
			await this.delete("/objects/" + evaluation.id);
		} catch (ignored) {}
	}

	async testEvaluations_PauseAndResumeAsynchronousEvaluation() {
		let payload = {
			expression: "(Delay forSeconds: 30) wait",
			sync: false,
		};
		let evaluation = await this.post("/evaluations", payload);
		this.assertEquals(evaluation.state, "pending", evaluation.state);
		let attempts = 0;
		evaluation = await this.get("/evaluations/" + evaluation.id);
		while (attempts <= 3 && evaluation.state !== "evaluating") {
			attempts++;
			await this.wait(200);
			evaluation = await this.get("/evaluations/" + evaluation.id);
		}
		this.assertEquals(evaluation.state, "evaluating", "evaluation.state");
		await this.post("/evaluations/" + evaluation.id + "/pause");
		evaluation = await this.get("/evaluations/" + evaluation.id);
		this.assertEquals(evaluation.state, "paused", "evaluation.state");
		await this.post("/evaluations/" + evaluation.id + "/resume");
		attempts = 0;
		while (attempts <= 3 && evaluation.state !== "finished") {
			attempts++;
			await this.wait(200);
			evaluation = await this.get("/evaluations/" + evaluation.id);
		}
		this.assertEquals(evaluation.state, "finished", "evaluation.state");
		try {
			await this.delete("/objects/" + evaluation.id);
		} catch (ignored) {}
	}

	async testEvaluations_PauseDebugAndResumeAsynchronousEvaluation() {
		let payload = {
			expression:
				"| m | m := 0. [m < 180] whileTrue: [(Delay forSeconds: 1) wait. m := m + 1]",
			sync: false,
		};
		let evaluation = await this.post("/evaluations", payload);
		this.assertEquals(evaluation.state, "pending", "evaluation.state");
		let attempts = 0;
		while (attempts <= 3 && evaluation.state !== "evaluating") {
			attempts++;
			this.wait(200);
			evaluation = await this.get("/evaluations/" + evaluation.id);
		}
		this.assertEquals(evaluation.state, "evaluating", "evaluation.state");
		await this.post("/evaluations/" + evaluation.id + "/pause");
		evaluation = await this.get("/evaluations/" + evaluation.id);
		this.assertEquals(evaluation.state, "paused", "evaluation.state");
		let target = {
			evaluation: evaluation.id,
		};
		let _debugger = await this.post("/debuggers", target);
		await this.post("/debuggers/" + _debugger.id + "/resume");
		evaluation = await this.get("/evaluations/" + evaluation.id);
		this.assertEquals(evaluation.state, "evaluating", "evaluation.state");
		try {
			await this.delete("/evaluations/" + evaluation.id);
			await this.delete("/objects/" + evaluation.id);
		} catch (ignored) {}
	}

	async testEvaluations_PauseDebugAndTerminateAsynchronousEvaluation() {
		let payload = {
			expression:
				"| m | m := 0. [m < 180] whileTrue: [(Delay forSeconds: 1) wait. m := m + 1]",
			sync: false,
		};
		let evaluation = await this.post("/evaluations", payload);
		this.assertEquals(evaluation.state, "pending", "evaluation.state");
		let attempts = 0;
		while (attempts <= 3 && evaluation.state !== "evaluating") {
			attempts++;
			this.wait(200);
			evaluation = await this.get("/evaluations/" + evaluation.id);
		}
		this.assertEquals(evaluation.state, "evaluating", "evaluation.state");
		await this.post("/evaluations/" + evaluation.id + "/pause");
		evaluation = await this.get("/evaluations/" + evaluation.id);
		this.assertEquals(evaluation.state, "paused", "evaluation.state");
		let target = {
			evaluation: evaluation.id,
		};
		let _debugger = await this.post("/debuggers", target);
		this.post("/debuggers/" + _debugger.id + "/terminate");
		let active = await this.get("/evaluations");
		this.assertAllSatisfy(
			active,
			(e) => e.id !== evaluation.id,
			"debugged evaluation should not be present as it was terminated from the debugger"
		);
		try {
			await this.delete("/evaluations/" + evaluation.id);
			await this.delete("/objects/" + evaluation.id);
		} catch (ignored) {}
	}

	async testEvaluations_PinEvaluationResult() {
		let payload = {
			expression: "3 + 4",
			sync: true,
			pin: true,
		};
		const evaluation = await this.post("/evaluations", payload);
		try {
			this.assertNotNull(evaluation);
			this.assertNotNull(evaluation.id);
			this.assertEquals(
				evaluation.class,
				"SmallInteger",
				"evaluation.class"
			);
			this.assertEquals(
				evaluation.printString,
				"7",
				"evaluation.printString"
			);
		} finally {
			await this.delete("/objects/" + evaluation.id);
		}
	}

	async testEvaluations_SynchronousEvaluation() {
		let payload = {
			expression: "3 + 4",
			sync: true,
		};
		let evaluation = await this.post("/evaluations", payload);
		this.assertEquals(evaluation.class, "SmallInteger", "evaluation.class");
		this.assertEquals(
			evaluation.printString,
			"7",
			"evaluation.printString"
		);
	}

	async testEvaluations_SynchronousEvaluationError() {
		let payload = {
			expression: "3 + ",
			sync: true,
		};
		let error;
		try {
			await this.post("/evaluations", payload);
		} catch (e) {
			error = e.error.response.data;
		}
		this.assertNotNull(error);
		this.assertIsString(error.description, "error.description");
	}

	async testEvaluations_TemporaryVariableInDebuggerContext() {
		let change = {
			type: "AddClass",
			className: "TestTemporaryVariableInDebuggerContext",
			superclass: "Object",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		change = {
			type: "AddMethod",
			className: "TestTemporaryVariableInDebuggerContext",
			sourceCode:
				"testTemporaryVariableInDebuggerContext  | temp | temp := 26. self halt",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		let payload = {
			expression:
				"TestTemporaryVariableInDebuggerContext new testTemporaryVariableInDebuggerContext",
			sync: true,
			pin: true,
		};
		let error;
		try {
			await this.post("/evaluations", payload);
		} catch (e) {
			error = e.error.response.data;
			this.assertNotNull(error.evaluation, "error.evaluation");
		}
		payload = { evaluation: error.evaluation };
		const _debugger = await this.post("/debuggers", payload);
		try {
			payload = {
				expression: "temp",
				sync: true,
				pin: false,
				context: {
					debugger: _debugger.id,
					frame: 1,
				},
			};
			const receiver = await this.post("/evaluations", payload);
			this.assertEquals(
				receiver.printString,
				"26",
				"receiver.printString"
			);
		} finally {
			await this.delete("/debuggers/" + _debugger.id);
			change = {
				type: "RemoveMethod",
				className: "TestTemporaryVariableInDebuggerContext",
				selector: "testTemporaryVariableInDebuggerContext",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
			change = {
				type: "RemoveClass",
				className: "TestTemporaryVariableInDebuggerContext",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
		}
	}

	// Object tests...

	async testObjects_PinObjectSlot() {
		let change = {
			type: "AddClass",
			className: "TestPinObjectSlot",
			instanceVariables: ["v"],
			superclass: "Object",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		change = {
			type: "AddMethod",
			className: "TestPinObjectSlot",
			sourceCode: "v: value v := value",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		let evaluation = {
			expression: "TestPinObjectSlot new v: 26",
			sync: true,
			pin: true,
		};
		const object = await this.post("/evaluations", evaluation);
		let target = {
			uri: "/objects/" + object.id + "/v",
		};
		const pinned = await this.post("/objects", target);
		try {
			this.assertNotNull(pinned);
			this.deny(pinned.id === object.id);
			const v = await this.get("/objects/" + pinned.id);
			this.assertEquals(v.class, "SmallInteger", "object.class");
			this.assertEquals(v.printString, "26", "object.printString");
		} finally {
			try {
				await this.delete("/objects/" + object.id);
				await this.delete("/objects/" + pinned.id);
			} catch (ignored) {}
			change = {
				type: "RemoveMethod",
				className: "TestPinObjectSlot",
				selector: "v:",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
			change = {
				type: "RemoveClass",
				className: "TestPinObjectSlot",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
		}
	}

	async testObjects_PinnedObject() {
		let evaluation = {
			expression: "3 + 4",
			sync: true,
			pin: true,
		};
		const object = await this.post("/evaluations", evaluation);
		try {
			const pinned = await this.get("/objects/" + object.id);
			this.assertEquals(pinned.id, object.id, "object.id");
			this.assertEquals(pinned.class, "SmallInteger", "object.class");
			this.assertEquals(pinned.printString, "7", "object.printString");
		} finally {
			await this.delete("/objects/" + object.id);
		}
	}

	async testObjects_PinnedObjectIndexedSlots() {
		let evaluation = {
			expression: "#(true 2 nil)",
			sync: true,
			pin: true,
		};
		const object = await this.post("/evaluations", evaluation);
		try {
			this.assertEquals(object.class, "Array", "object.class");
			this.assert(
				object.hasIndexedSlots,
				"object.hasIndexedSlots should be true"
			);
			let slots = await this.get(
				"/objects/" + object.id + "/indexed-slots"
			);
			this.assertEquals(slots.length, 3, "slots.length");
			this.assertAnySatisfy(
				slots,
				(s) => s.slot === 1 && s.printString === "true",
				"element.slot equals true"
			);
			this.assertAnySatisfy(
				slots,
				(s) => s.slot === 2 && s.printString === "2",
				"element.slot equals 2"
			);
			this.assertAnySatisfy(
				slots,
				(s) => s.slot === 3 && s.printString === "nil",
				"element.slot equals nil"
			);
			const slot = await this.get("/objects/" + object.id + "/3");
			this.assertEquals(slot.printString, "nil", "object.printString");
			slots = await this.get(
				"/objects/" + object.id + "/indexed-slots?from=2&to=2"
			);
			this.assertEquals(slots.length, 1, "slots.length");
			this.assert(slots[0].slot === 2 && slots[0].printString === "2");
		} finally {
			await this.delete("/objects/" + object.id);
		}
	}

	async testObjects_PinnedObjectInstanceVariables() {
		let change = {
			type: "AddClass",
			className: "TestPinnedObjectInstanceVariables",
			instanceVariables: ["v"],
			superclass: "Object",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		change = {
			type: "AddMethod",
			className: "TestPinnedObjectInstanceVariables",
			sourceCode: "v: value v := value",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		let evaluation = {
			expression: "TestPinnedObjectInstanceVariables new v: 26",
			sync: true,
			pin: true,
		};
		const object = await this.post("/evaluations", evaluation);
		try {
			this.assertEquals(
				object.class,
				"TestPinnedObjectInstanceVariables",
				"object.class"
			);
			let vars = await this.get(
				"/objects/" + object.id + "/instance-variables"
			);
			this.assertEquals(vars.length, 1, "variables.length");
			this.assertEquals(vars[0].name, "v", "variable.name");
		} finally {
			try {
				await this.delete("/objects/" + object.id);
			} catch (ignored) {}
			change = {
				type: "RemoveMethod",
				className: "TestPinnedObjectInstanceVariables",
				selector: "v:",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
			change = {
				type: "RemoveClass",
				className: "TestPinnedObjectInstanceVariables",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
		}
	}

	async testObjects_PinnedObjectNamedSlots() {
		let change = {
			type: "AddClass",
			className: "TestPinnedObjectNamedSlots",
			instanceVariables: ["v"],
			superclass: "Object",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		change = {
			type: "AddMethod",
			className: "TestPinnedObjectNamedSlots",
			sourceCode: "v: value v := value",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		let evaluation = {
			expression: "TestPinnedObjectNamedSlots new v: 26",
			sync: true,
			pin: true,
		};
		const object = await this.post("/evaluations", evaluation);
		try {
			this.assertEquals(
				object.class,
				"TestPinnedObjectNamedSlots",
				"object.class"
			);
			this.assert(
				object.hasNamedSlots,
				"object.hasNamedSlots should be true"
			);
			const slots = await this.get(
				"/objects/" + object.id + "/named-slots"
			);
			this.assertEquals(slots.length, 1, "slots.length");
			this.assert(
				slots[0].slot === "v" && slots[0].printString === "26",
				"slots should include one such that s.slot equals 'v' and s.printstring is '26'"
			);
		} finally {
			try {
				await this.delete("/objects/" + object.id);
			} catch (ignored) {}
			change = {
				type: "RemoveMethod",
				className: "TestPinnedObjectNamedSlots",
				selector: "v:",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
			change = {
				type: "RemoveClass",
				className: "TestPinnedObjectNamedSlots",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
		}
	}

	async testObjects_PinnedObjectSlot() {
		let change = {
			type: "AddClass",
			className: "TestPinnedObjectSlot",
			instanceVariables: ["v"],
			superclass: "Object",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		change = {
			type: "AddMethod",
			className: "TestPinnedObjectSlot",
			sourceCode: "v: value v := value",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		let evaluation = {
			expression: "TestPinnedObjectSlot new v: 26",
			sync: true,
			pin: true,
		};
		const object = await this.post("/evaluations", evaluation);
		try {
			this.assertEquals(
				object.class,
				"TestPinnedObjectSlot",
				"object.class"
			);
			const v = await this.get("/objects/" + object.id + "/v");
			this.assertEquals(v.class, "SmallInteger", "object.class");
			this.assertEquals(v.printString, "26", "object.printString");
		} finally {
			try {
				await this.delete("/objects/" + object.id);
			} catch (ignored) {}
			change = {
				type: "RemoveMethod",
				className: "TestPinnedObjectSlot",
				selector: "v:",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
			change = {
				type: "RemoveClass",
				className: "TestPinnedObjectSlot",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
		}
	}

	async testObjects_PinnedObjects() {
		let evaluation = {
			expression: "3 + 4",
			sync: true,
			pin: true,
		};
		const object = await this.post("/evaluations", evaluation);
		try {
			const pinned = await this.get("/objects");
			this.assertAnySatisfy(
				pinned,
				(o) =>
					o.id === object.id &&
					o.class === "SmallInteger" &&
					o.printString === "7",
				"element is 7"
			);
		} finally {
			await this.delete("/objects/" + object.id);
		}
	}

	async testObjects_UnpinObject() {
		let evaluation = {
			expression: "3 + 4",
			sync: true,
			pin: true,
		};
		const object = await this.post("/evaluations", evaluation);
		await this.delete("/objects/" + object.id);
		const pinned = await this.get("/objects");
		this.assertAllSatisfy(
			pinned,
			(o) => o.id !== object.id,
			"element differs from removed object"
		);
	}

	// Debugger tests...

	async testDebuggers_CreateDebugger() {
		let evaluation = {
			expression: "1 halt factorial",
			sync: true,
		};
		let error;
		try {
			await this.post("/evaluations", evaluation);
		} catch (e) {
			error = e.error.response.data;
			this.assertIncludes(
				error.description.toLowerCase(),
				"halt",
				"error.description"
			);
			this.assertNotNull(error.evaluation);
		}
		let _debugger;
		try {
			let target = {
				evaluation: error.evaluation,
			};
			_debugger = await this.post("/debuggers", target);
			this.assertNotNull(_debugger);
			const frames = await this.get(
				"/debuggers/" + _debugger.id + "/frames"
			);
			this.assert(frames.length > 0, "debugger should have frames");
		} finally {
			if (_debugger !== null) {
				await this.delete("/debuggers/" + _debugger.id);
			}
		}
	}

	async testDebuggers_DebuggerFrames() {
		let evaluation = {
			expression: "1 halt factorial",
			sync: true,
		};
		let error;
		try {
			await this.post("/evaluations", evaluation);
		} catch (e) {
			error = e.error.response.data;
			this.assertIncludes(
				error.description.toLowerCase(),
				"halt",
				"error.description"
			);
			this.assertNotNull(error.evaluation);
		}
		let target = {
			evaluation: error.evaluation,
		};
		const _debugger = await this.post("/debuggers", target);
		const frames = await this.get("/debuggers/" + _debugger.id + "/frames");
		this.assertNotEmpty(frames, "frames");
		this.assertAllSatisfy(
			frames,
			(f) => f.label !== "",
			"frame.label should not be empty"
		);
		await this.delete("/debuggers/" + _debugger.id);
	}

	async testDebuggers_FrameBindings() {
		let evaluation = {
			expression: "1 halt factorial",
			sync: true,
		};
		let error;
		try {
			await this.post("/evaluations", evaluation);
		} catch (e) {
			error = e.error.response.data;
			this.assertIncludes(
				error.description.toLowerCase(),
				"halt",
				"error.description"
			);
			this.assertNotNull(error.evaluation);
		}
		let target = {
			evaluation: error.evaluation,
		};
		const _debugger = await this.post("/debuggers", target);
		const frames = await this.get("/debuggers/" + _debugger.id + "/frames");
		this.assertNotEmpty(frames, "frames");
		const bindings = await this.get(
			"/debuggers/" + _debugger.id + "/frames/1/bindings"
		);
		this.assertNotEmpty(bindings, "bindings");
		this.assertAnySatisfy(
			bindings,
			(b) => b.name === "self",
			"binding.name is 'self'"
		);
		await this.delete("/debuggers/" + _debugger.id);
	}

	async testDebuggers_StepInto() {
		let evaluation = {
			expression: "1 halt factorial",
			sync: true,
		};
		let error;
		try {
			await this.post("/evaluations", evaluation);
		} catch (e) {
			error = e.error.response.data;
			this.assertIncludes(
				error.description.toLowerCase(),
				"halt",
				"error.description"
			);
			this.assertNotNull(error.evaluation);
		}
		let target = {
			evaluation: error.evaluation,
		};
		const _debugger = await this.post("/debuggers", target);
		const frames = await this.get("/debuggers/" + _debugger.id + "/frames");
		this.assertNotEmpty(frames, "frames");
		await this.post("/debuggers/" + _debugger.id + "/frames/1/stepinto");
		const frame = await this.get(
			"/debuggers/" + _debugger.id + "/frames/1"
		);
		this.assertEquals(
			frame.method.selector,
			"factorial",
			"frame.method.selector"
		);
		await this.delete("/debuggers/" + _debugger.id);
	}

	// Testing tests...

	async testTesting_RunTests() {
		let change = {
			type: "AddClass",
			className: "TestRunTests",
			superclass: "TestCase",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		change = {
			type: "AddMethod",
			className: "TestRunTests",
			sourceCode: "testRunTests self assert: true",
			author: "Webside_BackendTest",
			package: "Webside_BackendTest",
		};
		await this.post("/changes", change);
		let test = {
			class: "TestRunTests",
			selector: "testRunTests",
		};
		let suite = {
			methods: [test],
		};
		const run = await this.post("/test-runs", suite);
		this.assertNotNull(run);
		let attempts = 0;
		let status;
		try {
			while (attempts <= 3 && (!status || status.running)) {
				attempts++;
				this.wait(200);
				status = await this.get("/test-runs/" + run.id + "/status");
				this.assertNotNull(status, "status");
			}
			const results = await this.get("/test-runs/" + run.id + "/results");
			this.assertAllSatisfy(
				["passed", "failed", "errors", "skipped"],
				(s) => results[s],
				"results should have all keys: 'passed', 'failed', 'errors' and 'skipped'"
			);
			this.assertAnySatisfy(
				results.passed,
				(t) =>
					t.class === "TestRunTests" && t.selector === "testRunTests",
				"test.class is 'TestRunTests' and t.selector is 'testRunTests'"
			);
		} finally {
			await this.delete("/test-runs/" + run.id);
			change = {
				type: "RemoveMethod",
				className: "TestRunTests",
				selector: "testRunTests",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
			change = {
				type: "RemoveClass",
				className: "TestRunTests",
				author: "Webside_BackendTest",
				package: "Webside_BackendTest",
			};
			await this.post("/changes", change);
		}
	}

	// Workspace tests...

	async testWorkspaces_Workspace() {
		let workspace = await this.post("/workspaces");
		try {
			let context = { workspace: workspace.id };
			let evaluation = {
				expression: "a := 1",
				sync: true,
				context: context,
			};
			await this.post("/evaluations", evaluation);
			evaluation = { expression: "a", sync: true, context: context };
			const result = await this.post("/evaluations", evaluation);
			this.assertEquals(result.class, "SmallInteger", "result.class");
			this.assertEquals(result.printString, "1", "result.printString");
		} finally {
			await this.delete("/workspaces/" + workspace.id);
		}
	}

	async testWorkspaces_WorkspaceBindings() {
		let workspace = await this.post("/workspaces");
		try {
			let context = { workspace: workspace.id };
			let evaluation = {
				expression: "a := 1",
				sync: true,
				context: context,
			};
			await this.post("/evaluations", evaluation);
			const bindings = await this.get(
				"/workspaces/" + workspace.id + "/bindings"
			);
			this.assertEquals(bindings.length, 1, "bindings.length");
			this.assertEquals(bindings[0].name, "a", "binding.name");
			this.assertEquals(bindings[0].value, "1", "binding.value");
		} finally {
			await this.delete("/workspaces/" + workspace.id);
		}
	}

	async testWorkspaces_WorkspaceEvaluationError() {
		let workspace = await this.post("/workspaces");
		try {
			let context = { workspace: workspace.id };
			let evaluation = {
				expression: "1 + ",
				sync: true,
				context: context,
			};
			let error;
			try {
				await this.post("/evaluations", evaluation);
			} catch (e) {
				error = e.error.response.data;
			}
			this.assertNotNull(error);
			let description = error.description.toLowerCase();
			this.assert(
				description.includes("primary missing") ||
					description.includes("variable or expression expected"),
				"error description should mention that a primary or variable is missing"
			);
		} finally {
			await this.delete("/workspaces/" + workspace.id);
		}
	}

	async testWorkspaces_Workspaces() {
		const workspace = await this.post("/workspaces");
		try {
			const workspaces = await this.get("/workspaces");
			this.assertAnySatisfy(
				workspaces,
				(w) => w.id === workspace.id,
				"workspace.id is the one of just created workspace"
			);
		} finally {
			await this.delete("/workspaces/" + workspace.id);
		}
	}
}

export { BackendTest, BackendTestSuite };
