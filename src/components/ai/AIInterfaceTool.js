class AIFunctionParameter {
	constructor(name, type, description = "") {
		this.name = name;
		this.type = type;
		this.description = description;
		this.required = false;
	}

	asJson() {
		return {
			type: this.type,
			description: this.description,
		};
	}

	isRequired() {
		return this.required;
	}
}

class AIInterfaceTool {
	constructor() {
		this.name = "";
		this.description = "";
	}

	asJson() {
		return {
			type: this.type(),
		};
	}
}

class AIFunction extends AIInterfaceTool {
	constructor() {
		super();
		this.parameters = [];
		this.handler = (args) => {};
	}

	asJson() {
		const params = {};
		if (this.parameters.length > 0) {
			const properties = {};
			this.parameters.forEach((p) => {
				properties[p.name] = p.asJson();
			});
			params.type = "object";
			params.properties = properties;
			params.required = this.parameters
				.filter((p) => p.isRequired())
				.map((p) => p.name);
		}
		const json = {
			name: this.name,
			description: this.description,
			parameters: params,
		};
		if (this.unit) {
			json.unit = this.unit.asJson();
		}
		return {
			function: json,
		};
	}

	addParameter(parameter) {
		this.parameters.push(parameter);
	}

	type() {
		return "function";
	}

	addStringParameter(name, description) {
		const parameter = AIFunctionParameter(name, "string", description);
		this.addParameter(parameter);
	}

	static searchImplementors() {
		const instance = new this();
		instance.name = "search_implementors";
		instance.description =
			'Search implementors of a given selector. Call this whenever you need to know what are the methods implementing a given message, for example when a customer asks "What are the implementors of <selector>"';
		instance.addStringParameter(
			"selector",
			"The selector that the user would like to search implementors for"
		);
		instance.handler = (args) => {
			// Not implemented yet
			// return args.selector
			// 	.asSymbol()
			// 	.implementors()
			// 	.map((m) => m.toString())
			// 	.join("");
			return "";
		};
		return instance;
	}
}

class AIToolCall {
	constructor(id, tool, args) {
		this.id = id;
		this.tool = tool;
		this.arguments = args;
	}

	asJsonO() {
		let json = { id: this._id, type: this.tool.type() };
		json[this.tool.type()] = {
			name: this.tool.name,
			arguments: JSON.stringify(this.arguments),
		};
		return json;
	}

	invoke() {
		// Not implemented yet
		// return this._tool.handler(this._arguments);
	}
}

export { AIInterfaceTool, AIFunction, AIFunctionParameter, AIToolCall };
