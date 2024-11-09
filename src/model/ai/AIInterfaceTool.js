class AIFunctionParameter {
	constructor(name, type, description = "", required = false) {
		this.name = name;
		this.type = type;
		this.description = description;
		this.required = required;
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
	constructor(name = "", description = "") {
		this.name = name;
		this.description = description;
	}

	asJson() {
		return {
			type: this.type(),
		};
	}
}

class AIFunction extends AIInterfaceTool {
	constructor(name, description) {
		super(name, description);
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
		const fx = {
			name: this.name,
			description: this.description,
			parameters: params,
		};
		if (this.unit) {
			fx.unit = this.unit.asJson();
		}
		const json = super.asJson();
		json.function = fx;
		return json;
	}

	addParameter(parameter) {
		this.parameters.push(parameter);
	}

	type() {
		return "function";
	}

	addStringParameter(name, description, required) {
		const parameter = new AIFunctionParameter(
			name,
			"string",
			description,
			required
		);
		this.addParameter(parameter);
	}
}

class AIToolCall {
	constructor(id, tool, args) {
		this.id = id;
		this.tool = tool;
		this.arguments = args;
	}

	asJson() {
		let json = { id: this.id, type: this.tool.type() };
		json[this.tool.type()] = {
			name: this.tool.name,
			arguments: JSON.stringify(this.arguments),
		};
		return json;
	}

	async invoke() {
		if (this.tool && this.tool.handler)
			return await this.tool.handler(this.arguments);
	}
}

export { AIInterfaceTool, AIFunction, AIFunctionParameter, AIToolCall };
