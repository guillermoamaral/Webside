class StMethod extends Object {
	constructor() {
		super();
		this.methodClass = null;
		this.selector = null;
		this.category = "undefined";
		this.source = "no source";
		this.author = null;
		this.timestamp = null;
		this.package = null;
		this.overriding = false;
		this.overriden = false;
		this.ast = null;
		this.bytecodes = null;
		this.disassembly = null;
		this.template = false;
	}

	static newTemplate() {
		var template = new StMethod();
		template.selector = "<new>";
		template.template = true;
		template.source =
			'messagePattern\r\t"comment"\r\t| temporaries |\r\tstatements';
		return template;
	}

	fromJson(json) {
		super.fromJson(json);
		this.methodClass = json.methodClass;
		this.selector = json.selector;
		this.category = json.category;
		this.source = json.source;
		this.author = json.author;
		this.timestamp = json.timestamp;
		this.package = json.package;
		this.overriding = json.overriding;
		this.overriden = json.overriden;
		this.ast = json.ast;
		this.bytecodes = json.bytecodes;
		this.disassembly = json.disassembly;
	}

	isTest() {
		return this.selector.startsWith("test");
	}

	isTemplate() {
		return this.template;
	}
}

export default StMethod;
