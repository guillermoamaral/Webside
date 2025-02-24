import AICodeAssistantMessage from "./AICodeAssistantMessage";
import { OpenAIInterface } from "./AIInterface";
import { AIFunction } from "./AIInterfaceTool";

class AICodeAssistant {
	constructor(backend) {
		this.backend = backend;
		this.active = false;
		this.interface = this.defaultInterface();
		this.systemPrompt = this.defaultSystemPrompt();
		this.messages = [];
		this.promptContext = "";
		this.initializeMessages();
		this.initializeTools();
	}

	defaultInterface() {
		return new OpenAIInterface();
	}

	defaultSystemPrompt() {
		return `You are an expert Smalltalk programmer.
When I ask for help to analyze, explain or write Smalltalk code, you will reply accordingly.
In your response you will avoid using the words 'Smalltalk' and 'snippet'.`;
	}

	systemPromptForCodeAnalysis() {
		return `You are helping a user analyze Smalltalk code.
Your task is to explain in detail provided code.
If you need to dig in sent messages, you can invoke provided tools.`;
	}

	systemPromptForCodeFromTestGeneration() {
		return `You are helping a user create Smalltalk code based on a given set of test cases the class should honor.
Your task is to generate one or more classes with their methods such that the provided tests pass.
If you find any issue when generating code do say it.`;
	}

	systemPromptForTestGeneration() {
		return `You are helping a user create test cases for a Smalltalk system.
Your task is to generate a test case for the provided code.`;
	}

	initializeMessages() {
		const message = AICodeAssistantMessage.systemPrompt(this.systemPrompt);
		this.messages = [message];
	}

	codeResponseFormatSpecification() {
		return `You answer should be in correct Smalltalk, without any \`\`\` marks nor the word Smalltalk, in the following format:
<issue>if you think there's a problem, describe it here, otherwise do not include this tag</issue>
<thinking>your rationale and explanation here, be brief</thinking>
${this.codeFormatSpecification()}`;
	}

	codeFormatSpecification() {
		return `Any Smalltalk code you provide should be in correct Smalltalk, without any \`\`\` marks nor the word Smalltalk.
		If you provide classes the code should be organized in the following format:
		<code>
			<class>
				<superclass>Smalltalk superclass name here</superclass>
				<name>Smalltalk class name here</name>
				<instanceVariables>instance variables here, space-separated</instanceVariables>
				<classVariables>class variables here, space-separated</instanceVariables>
				<method>Smalltalk method code here; avoid including the class and >> before the selector</method>
				<method>Smalltalk method code here; avoid including the class and >> before the selector</method>
				...
			</class>
			<script>Smalltalk code that is neither a class nor a method</script>
		</code>
		If you do not provide classes, avoid the level <class></class> and use directly <method></method>`;
	}

	testFormatSpecification() {
		return `Any Smalltalk code you provide should be in correct Smalltalk, without any \`\`\` marks nor the word Smalltalk.
		Do use testX for test selector and avoid including the class and >> before the selector.
		Your answer should be in correct Smalltalk in the following format:
		<issue>if you think there is a problem, describe it here, otherwise do not include this tag</issue>
		<thinking>your rationale and explanation here, be brief</thinking>
		<code>
			<method>Smalltalk method code for the test here; avoid including the class and >> before the selector</method>.
			<method>Smalltalk method code for the test here; avoid including the class and >> before the selector</method>.
		</code>
		If there is more than one test, enclose each of them between <method> and </method>.`;
	}

	initializeTools() {
		this.tools = [];
		this.tools.push(this.searchImplementorsTool());
		this.tools.push(this.searchSendersTool());
		this.tools.push(this.classDefinitionTool());
		this.tools.push(this.classProtocolTool());
	}

	searchImplementorsTool() {
		const tool = new AIFunction(
			"search_implementors",
			'Search implementors of a given selector. Call this whenever you need to know what are the methods implementing a given message, for example, when the user asks "What are the implementors of <selector>"'
		);
		tool.addStringParameter(
			"selector",
			"The selector that the user would like to search implementors for",
			true
		);
		tool.handler = async (args) => {
			const selector = args.selector;
			if (!selector) return "Selector is missing";
			let implementors = [];
			try {
				implementors = await this.backend.implementors(selector);
			} catch (ignored) {}
			if (implementors.length === 0)
				return `There are no implementors of ${selector}`;
			let result = "";
			implementors.forEach(
				(m) =>
					(result += `\n\n${m.methodClass}>>${m.selector}\n${m.source}`)
			);
			return result;
		};
		return tool;
	}

	searchSendersTool() {
		const tool = new AIFunction(
			"search_senders",
			'Search senders of a given selector. Call this whenever you need to know what are the methods sending a given message, for example, when the user asks "What are the senders of <selector>"'
		);
		tool.addStringParameter(
			"selector",
			"The selector that the user would like to search senders for",
			true
		);
		tool.handler = async (args) => {
			const selector = args.selector;
			if (!selector) return "Selector is missing";
			let senders = [];
			try {
				senders = await this.backend.senders(selector);
			} catch (ignored) {}
			if (senders.length === 0)
				return `There are no senders of ${selector}`;
			let result = "";
			senders.forEach(
				(m) =>
					(result += `\n\n${m.methodClass}>>${m.selector}\n${m.source}`)
			);
			return result;
		};
		return tool;
	}

	classDefinitionTool() {
		const tool = new AIFunction(
			"class_definition",
			'Search the definition of a given class name. Call this whenever you need to know what is the definition of a given class, for example, when the user asks "What is the definition of <classname>"'
		);
		tool.addStringParameter(
			"classname",
			"The name of the class that the user would like to know its definition",
			true
		);
		tool.handler = async (args) => {
			const classname = args.classname;
			if (!classname) return "Class name is missing";
			let species;
			try {
				species = await this.backend.classNamed(classname);
			} catch (ignored) {}
			if (!species) return `There is no class named ${classname}`;
			return species.definition;
		};
		return tool;
	}

	classProtocolTool() {
		const tool = new AIFunction(
			"class_protocol",
			'Search the list of messages that are understood by instances of a given class name. Call this whenever you need to know what is the set of messages (protocol) of a given class, for example, when the user asks "What are the messages an instance of <classname> understands" or "What services does <classname> implements"'
		);
		tool.addStringParameter(
			"classname",
			"The name of the class that the user would like to know its protocol",
			true
		);
		tool.handler = async (args) => {
			const classname = args.classname;
			if (!classname) return "Class name is missing";
			let methods = [];
			try {
				methods = await this.backend.methods(classname);
			} catch (ignored) {}
			if (methods.length === 0)
				return `Class named ${classname} has no methods`;
			let result = "This are the methods implemented by the class:";
			methods.forEach((m) => (result += `\n${m.selector}`));
			return result;
		};
		return tool;
	}

	// Basic

	async sendPrompt(text, visibleText) {
		const prompt = `${this.promptContext}\n${text}`;
		const message = AICodeAssistantMessage.userPrompt(prompt, visibleText);
		return await this.sendMessage(message);
	}

	interfaceName() {
		return this.interface.name();
	}

	useInterface(aiInterface) {
		this.interface = aiInterface;
		if (this.interface) this.interface.tools = this.tools;
	}

	lastMessage() {
		return this.messages[this.messages.length - 1];
	}

	clearHistory() {
		this.initializeMessages();
	}

	// Context

	useCodeContext(code) {
		this.promptContext = `Consider the following Smalltalk code:\n${code}`;
	}

	addClassContext(species) {
		let context = `Consider a class named ${species.name}`;
		if (species.comment) {
			context += ` with this comment:\n${species.comment}`;
		}
		context += ` and the following definition\n${species.definition}`;
		this.promptContext += context;
	}

	async addMethodsContext(species) {
		let context = `Consider the class ${species.name} which defines the following class methods:\n`;
		let methods = await this.backend.methods(species.class);
		methods.forEach((m) => {
			context += `${m.source}\n`;
		});
		context += "and the following instance methods:\n";
		methods = await this.backend.methods(species.name);
		methods.forEach((m) => {
			context += `${m.source}\n`;
		});
		this.promptContext += context;
	}

	async addContextsFrom(text) {
		const pattern = /@([a-zA-Z0-9]+)(#([a-zA-Z0-9]+|\*))?/g;
		let match;
		while ((match = pattern.exec(text)) !== null) {
			const classname = match[1];
			const selector = match[2]
				? match[2].substring(1, match[2].length)
				: null;
			if (selector) {
				if (selector === "*") {
					let species = await this.backend.classNamed(classname);
					this.addMethodsContext(species);
				} else {
					let method = await this.backend.method(classname, selector);
					this.addMethodContext(method);
				}
			} else {
				let species = await this.backend.classNamed(classname);
				this.addClassContext(species);
			}
		}
	}

	useMethodContext(method) {
		this.clearPromptContext();
		this.addMethodContext(method);
	}

	clearPromptContext() {
		this.promptContext = "";
	}

	addMethodContext(method) {
		let context = `Consider a method in a class ${method.methodClass} with selector #${method.selector} and with the following code:\n${method.source}`;
		this.promptContext += context;
	}

	beCodeAnalyzer() {
		this.systemPrompt = this.systemPromptForCodeAnalysis();
	}

	beTestsGenerator() {
		this.systemPrompt = this.systemPromptForTestGeneration();
	}

	beCodeFromTestsGenerator() {
		this.systemPrompt = this.systemPromptForCodeFromTestGeneration();
	}

	useTestsContext(tests) {
		this.promptContext = `Consider the following tests in the form: <test>test code here</test>.\n`;
		tests.forEach((test) => {
			this.promptContext += `<test>${test.source}</test>\n`;
		});
	}

	// Services

	async freePrompt(text) {
		await this.addContextsFrom(text);
		const answer = this.sendPrompt(
			`${text}\n${this.codeFormatSpecification()}`,
			text
		);
		this.clearPromptContext();
		return answer;
	}

	async writeTestForCode(code) {
		this.useCodeContext(code);
		const answer = await this.requestTestsWith(
			"Write a unit test, just one, for the given code."
		);
		this.clearPromptContext();
		return answer;
	}

	async improveMethod(method) {
		this.useMethodContext(method);
		const answer = await this.sendPrompt("Improve the given method.");
		this.clearPromptContext();
		return answer;
	}

	async writeTestsForMethod(method) {
		this.useMethodContext(method);
		const answer = await this.requestTestsWith(
			"Write as many tests as you can for the given method. It is a good idea to generate one test case for simple use cases, border conditions, different argument classes, etc."
		);
		this.clearPromptContext();
		return answer;
	}

	async writeCodeFromTests(tests) {
		this.useTestsContext(tests);
		const answer = await this.requestCodeWith(
			"Provide Smalltalk code that passes given tests."
		);
		this.clearPromptContext();
		return answer;
	}

	async commentMethod(method) {
		this.useMethodContext(method);
		const answer = await this.sendPrompt(
			"Give me a clear and concise comment for the given method, in less than 200 characters."
		);
		this.clearPromptContext();
		return answer.rawContent;
	}

	async explainCode(code) {
		this.useCodeContext(code);
		const answer = await this.sendPrompt(
			"Explain the given code.",
			"Explain the given code."
		);
		this.clearPromptContext();
		return answer;
	}

	async improveCode(code) {
		this.useCodeContext(code);
		const answer = await this.requestCodeWith("Improve the given code.");
		this.clearPromptContext();
		return answer;
	}

	async explainMethod(method) {
		this.useMethodContext(method);
		const answer = await this.sendPrompt(
			"Explain the given method.",
			"Explain the given method."
		);
		this.clearPromptContext();
		return answer.rawContent;
	}

	writeTestForMethod(method) {
		this.useMethodContext(method);
		const answer = this.requestTestsWith(
			"Write a unit test, just one, for the given method.",
			1
		);
		this.clearPromptContext();
		return answer;
	}

	async writeCodeFromDescription(text) {
		return await this.requestCodeWith(text);
	}

	async deduceTypeOfExpression(expression) {
		this.useCodeContext(expression);
		const answer = await this.sendPrompt(
			"What is the type of such expression?"
		);
		this.clearPromptContext();
		return answer.rawContent;
	}

	async suggestCategoryForMethod(method) {
		this.useMethodContext(method);
		const answer = await this.sendPrompt(
			"Suggest a category for the given method. Only provide the category. No explanation. Just a word.",
			`Suggest a category for ${method.methodClass} >> #${method.selector}`
		);
		this.clearPromptContext();
		return answer.rawContent.trim();
	}

	// Private

	async sendMessage(message) {
		this.messages.push(message);
		let response = await this.interface.sendMessages(this.messages);
		this.messages.push(response);
		if (response.hasToolCall()) {
			const data = await response.invokeTool();
			let result = AICodeAssistantMessage.toolResult(data);
			result.toolCall = response.toolCall;
			this.messages.push(result);
			response = await this.interface.sendMessages(this.messages);
			this.messages.push(response);
		}
		return response;
	}

	async requestCodeWith(text) {
		return await this.sendPrompt(
			`${text} ${this.codeResponseFormatSpecification()}`,
			text
		);
	}

	async requestTestsWith(text) {
		return await this.sendPrompt(
			`${text} ${this.testFormatSpecification()}`,
			text
		);
	}

	describeTestError(test) {
		// Not implemented yet
		// try {
		// 	test.runCase();
		// } catch (error) {
		// 	const description = this.describeError(error, true);
		// 	return `Resulted in the following error: ${description}`;
		// }
		return "Does not result in error!";
	}

	describeTestFailure(test) {
		// Not implemented yet
		// const method = test.class().method(test.selector());
		// try {
		// 	test.runCase();
		// } catch (exception) {
		// 	const frame = exception
		// 		.signalContext()
		// 		.stack()
		// 		.find((f) => f.method === method);
		// 	if (!frame) return exception.description();
		// 	const interval = frame.pcRangeContextIsActive(false);
		// 	const assertion = method
		// 		.sourceCode()
		// 		.substring(interval.first, interval.last);
		// 	return `Failed at: ${assertion}`;
		// }
		return "Does not fail!";
	}

	describeError(anError, providingHint) {
		// Not implemented yet
		// if (anError !== MessageNotUnderstood) {
		// 	return anError.description();
		// }
		// let description = `An instance of ${anError
		// 	.receiver()
		// 	.class()
		// 	.name()} did not understand ${anError.message().selector()}`;
		// if (providingHint) {
		// 	description += `. Hint: these are the messages the object understands and that could be used instead: `;
		// 	description += anError.receiver().class().allSelectors().join(", ");
		// }
		// return description;
	}
}

export default AICodeAssistant;
