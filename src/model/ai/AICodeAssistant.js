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

	initializeMessages() {
		const message = AICodeAssistantMessage.systemPrompt(this.systemPrompt);
		this.messages = [message];
	}

	defaultInterface() {
		return new OpenAIInterface();
	}

	defaultSystemPrompt() {
		return `You are an expert Smalltalk programmer, integrated in a Smalltalk IDE.
	You have access to several tools that can help you obtain information about the codebase. These tools are precise and return authoritative results. 
	You should invoke them as needed, and trust their results.
	
	Here is what each tool provides:
	
	- search_implementors(selector): returns the methods that implement a given selector (message). Only use this to find methods implementing a selector, not for classes or protocols.
	
	- search_senders(selector): returns the methods that send a given selector (message).
	
	- search_classReferences(classname): returns the methods that reference a given class.
	
	- class_definition(classname): returns the definition of a given class.
	
	- class_protocol(classname): returns the list of selectors (messages) that instances of the class understand and implement. Use this to answer questions like "what messages does <classname> implement?", "what methods does <classname> have?", "what services does <classname> provide?", etc.
	
	- search_subclasses(classname): returns the direct subclasses of a given class.
	
	- evaluate_expression(expression): evaluates a Smalltalk expression and returns the result.
	
	General guidelines:
	
	- Use the tools whenever possible instead of guessing.
	- When a tool returns a result, assume it is correct. Do not reinterpret or modify its meaning. If the result is a list of selectors, present it as <code><method class="ClassName" selector="selectorName"/>...</code>.
	- If the tool result is already formatted as XML or code, preserve that structure and do not add extra explanations unless asked.
	- Never propose classes or selectors unless you have verified their existence using the appropriate tool.
	- Avoid using the words 'Smalltalk' and 'snippet' in your responses.
	
	You are helping the user analyze, explain, and write Smalltalk code interactively in the IDE.`;
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

	codeResponseFormatSpecification() {
		return `You answer should be in correct Smalltalk, without any \`\`\` marks nor the word Smalltalk, in the following format:
<issue>if you think there's a problem, describe it here, otherwise do not include this tag</issue>
<thinking>your rationale and explanation here, be brief</thinking>
${this.codeFormatSpecification()}`;
	}

	codeFormatSpecification() {
		return `Any Smalltalk code you provide should be in correct Smalltalk, without any \`\`\` Markdown mark nor the word Smalltalk,
		and using the following format.
		If you only provide names classes, use this format:
		
		<code>
			<class name="Class name here"/>
			...
			<class name="Class name here"/>
		</code>

		For example:

		<code>
			<class name="Foo"/>
			<class name="Bar"/>
		</code>

		If you provide classes with methods, the code should be organized like this:
		
		<code>
			<class superclass="Superclass name here" name="Class name here" 
			instanceVariables="Instance variables here, space-separated"
			classVariables="Class variables here, space-separated"/>
				<method selector="Method selector here">Method code here; avoid including the class and >> before the selector</method>
				...
				<method selector="Method selector here">Method code here; avoid including the class and >> before the selector</method>				...
			</class>
			<script>Smalltalk code that is neither a class nor a method</script>
		</code>

		For example:

		<code>
			<class superclass="Foo" name="Bar" 
			instanceVariables="a b c"
			classVariables="A B"/>
				<method>bar ^self foo + 1</method>
				...
				<method>foo ^super foo * 2</method>				...
			</class>
		</code>

		If you do not provide classes and only provide a bunch of methods, the code should be organized like this:

		<code>
			<method class="Class name here">
				Method code here; avoid including the class and >> before the selector
			</method>
			...
			<method class="Class name here">
				Method code here; avoid including the class and >> before the selector
			</method>
		</code>

		For example:

		<code>
			<method class="Foo">
				foo
					^self 1 + 2
			</method>
			...
			<method class="Bar">
				bar
					^self foo - 1
			</method>
		</code>		

		If you want to provide just signatures, i.e., class and selector pairs, use this organization instead:

		<code>
			<method class="Class name here" selector="Method selector here"/>
			...
			<method class="Class name here" selector="Method selector here"/>
		</code>
		
		For example:

		<code>
			<method class="Foo" selector="foo"/>
			...
			<method class="Bar" selector="bar"/>
		</code>
		
		Always enclose any code in <code></code> tags. Do not give me code without <code></code>`;
	}

	codeRestrictionsSpecification() {
		return `Unless you have provided a class in the conversation, never propose a class that you are not 100-percent sure it exists in the system. 
		You have tools to check whether a class exist so you can verify a class exist before giving code that references it.
		The same for the selectors. You can check whether a selector has implementors using the corresponding tool and avoid giving me code that has potential flaws.`;
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

	xmlFromClasses(classes) {
		let xml = "<code>";
		classes.forEach(
			(c) =>
				(xml += `\n<class superclass=${c.superclass} name=${c.name}/>`)
		);
		xml += "</code>";
		return xml;
	}

	xmlFromMethods(methods) {
		let xml = "<code>";
		methods.forEach(
			(m) =>
				(xml += `\n<method class=${m.methodClass}>${m.source}</method>`)
		);
		xml += "</code>";
		return xml;
	}

	xmlFromSignaturesOf(methods) {
		let xml = "<code>";
		methods.forEach(
			(m) =>
				(xml += `\n<method class=${m.methodClass} selector=${m.selector}/>`)
		);
		xml += "</code>";
		return xml;
	}

	// Tools

	initializeTools() {
		this.tools = [];
		this.tools.push(this.searchImplementorsTool());
		this.tools.push(this.searchSendersTool());
		this.tools.push(this.searchReferencesTool());
		this.tools.push(this.classDefinitionTool());
		this.tools.push(this.classProtocolTool());
		this.tools.push(this.subclassesTool());
		this.tools.push(this.evaluationTool());
	}

	searchImplementorsTool() {
		const tool = new AIFunction(
			"search_implementors",
			"Retrieves the list of methods that implement a given message (selector). " +
				'Use this ONLY when the user asks "What are the implementors of <selector>?" or "Which methods implement <selector>?", ' +
				"NOT when asking about classes or protocols."
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
			return this.xmlFromSignaturesOf(implementors);
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
			return this.xmlFromSignaturesOf(senders);
		};
		return tool;
	}

	searchReferencesTool() {
		const tool = new AIFunction(
			"search_classReferences",
			'Search references of a given class. Call this whenever you need to know what are the methods referencing a given class, for example, when the user asks "What are the references of <classname>"'
		);
		tool.addStringParameter(
			"classname",
			"The class that the user would like to search references for",
			true
		);
		tool.handler = async (args) => {
			const classname = args.classname;
			if (!classname) return "Class is missing";
			let references = [];
			try {
				references = await this.backend.classReferences(classname);
			} catch (ignored) {}
			if (references.length === 0)
				return `There are no references of ${classname}`;
			return this.xmlFromMethods(references);
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
			"Retrieves the list of messages (selectors) that instances of a given class understand and implement. " +
				'Use this when the user asks "What messages does <classname> implement?", "What messages does <classname> understand?", ' +
				'"What is the protocol of <classname>?", "What methods does <classname> have?", "What services does <classname> provide?" or any similar phrasing.'
		);
		tool.addStringParameter(
			"classname",
			"The name of the class for which you want to retrieve its protocol (understood messages)",
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
			return this.xmlFromSignaturesOf(methods);
		};
		return tool;
	}

	subclassesTool() {
		const tool = new AIFunction(
			"search_subclasses",
			'Search subclasses of a given class. Call this whenever you need to know what are the subclasses of a given class, for example, when the user asks "What are the subclasses of <classname>"'
		);
		tool.addStringParameter(
			"classname",
			"The class that the user would like to know its subclasses",
			true
		);
		tool.handler = async (args) => {
			const classname = args.classname;
			if (!classname) return "Class is missing";
			let subclasses = [];
			try {
				subclasses = await this.backend.subclasses(classname);
			} catch (ignored) {}
			if (subclasses.length === 0)
				return `There are no subclasses of ${classname}`;
			return this.xmlFromClasses(subclasses);
		};
		return tool;
	}

	evaluationTool() {
		const tool = new AIFunction(
			"evaluate_expression",
			'Evaluates a Smalltalk expression. Call this whenever you need to evaluate an expression, for example, when the user asks "What is the result of 3 + 4, 123 factorial, Process allInstances size < 10 ifTrue: [Processor yield]"'
		);
		tool.addStringParameter(
			"expression",
			"The expression to be evaluated",
			true
		);
		tool.handler = async (args) => {
			const expression = args.expression;
			if (!expression) return "Expression is missing";
			let result;
			try {
				result = await this.backend.evaluateExpression(
					expression,
					true,
					true
				);
				result = result.printString;
			} catch (error) {
				result = error?.error?.description || error.toString();
			}
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
			`${text}\n${this.codeFormatSpecification()}\n${this.codeRestrictionsSpecification()}`,
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
			console.log("Tool invokation result");
			console.log(data);
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
