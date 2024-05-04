import { ide } from "./IDE";

class CodeAssistant {
	constructor(api) {
		this.api = api;
		this.aiCodeOpeningTag = "<assistantcode>";
		this.aiCodeClosingTag = "</assistantcode>";
		this.userCodeOpeningTag = "<usercode>";
		this.userCodeClosingTag = "</usercode>";
		this.localContext = "";
		this.messages = [];
		this.includeHistory = true;
		this.setupGeneralContext();
	}

	deleteMessageHistory() {
		this.messages = [this.messages[0]];
	}

	setupGeneralContext() {
		let context = "We are in the context of Smalltalk.\n";
		const dialect = ide.currentDialect();
		if (dialect) context += "Specifically, " + dialect + " Smalltalk.\n";
		context +=
			"Thus, when I ask for help to analyze, explain or write code, you will reply as experimented Smalltalk programmer.\n";
		context += "In your response:\n";
		context +=
			"1. You will not include the words 'Smalltalk' and 'snippet'.\n";
		context +=
			"2. You will enclose any piece of Smalltalk code between " +
			this.aiCodeOpeningTag +
			" and " +
			this.aiCodeClosingTag +
			" tags.\r";
		const message = {
			role: "system",
			rawContent: context,
		};
		this.messages.push(message);
	}

	clearLocalContext() {
		this.localContext = "";
	}

	async useClassContext(classname, useMethods = false) {
		this.localContext = "";
		const species = await ide.backend.classNamed(classname);
		if (species) {
			this.localContext += "We have a class named " + classname;
			if (species.comment && species.comment.length > 0)
				this.localContext +=
					' whose comment is the following:\n"' +
					species.comment +
					'".\n';
		}
		if (useMethods) {
			const methods = await ide.backend.methods(classname);
			if (methods.length > 0) {
				this.localContext +=
					"The class defines the following methods. ";
				methods.forEach((m) => {
					this.localContext += "\n\n" + m.source;
				});
			}
		}
	}

	async sendMessage(message) {
		const response = {
			role: "assistant",
			parts: [{ type: "text", content: "..." }],
		};
		this.messages.push(message);
		const conversation = this.includeHistory
			? [...this.messages]
			: [message];
		this.messages.push(response);
		response.rawContent = await this.sendMessages(conversation);
		response.parts = this.breakResponse(response.rawContent);
		return response;
	}

	async sendMessages(messages) {
		const conversation = messages.map((m) => {
			return { role: m.role, content: m.rawContent };
		});
		return await this.api.sendMessages(conversation);
	}

	breakResponse(text) {
		const parts = [];
		text.replaceAll("\n", "\r")
			.split(this.aiCodeOpeningTag)
			.forEach((p) => {
				const i = p.indexOf(this.aiCodeClosingTag);
				if (i > 0) {
					parts.push({ type: "code", content: p.substring(0, i) });
				}
				const offset = i > 0 ? this.aiCodeClosingTag.length : 1;
				const text = p.substring(i + offset, p.length);
				if (text.length > 0)
					parts.push({ type: "text", content: text });
			});
		return parts;
	}

	decorateContent(text) {
		return this.localContext + text;
	}

	encloseCode(code) {
		return this.userCodeOpeningTag + code + this.userCodeClosingTag;
	}

	async sendCodePrompt(prompt, code) {
		if (!code || code === "")
			return {
				role: "assistant",
				parts: [{ type: "text", content: "No code provided" }],
			};
		const raw = this.decorateContent(
			prompt + "\n" + this.encloseCode(code)
		);
		const message = {
			role: "user",
			rawContent: raw,
			parts: [
				{ type: "text", content: prompt },
				{ type: "code", content: code },
			],
		};
		return await this.sendMessage(message);
	}

	// Services...
	async explainCode(code) {
		return await this.sendCodePrompt(
			"Explain the following code, without giving me back the code, only the plain explanation, without using more than 200 characters:",
			code
		);
	}

	async testCode(code) {
		return await this.sendCodePrompt(
			"Write a unit test for the following code:",
			code
		);
	}

	async improveCode(code) {
		return await this.sendCodePrompt("Improve the following code:", code);
	}

	async categorizeMethod(method) {
		return await this.sendPrompt(
			"Suggest a category for the following method: " + method.source
		);
	}

	async sendPrompt(prompt) {
		const message = {
			role: "user",
			rawContent: prompt,
			parts: [{ type: "text", content: prompt }],
		};
		return await this.sendMessage(message);
	}
}

export default CodeAssistant;
