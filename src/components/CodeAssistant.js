import { ide } from "./IDE";

class CodeAssistant {
	constructor(api, context) {
		this.api = api;
		this.aiCodeOpeningTag = "<assistantcode>";
		this.aiCodeClosingTag = "</assistantcode>";
		this.userCodeOpeningTag = "<usercode>";
		this.userCodeClosingTag = "</usercode>";
		this.includeHistory = true;
		this.messages = [];
		this.setupGeneralContext(context);
		this.localContext = "";
	}

	deleteMessageHistory() {
		this.messages = [this.messages[0]];
	}

	setupGeneralContext(text) {
		let context = text;
		// context +=
		// 	"\nIn your response you must enclose any piece of code you provide between " +
		// 	this.aiCodeOpeningTag +
		// 	" and " +
		// 	this.aiCodeClosingTag +
		// 	" tags.\n";
		const message = {
			role: "system",
			rawContent: context,
		};
		this.messages.push(message);
	}

	encloseCode(code) {
		return this.userCodeOpeningTag + code + this.userCodeClosingTag;
	}

	clearLocalContext() {
		this.localContext = "";
	}

	useCodeContext(text) {
		this.clearLocalContext();
		this.addLocalContext(text);
	}

	addLocalContext(text) {
		this.localContext += "\n" + text;
	}

	addCodeContext(code) {
		this.addLocalContext(
			"Consider a the follwing code:\n" + this.encloseCode(code)
		);
	}

	async addMethodContext(classname, selector) {
		let method;
		try {
			method = await ide.backend.method(classname, selector);
		} catch (ignored) {}
		if (!method) return;
		const context =
			"Consider a method in a class " +
			method.methodClass +
			" with selector #" +
			method.selector +
			" and with the follwing code:\n" +
			this.encloseCode(method.source);
		this.addLocalContext(context);
	}

	async addClassContext(classname, includeMethods = false) {
		let context = "";
		let species;
		try {
			species = await ide.backend.classNamed(classname);
		} catch (ignored) {}
		if (!species) return;
		context += "Consider a class named " + classname;
		if (species.comment && species.comment.length > 0)
			context += ' with this comment:\n"' + species.comment + '".\n';
		this.addLocalContext(context);
		if (includeMethods) {
			context = "";
			let methods;
			try {
				methods = await ide.backend.methods(classname);
			} catch (ignored) {}
			if (!methods || methods.length === 0) return;
			context += "This class defines the following methods\n";
			context += this.userCodeOpeningTag;
			methods.forEach((m) => {
				context += "\n\n" + m.source;
			});
			context += "\n" + this.userCodeClosingTag;
			this.addLocalContext(context);
		}
	}

	async addContextsFrom(text) {
		const pattern = /@([a-zA-Z0-9]+)(#[a-zA-Z0-9:]+)?/g;
		let match;
		while ((match = pattern.exec(text)) !== null) {
			const classname = match[1];
			const selector = match[2]
				? match[2].substring(1, match[2].length)
				: null;
			if (selector) {
				await this.addMethodContext(classname, selector);
			} else {
				await this.addClassContext(classname);
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
		const pattern =
			/^([A-Za-z \t]*)```([A-Za-z]*)?\n([\s\S]*?)```([A-Za-z \t]*)*$/gm;
		let parts = [];
		let matches;
		let i = 0;
		while ((matches = pattern.exec(text)) !== null) {
			if (matches.index === pattern.lastIndex) {
				pattern.lastIndex++;
			}
			if (i < matches.index) {
				parts.push({
					type: "text",
					content: text.substring(i, matches.index),
				});
			}
			parts.push({
				type: "code",
				content: matches[3],
				syntax: matches[2],
			});
			i = matches.index + matches[0].length;
		}
		if (i < text.length) {
			parts.push({
				type: "text",
				content: text.substring(i, text.length),
			});
		}
		return parts;
	}

	async sendCodePrompt(prompt, code) {
		if (!code || code === "")
			return {
				role: "assistant",
				parts: [{ type: "text", content: "No code provided" }],
			};
		const message = {
			role: "user",
			rawContent: prompt + "\n" + this.encloseCode(code),
			parts: [
				{ type: "text", content: prompt },
				{ type: "code", content: code },
			],
		};
		return await this.sendMessage(message);
	}

	// Services...
	async explainCode(code) {
		this.useCodeContext(code);
		return await this.sendPrompt(
			"Explain the given code, without giving me back the code, only the plain explanation, without using more than 200 characters.",
			true
		);
	}

	async testCode(code) {
		this.useCodeContext(code);
		return await this.sendPrompt(
			"Write a unit test for the given code.",
			true
		);
	}

	async improveCode(code) {
		this.useCodeContext(code);
		return await this.sendPrompt("Improve the given code.", true);
	}

	async categorizeMethod(method) {
		return await this.sendPrompt(
			"Suggest a category for the following method: " + method.source
		);
	}

	async sendPrompt(prompt) {
		let raw = prompt;
		await this.addContextsFrom(prompt);
		raw = this.localContext + "\n" + prompt;
		const message = {
			role: "user",
			rawContent: raw,
			parts: [{ type: "text", content: prompt }],
		};
		return await this.sendMessage(message);
	}
}

export default CodeAssistant;
