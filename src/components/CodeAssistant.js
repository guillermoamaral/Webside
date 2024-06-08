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
		this.localContextDescription = "";
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

	clearLocalContext() {
		this.localContext = this.localContextDescription = "";
	}

	useCodeContext(code) {
		this.localContext = "Consider a the follwing piece of code:\n";
		this.localContext += this.encloseCode(code);
		this.localContextDescription = "code";
	}

	useMethodContext(method) {
		this.localContext =
			"Consider a method in a class named " +
			method.methodClass +
			" with selector " +
			method.selector +
			" and with the follwing source code:\n";
		this.localContext += this.encloseCode(method.source);
		this.localContextDescription =
			method.methodClass + ">>#" + method.selector;
	}

	async useClassContext(species, useMethods = false) {
		this.localContext = "";
		const updated = await ide.backend.classNamed(species.name);
		if (updated) {
			this.localContext += "Consider a class named " + species.name;
			if (updated.comment && updated.comment.length > 0)
				this.localContext +=
					' whose comment is the following:\n"' +
					updated.comment +
					'".\n';
		}
		this.localContextDescription = species.name;
		if (useMethods) {
			const methods = await ide.backend.methods(species);
			if (methods.length > 0) {
				this.localContext +=
					"The class defines the following methods\n";
				this.localContext += this.userCodeOpeningTag;
				methods.forEach((m) => {
					this.localContext += "\n\n" + m.source;
				});
				this.localContext += "\n" + this.userCodeClosingTag;
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
		response.parts = this.breakResponse_(response.rawContent);
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

	breakResponse_(text) {
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

	addLocalContext(text) {
		let context =
			"\nIn your response you should enclose any piece of code between " +
			this.aiCodeOpeningTag +
			" and " +
			this.aiCodeClosingTag +
			" tags.\n";
		return this.localContext + context + text;
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
		const raw = this.addLocalContext(
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

	async sendPrompt(prompt, useLocalContext = false) {
		let raw = prompt;
		if (useLocalContext) {
			raw = this.localContext + "\n" + prompt;
		}
		const message = {
			role: "user",
			rawContent: raw,
			parts: [{ type: "text", content: prompt }],
		};
		return await this.sendMessage(message);
	}
}

export default CodeAssistant;
