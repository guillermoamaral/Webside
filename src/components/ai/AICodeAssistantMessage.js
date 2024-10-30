import AICodeGenerationResult from "./AICodeGenerationResult";
import AISuggestedClass from "./AISuggestedClass";
import AISuggestedMethod from "./AISuggestedMethod";
import AISuggestedScript from "./AISuggestedScript";

class AICodeAssistantMessage {
	constructor(role = "user", content = "", visibleContent) {
		this.role = role;
		this.rawContent = content;
		this.toolCall = null;
		this.tags = [];
		this.code = null;
		this.visibleContent = visibleContent;
		this.initializeParts();
	}

	static toolResult(content) {
		return new AICodeAssistantMessage("tool", content);
	}

	static systemPrompt(content) {
		return new AICodeAssistantMessage("system", content);
	}

	static assistantResponse(content) {
		return new AICodeAssistantMessage("assistant", content);
	}

	static userPrompt(content, visibleContent) {
		return new AICodeAssistantMessage("user", content, visibleContent);
	}

	asJson() {
		let json = { role: this.role, content: this.rawContent };
		if (this.toolCall !== null) {
			if (this.role === "tool") {
				json.tool_call_id = this.toolCall.id;
			} else {
				json.tool_calls = [this.toolCall.asJson()];
			}
		}
		return json;
	}

	getContent() {
		if (this.rawContent === null && this.toolCall !== null) {
			return "Tool call: " + this.toolCall.tool.name;
		}
		return this.rawContent;
	}

	invokeTool() {
		return this.toolCall.invoke();
	}

	hasToolCall() {
		return this.toolCall !== null;
	}

	isSystemMessage() {
		return this.role === "system";
	}

	extractMethodFrom(xmlNode) {
		return new AISuggestedMethod(null, xmlNode.textContent.trim());
	}

	extractScriptFrom(xmlNode) {
		console.log("script!", xmlNode.textContent.trim());
		return new AISuggestedScript(xmlNode.textContent.trim());
	}

	extractClassFrom(xmlNode) {
		const name = xmlNode.querySelector("name")?.textContent || "";
		const species = new AISuggestedClass(name);
		const superclassName =
			xmlNode.querySelector("superclass")?.textContent || "";
		if (superclassName) species.superclassName = superclassName;
		const instVarNames =
			xmlNode.querySelector("instanceVariables")?.textContent || "";
		if (instVarNames)
			species.instanceVariableNames = instVarNames.split(" ");
		const classVarNames =
			xmlNode.querySelector("classVariables")?.textContent || "";
		if (classVarNames)
			species.classVariableNames = classVarNames.split(" ");
		xmlNode.childNodes.forEach((child) => {
			if (
				child.nodeType === 1 &&
				child.tagName === "method" &&
				child.textContent
			) {
				species.addMethod(this.extractMethodFrom(child));
			}
		});
		return species;
	}

	extractCodeFrom(xmlNode) {
		const code = new AICodeGenerationResult();
		xmlNode.querySelector("code").childNodes.forEach((child) => {
			if (child.nodeType === 1) {
				if (child.tagName === "class")
					code.addClass(this.extractClassFrom(child));
				if (child.tagName === "method")
					code.addMethod(this.extractMethodFrom(child));
				if (child.tagName === "script")
					code.addScript(this.extractScriptFrom(child));
			}
		});
		return code;
	}

	initializeParts() {
		this.parts = [];
		if (this.visibleContent) {
			this.parts.push({
				type: "text",
				content: this.visibleContent,
			});
			return;
		}
		const regex = /<(code|thinking|issue)>([\s\S]*?)<\/\1>/g;
		let lastIndex = 0;
		let match, part;
		while ((match = regex.exec(this.rawContent)) !== null) {
			if (lastIndex < match.index) {
				part = {
					type: "text",
					content: this.rawContent
						.slice(lastIndex, match.index)
						.trim(),
				};
				this.parts.push(part);
			}
			const type = match[1];
			let content = match[2].trim();
			if (content.length > 0) {
				part = {
					type: "text",
					content: `**${type.charAt(0).toUpperCase()}${type.slice(
						1
					)}**. ${content}`,
				};
				if (type === "code") {
					const parser = new DOMParser();
					const xmlNode = parser.parseFromString(
						`<code>${content}</code>`,
						"text/xml"
					);
					const code = this.extractCodeFrom(xmlNode);
					part.type = "code";
					part.code = code;
					part.content = code.codeChunk();
					console.log(code);
				}
				if (
					part.type === "text" &&
					this.parts.length > 0 &&
					this.parts[this.parts.length - 1].type === "text"
				) {
					this.parts[
						this.parts.length - 1
					].content += `${part.content}`;
				} else {
					this.parts.push(part);
				}
			}
			lastIndex = regex.lastIndex;
		}
		if (lastIndex < this.rawContent.length) {
			part = {
				type: "text",
				content: this.rawContent.slice(lastIndex).trim(),
			};
			this.parts.push(part);
		}
	}
}

export default AICodeAssistantMessage;
