import AICodeAssistantMessageParser from "./AICodeAssistantMessageParser";

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

	async invokeTool() {
		return await this.toolCall.invoke();
	}

	hasToolCall() {
		return this.toolCall !== null;
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
		this.parts = new AICodeAssistantMessageParser().parse(this.rawContent);
	}
}

export default AICodeAssistantMessage;
