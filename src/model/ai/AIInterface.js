import axios from "axios";
import AICodeAssistantMessage from "./AICodeAssistantMessage";
import { AIToolCall } from "./AIInterfaceTool";

class AIInterface {
	constructor() {
		this.maxTokens = 3500;
		this.temperature = 0;
		this.model = this.defaultModel();
		this.tools = [];
		this.key = "";
	}

	static availableTypes() {
		return [OpenAIInterface, GroqInterface, MistralInterface];
	}

	static newNamed(name) {
		const type = this.availableTypes().find(
			(t) => t.displayName() === name
		);
		if (type) return new type();
		throw new Error("Invalid interface type");
	}

	name() {
		return this.constructor.name.replace("Interface", "");
	}

	url() {
		return this.constructor.url();
	}

	addTool(tool) {
		this.tools.push(tool);
	}

	removeTools() {
		this.tools = [];
	}

	sendPrompt(text) {
		return this.sendPromptAs(text, "user");
	}

	availableModels() {
		return [this.defaultModel()];
	}

	async sendMessages(messages) {
		const body = {};
		body.model = this.model;
		body.messages = messages.map((msg) => msg.asJson());
		body.temperature = this.temperature;
		body.max_tokens = this.maxTokens;
		if (this.tools.length > 0) {
			body.tools = this.tools.map((tool) => tool.asJson());
		}
		let answer;
		try {
			answer = await this.post("/chat/completions", body);
		} catch (error) {
			let description = error.message;
			if (
				error.response &&
				error.response.data &&
				error.response.data.error &&
				error.response.data.error.message
			)
				description = error.response.data.error.message;
			return AICodeAssistantMessage.assistantResponse(
				`There was an error: ${description}`
			);
		}
		const message = answer.choices[0].message;
		const response = AICodeAssistantMessage.assistantResponse(
			message.content
		);
		if (answer.choices[0].finish_reason === "tool_calls") {
			const name = message.tool_calls[0].function.name;
			const tool = this.tools.find((t) => t.name === name);
			if (!tool) {
				throw new Error("tool not found");
			}
			const args = JSON.parse(message.tool_calls[0].function.arguments);
			const call = new AIToolCall(message.tool_calls[0].id, tool, args);
			response.toolCall = call;
		}
		return response;
	}

	async getModels() {
		let models = [];
		try {
			const response = await this.get("/models");
			models = response.data;
		} catch (error) {
			console.log(error);
		}
		return models;
	}

	async sendPromptAs(text, role) {
		const message = {};
		message.role = role || "user";
		message.content = text;
		return await this.sendMessages([message]);
	}

	async get(uri) {
		const headers = {
			Authorization: `Bearer ${this.key}`,
		};
		const response = await axios.get(this.url() + uri, {
			headers: headers,
		});
		return response.data;
	}

	async post(uri, payload) {
		const headers = {
			Authorization: `Bearer ${this.key}`,
		};
		const response = await axios.post(this.url() + uri, payload, {
			headers: headers,
		});
		return response.data;
	}

	async modelWithId(id) {
		return await this.get(`/models/${id}`);
	}
}

class GroqInterface extends AIInterface {
	static url() {
		return "https://api.groq.com/openai/v1";
	}

	defaultModel() {
		return "llama-3.3-70b-versatile";
	}

	static displayName() {
		return "Groq";
	}
}

class MistralInterface extends AIInterface {
	static url() {
		return "https://api.mistral.ai/v1";
	}

	defaultModel() {
		return "pixtral-12b-latest";
	}

	static displayName() {
		return "Mistral";
	}
}

class OpenAIInterface extends AIInterface {
	static url() {
		return "https://api.openai.com/v1";
	}

	defaultModel() {
		return "gpt-4o"; //"chatgpt-4o-latest";
	}

	static displayName() {
		return "OpenAI";
	}
}

export { AIInterface, GroqInterface, MistralInterface, OpenAIInterface };
