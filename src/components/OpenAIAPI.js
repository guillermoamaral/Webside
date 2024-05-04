import axios from "axios";

const FakeAPI = false;

class OpenAIAPI {
	constructor(apiKey, model) {
		this.url = "https://api.openai.com/v1";
		this.apiKey = apiKey;
		this.model = model || "gpt-3.5-turbo";
		this.maxTokens = 3500;
		this.temperature = 0;
	}

	async get(uri) {
		const headers = {
			Authorization: `Bearer ${this.apiKey}`,
		};
		try {
			const response = await axios.get(this.url + uri, {
				headers: headers,
			});
			return response.data;
		} catch (error) {
			this.handleError(error);
		}
	}

	async post(uri, payload) {
		const headers = {
			Authorization: `Bearer ${this.apiKey}`,
		};
		try {
			const response = await axios.post(this.url + uri, payload, {
				headers: headers,
			});
			return response.data;
		} catch (error) {
			this.handleError(error);
		}
	}

	async getModels() {
		const data = await this.get("/models");
		return data.data.map((m) => m.id);
	}

	async sendSystemMessage(text) {
		return await this.sendMessage(text, "system");
	}

	async sendMessage(text, role = "user") {
		return await this.sendMessages([{ role: role, content: text }]);
	}

	async sendMessages(messages) {
		if (FakeAPI) {
			return "This is a fake answer for testing purposes";
		}
		try {
			const body = {
				model: this.model,
				messages: messages,
				temperature: this.temperature,
				max_tokens: this.maxTokens,
			};
			const data = await this.post("/chat/completions", body);
			return data.choices[0].message.content;
		} catch (error) {
			return this.handleError(error);
		}
	}

	handleError(error) {
		console.log(error);
		return "Cannot communicate with OpenAI";
	}
}

export default OpenAIAPI;
