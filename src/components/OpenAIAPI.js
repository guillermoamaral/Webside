import axios from "axios";

const FakeAPI = false;

class OpenAIAPI {
	constructor(apiKey) {
		this.apiKey = apiKey;
	}

	async sendMessage(text) {
		if (FakeAPI) {
			return "blah blah bla";
		}
		try {
			const headers = {
				Authorization: `Bearer ${this.apiKey}`,
			};
			const body = {
				model: "gpt-3.5-turbo",
				messages: [{ role: "user", content: text }],
				temperature: 1,
				max_tokens: 3500,
			};
			const response = await axios.post(
				"https://api.openai.com/v1/chat/completions",
				body,
				{ headers: headers }
			);
			return response.data.choices[0].message.content;
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
