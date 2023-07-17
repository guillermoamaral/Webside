import OpenAIAPI from "./OpenAIAPI";
import { ide } from "./IDE";

class CodeAssistant {
	constructor() {
		const key = ide.settings.section("openAI").get("apiKey");
		this.openAI = new OpenAIAPI(key);
	}

	async improveCode(code) {
		if (!code || code === "") return "There is nothing to improve";
		const response = await this.openAI.sendMessage(
			"Improve this Smalltalk code " + code
		);
		return this.extractSmalltalkBlocks(response);
	}

	async explainCode(code) {
		if (!code || code === "") return "There is nothing to explain";
		return await this.openAI.sendMessage(
			"Explain this Smalltalk code " + code
		);
	}

	async testCode(code) {
		if (!code || code === "") return "There is nothing to test";
		const response = await this.openAI.sendMessage(
			"Write a unit test for this Smalltalk code " + code
		);
		return this.extractSmalltalkBlocks(response);
	}

	async categorizeMethod(method) {
		const response = await this.openAI.sendMessage(
			"Suggest a category this Smalltalk method " + method.source
		);
		return this.extractSmalltalkBlocks(response);
	}

	extractSmalltalkBlocks(text) {
		const regex = /```smalltalk(.*?)```/gs;
		const matches = [...text.matchAll(regex)];
		const blocks = matches.map((match) => match[1]);
		console.log(text);
		console.log(blocks);
		return blocks.length > 0 ? blocks[0] : text;
	}
}

export default CodeAssistant;
