// AICodeAssistantMessageParser.js
import AICodeGenerationResult from "./AICodeGenerationResult";
import AISuggestedClass from "./AISuggestedClass";
import AISuggestedMethod from "./AISuggestedMethod";
import AISuggestedScript from "./AISuggestedScript";

class AICodeAssistantMessageParser {
	parse(rawContent) {
		const parts = [];
		if (!rawContent || rawContent.trim() === "") return parts;
		let raw = rawContent
			.replace(/```[a-zA-Z]*\n?/g, "")
			.replace(/```/g, "")
			.trim();
		const regex = /<(code|thinking|issue)>([\s\S]*?)<\/\1>/g;
		let lastIndex = 0;
		let match;
		while ((match = regex.exec(raw)) !== null) {
			if (lastIndex < match.index) {
				parts.push({
					type: "text",
					content: raw.slice(lastIndex, match.index).trim(),
				});
			}
			const type = match[1];
			const content = match[2].trim();
			if (type === "code") {
				const code = this.extractCodeFrom(content);
				parts.push({
					type: "code",
					code: code,
					content: code.codeChunk(),
				});
			} else {
				parts.push({
					type: "text",
					content: `**${type.charAt(0).toUpperCase()}${type.slice(
						1
					)}**. ${content}`,
				});
			}
			lastIndex = regex.lastIndex;
		}
		if (lastIndex < raw.length) {
			parts.push({
				type: "text",
				content: raw.slice(lastIndex).trim(),
			});
		}
		this.extractLooseCode(parts);
		this.joinContiguousClassNames(parts);
		this.joinContiguousSelectors(parts);
		return parts;
	}

	extractCodeFrom(content) {
		return this.parseCodeBlock("code", "", content);
	}

	extractClassFrom(xmlNode) {
		const name = xmlNode.getAttribute("name") || "";
		const species = new AISuggestedClass(name);
		const superclassName = xmlNode.getAttribute("superclass");
		if (superclassName) species.superclassName = superclassName;
		const instVarNames = xmlNode.getAttribute("instanceVariables");
		if (instVarNames)
			species.instanceVariableNames = instVarNames.split(" ");
		const classVarNames = xmlNode.getAttribute("classVariables");
		if (classVarNames)
			species.classVariableNames = classVarNames.split(" ");
		xmlNode.childNodes.forEach((child) => {
			if (
				child.nodeType === 1 &&
				child.tagName === "method" &&
				child.textContent
			) {
				species.addMethod(this.extractMethodFrom(child, species));
			}
		});
		return species;
	}

	extractMethodFrom(xmlNode, suggestedClass) {
		const classname = xmlNode.getAttribute("class");
		const selector = xmlNode.getAttribute("selector");
		const sourceCode = xmlNode.textContent || "";
		const species = suggestedClass
			? suggestedClass
			: classname
			? new AISuggestedClass(classname)
			: null;
		return new AISuggestedMethod(species, selector, sourceCode);
	}

	extractScriptFrom(xmlNode) {
		return new AISuggestedScript(xmlNode.textContent.trim());
	}

	parseCodeBlock(tag, attributes, content) {
		const code = new AICodeGenerationResult();
		const parser = new DOMParser();
		const xmlDoc = parser.parseFromString(
			`<${tag}${attributes ? " " + attributes : ""}>${content}</${tag}>`,
			"text/xml"
		);
		const xmlNode = xmlDoc.querySelector(tag);
		if (!xmlNode) return code;
		if (tag === "code") {
			xmlNode.childNodes.forEach((child) => {
				if (child.nodeType === 1) {
					if (child.tagName === "class")
						code.addClass(this.extractClassFrom(child));
					if (child.tagName === "method")
						code.addMethod(this.extractMethodFrom(child));
					if (child.tagName === "script")
						code.addScript(this.extractScriptFrom(child));
				}
			});
		} else if (tag === "class") {
			code.addClass(this.extractClassFrom(xmlNode));
		} else if (tag === "method") {
			code.addMethod(this.extractMethodFrom(xmlNode));
		} else if (tag === "script") {
			code.addScript(this.extractScriptFrom(xmlNode));
		}

		if (tag === "code" && code.isEmpty() && content.trim() !== "") {
			code.addScript(new AISuggestedScript(content.trim()));
		}
		return code;
	}

	extractLooseCode(parts) {
		const selfClosingNodeRegex = /<(class|method|script)([^>]*)\/>/g;
		const nodeRegex = /<(class|method|script)([^>]*)>([\s\S]*?)<\/\1>/g;
		const remainingTextParts = parts.filter((p) => p.type === "text");
		remainingTextParts.forEach((part) => {
			const newParts = [];
			let lastIndex = 0;
			let match;
			while ((match = selfClosingNodeRegex.exec(part.content)) !== null) {
				if (lastIndex < match.index) {
					const content = part.content
						.slice(lastIndex, match.index)
						.trim();
					if (content !== "") {
						newParts.push({
							type: "text",
							content: content,
						});
					}
				}
				const tag = match[1];
				const attributes = match[2];
				const innerContent = "";
				const code = this.parseCodeBlock(tag, attributes, innerContent);
				newParts.push({
					type: "code",
					code: code,
					content: code.codeChunk(),
				});
				lastIndex = selfClosingNodeRegex.lastIndex;
			}
			nodeRegex.lastIndex = lastIndex;
			while ((match = nodeRegex.exec(part.content)) !== null) {
				const matchIndex = match.index;
				if (lastIndex < matchIndex) {
					const content = part.content
						.slice(lastIndex, matchIndex)
						.trim();
					newParts.push({
						type: "text",
						content: content,
					});
				}
				const tag = match[1];
				const attributes = match[2];
				const innerContent = match[3].trim();
				const code = this.parseCodeBlock(tag, attributes, innerContent);
				newParts.push({
					type: "code",
					code: code,
					content: code.codeChunk(),
				});
				lastIndex = matchIndex + match[0].length;
			}
			if (lastIndex < part.content.length) {
				newParts.push({
					type: "text",
					content: part.content.slice(lastIndex).trim(),
				});
			}
			const index = parts.indexOf(part);
			parts.splice(index, 1, ...newParts);
		});
	}

	joinContiguousClassNames(parts) {
		const newParts = [];
		let current = null;
		parts.forEach((part) => {
			if (
				part.type === "code" &&
				part.code &&
				part.code.hasOnlyClassNames()
			) {
				if (current === null) {
					current = {
						type: "code",
						code: part.code,
						content: part.content,
					};
					newParts.push(current);
				} else {
					part.code.classes.forEach((c) => current.code.addClass(c));
				}
			} else {
				current = null;
				newParts.push(part);
			}
		});
		parts.length = 0;
		parts.push(...newParts);
	}

	joinContiguousSelectors(parts) {
		const newParts = [];
		let current = null;
		parts.forEach((part) => {
			if (
				part.type === "code" &&
				part.code &&
				part.code.hasOnlySignatures()
			) {
				if (current === null) {
					current = {
						type: "code",
						code: part.code,
						content: part.content,
					};
					newParts.push(current);
				} else {
					part.code.methods.forEach((m) => current.code.addMethod(m));
				}
			} else {
				current = null;
				newParts.push(part);
			}
		});
		parts.length = 0;
		parts.push(...newParts);
	}
}

export default AICodeAssistantMessageParser;
