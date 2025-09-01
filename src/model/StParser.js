// The goal of this class implements is to provide some services for parsing Smalltalk code.
// However, it is by no means a full Smalltalk parser and does should be covered by the corresponding
// endpoints (i.e., supplied by the Smalltalk system at hand).
// It might be used as a fallback in case these services fail, take too much time or for the sake of
// avoiding too many requests to the backend.

const binarySelectors = [
	"+",
	"-",
	"*",
	"/",
	"\\",
	"~",
	"<",
	">",
	"<=",
	">=",
	"=",
	"~=",
	"==",
	"~~",
	"@",
	"->",
	"&",
	"|",
	",",
	"<<",
	">>",
];

class StParser extends Object {
	static methodBodyIndex(selector, source) {
		let index;
		if (selector.includes(":")) {
			const keywords = selector
				.split(":")
				.filter(Boolean)
				.map((k) => k + ":");
			for (const keyword of keywords) {
				const found = source.indexOf(keyword, index);
				if (found === -1) {
					throw new Error(`Keyword ${keyword} not found in source`);
				}
				index = found + keyword.length;
			}
			while (index < source.length && /\s/.test(source[index])) index++;
			while (index < source.length && /\S/.test(source[index])) index++;
			while (
				index < source.length &&
				(source[index] === "\r" || source[index] === "\n")
			)
				index++;
			return index;
		}

		if (binarySelectors.includes(selector)) {
			index = source.indexOf(selector);
			if (index === -1) {
				throw new Error(
					`Binary selector ${selector} not found in source`
				);
			}
			index = index + selector.length;
			while (index < source.length && /\s/.test(source[index])) index++;
			while (index < source.length && /\S/.test(source[index])) index++;
			while (
				index < source.length &&
				(source[index] === "\r" || source[index] === "\n")
			)
				index++;
			return index;
		}

		// Unary
		index = source.indexOf(selector);
		if (index === -1) {
			throw new Error(`Unary selector ${selector} not found in source`);
		}
		return index + selector.length;
	}

	static methodTemporaries(source) {
		const match = source.match(/\|\s*([^\|]+?)\s*\|/);
		if (!match) return [];
		return match[1].trim().split(/\s+/);
	}

	static methodArguments(source) {
		const args = [];
		const regex = /\b([a-zA-Z0-9_]+):[\s\r\n]*([a-zA-Z_][a-zA-Z0-9_]*)/g;
		let lastIndex = 0;

		while (true) {
			const match = regex.exec(source);
			if (!match) break;

			const between = source.slice(lastIndex, match.index);
			if (/[^\s\r\n]/.test(between)) break;

			args.push(match[2]);
			lastIndex = regex.lastIndex;
		}

		if (args.length > 0) return args;

		for (const selector of binarySelectors) {
			const pattern = new RegExp(
				`^\\s*${selector.replace(
					/[-/\\^$*+?.()|[\]{}]/g,
					"\\$&"
				)}\\s*([a-zA-Z_][a-zA-Z0-9_]*)`
			);
			const match = source.match(pattern);
			if (match) return [match[1]];
		}

		// Unary
		return [];
	}
}

export default StParser;
