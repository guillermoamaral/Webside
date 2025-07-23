import { SmalltalkLexer } from "./SmalltalkLexer";

export const tokenTypes = [
	"selector",
	"symbol",
	"argument",
	"temporary",
	"assignment",
	"string",
	"variable",
	"meta",
	"bracket",
	"reserved",
	"return",
	"global",
	"number",
	"comment",
	"separator",
];

export function tokenize(source, inSelector = false) {
	const lexer = SmalltalkLexer(inSelector);
	const state = lexer.startState();
	const lines = source.split(/\n/);
	const tokens = [];
	let offset = 0;
	for (let i = 0; i < lines.length; i++) {
		const line = lines[i];
		const stream = new StringStream(line);
		while (!stream.eol()) {
			const start = stream.pos;
			const type = lexer.token(stream, state);
			const end = stream.pos;
			if (type) {
				tokens.push({
					start: offset + start,
					end: offset + end,
					type,
					value: stream.current(),
				});
			}
			stream.start = stream.pos;
		}
		offset += line.length + 1;
	}
	return tokens;
}

class StringStream {
	constructor(string) {
		this.string = string;
		this.pos = 0;
		this.start = 0;
	}

	eol() {
		return this.pos >= this.string.length;
	}

	next() {
		return this.string.charAt(this.pos++);
	}

	peek() {
		return this.string.charAt(this.pos);
	}

	eat(match) {
		const ch = this.peek();
		if (typeof match === "string") {
			if (ch === match) {
				this.pos++;
				return ch;
			}
		} else if (match.test(ch)) {
			this.pos++;
			return ch;
		}
		return undefined;
	}

	eatWhile(match) {
		let start = this.pos;
		while (!this.eol() && this.eat(match));
		return this.pos > start;
	}

	current() {
		return this.string.slice(this.start, this.pos);
	}

	indentation() {
		const match = this.string.match(/^\s*/);
		return match ? match[0].length : 0;
	}

	eatSpace() {
		let start = this.pos;
		while (!this.eol() && /\s/.test(this.peek())) this.pos++;
		return this.pos > start;
	}
}
