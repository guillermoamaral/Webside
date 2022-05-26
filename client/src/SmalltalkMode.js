// Based in smalltalk.js from CodeMirror, copyright (c) by Marijn Haverbeke and others
// Distributed under an MIT license: https://codemirror.net/LICENSE

import CodeMirror from "codemirror";

CodeMirror.defineMode("smalltalk-method", function (config) {
	var binary = /[+\-/\\*~<>=@%|&?!,]/;
	var reserved = /true|false|nil|self|super|thisContext/;

	var Context = function (tokenizer, parent) {
		this.next = tokenizer;
		this.parent = parent;
	};

	var Token = function (name, context) {
		this.name = name;
		this.context = context;
	};

	var State = function () {
		this.context = new Context(next, null);
		this.beginning = true;
		this.expected = ["selector"];
		this.arguments = [];
		this.temporaries = [];
		this.indentation = 0;
		this.userIndentationDelta = 0;
	};

	State.prototype.userIndent = function (indentation) {
		this.userIndentationDelta =
			indentation > 0 ? indentation / config.indentUnit - this.indentation : 0;
	};

	var next = function (stream, context, state) {
		var token = new Token(null, context);
		var char = stream.next();
		if (char === '"') {
			token = nextComment(stream, new Context(nextComment, context));
			state.expected = ["variable"];
			state.beginning = false;
		} else if (char === "'") {
			token = nextString(stream, new Context(nextString, context));
			state.expected = ["selector"];
			state.beginning = false;
		} else if (char === "#") {
			if (stream.peek() === "'") {
				stream.next();
				token = nextSymbol(stream, new Context(nextSymbol, context));
			} else {
				if (stream.eatWhile(/[^\s.{}[\]()]/)) {
					token.name = "symbol";
				} else {
					token.name = "meta";
				}
				state.expected = ["selector"];
				state.beginning = false;
			}
		} else if (char === "<" && !state.beginning && false) {
			stream.eatWhile(/[^\s>]/);
			stream.next();
			token.name = "pragma";
			state.expected = ["variable"];
			state.beginning = false;
		} else if (char === "$") {
			stream.eatWhile(/[^\s>]/);
			stream.next();
			token.name = "string";
			state.expected = ["selector"];
			state.beginning = false;
		} else if (char === "|" && state.expected.includes("variable")) {
			token.context = new Context(nextTemporaries, context, state);
			state.expected = ["variable"];
			state.beginning = false;
		} else if (/[[\]{}()]/.test(char)) {
			token.name = "bracket";
			state.beginning = false;
			state.expected = /[[{(]/.test(char) ? ["variable"] : ["selector"];
			if (char === "[") {
				state.indentation++;
			} else if (char === "]") {
				state.indentation = Math.max(0, state.indentation - 1);
			}
		} else if (char === "." || char === ";") {
			token.name = "separator";
			state.expected = char === "." ? ["variable"] : ["selector"];
		} else if (char === "^") {
			token.name = "return";
			state.expected = ["variable"];
			state.beginning = false;
		} else if (char === ":") {
			if (stream.peek() === "=") {
				stream.next();
				token.name = "assignment";
				state.expected = ["variable"];
			}
		} else if (/\d/.test(char)) {
			stream.eatWhile(/[\w\d]/);
			token.name = "numeric";
			state.expected = ["selector"];
		} else if (binary.test(char)) {
			stream.eatWhile(binary);
			token.name = "selector";
			state.expected = state.beginning ? ["argument"] : ["variable"];
		} else if (/[\w_]/.test(char)) {
			stream.eatWhile(/[\w\d_]/);
			const word = stream.current();
			if (reserved.test(word)) {
				token.name = "reserved";
				state.expected = ["selector"];
				state.beginning = false;
			} else if (state.expected.includes("argument")) {
				state.arguments.push(word);
				token.name = "argument";
				state.expected = ["selector", "variable"];
			} else if (state.expected.includes("selector")) {
				if (stream.peek() === ":") {
					stream.next();
					token.name = "selector";
					state.expected = state.beginning ? ["argument"] : ["variable"];
				} else {
					token.name = "selector";
					state.expected = ["selector"];
					state.beginning = false;
				}
			} else {
				token.name = state.arguments.includes(word)
					? "argument"
					: state.temporaries.includes(word)
					? "temporary"
					: word[0] === word[0].toUpperCase()
					? "global"
					: "var";
				state.beginning = false;
				state.expected = ["selector"];
			}
		} else {
			console.log("weird");
		}
		return token;
	};

	var nextComment = function (stream, context) {
		stream.eatWhile(/[^"]/);
		return new Token("comment", stream.eat('"') ? context.parent : context);
	};

	var nextString = function (stream, context) {
		stream.eatWhile(/[^']/);
		return new Token("string", stream.eat("'") ? context.parent : context);
	};

	var nextSymbol = function (stream, context) {
		stream.eatWhile(/[^']/);
		return new Token("symbol", stream.eat("'") ? context.parent : context);
	};

	var nextTemporaries = function (stream, context, state) {
		var token = new Token(null, context);
		var char = stream.next();
		if (char === "|") {
			token.context = context.parent;
		} else {
			stream.eatWhile(/[^|]/);
			state.temporaries.push(
				...stream
					.current()
					.split(" ")
					.filter((t) => t.length > 0)
			);
			token.name = "temporary";
		}
		return token;
	};

	return {
		startState: function () {
			return new State();
		},
		token: function (stream, state) {
			state.userIndent(stream.indentation());
			if (stream.eatSpace()) {
				return null;
			}
			var token = state.context.next(stream, state.context, state);
			state.context = token.context;
			return token.name;
		},
		blankLine: function (state) {
			state.userIndent(0);
		},
		indent: function (state, textAfter) {
			var i =
				state.context.next === next && textAfter && textAfter.charAt(0) === "]"
					? -1
					: state.userIndentationDelta;
			return (state.indentation + i) * config.indentUnit;
		},
		electricChars: "]",
	};
});

CodeMirror.defineMIME("text/x-stsrc", { name: "smalltalk-method" });
