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

	var Token = function (type, context) {
		this.type = type;
		this.context = context;
	};

	var State = function () {
		this.context = new Context(next, null);
		this.first = true;
		this.withinSelector = true;
		this.expected = ["selector"];
		this.expects = (t) => {
			return this.expected.includes(t);
		};
		this.expect = (t) => {
			this.expected = typeof t === "string" ? [t] : t;
		};
		this.isArgument = (v) => {
			return this.arguments.includes(v);
		};
		this.isTemporary = (v) => {
			return this.temporaries.includes(v);
		};
		this.addArgument = (v) => {
			this.arguments.push(v);
		};
		this.addTemporary = (v) => {
			this.temporaries.push(v);
		};
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
			state.expect("variable");
			state.withinSelector = false;
		} else if (char === "'") {
			token = nextString(stream, new Context(nextString, context));
			state.expect("selector");
			state.withinSelector = false;
		} else if (char === "#") {
			if (stream.peek() === "'") {
				stream.next();
				token = nextSymbol(stream, new Context(nextSymbol, context));
			} else {
				if (stream.eatWhile(/[^\s.{}[\]()]/)) {
					token.type = "symbol";
				} else {
					token.type = "meta";
				}
				state.expect("selector");
				state.withinSelector = false;
			}
		} else if (char === "<" && !state.withinSelector && false) {
			stream.eatWhile(/[^\s>]/);
			stream.next();
			token.type = "pragma";
			state.expect("variable");
			state.withinSelector = false;
		} else if (char === "$") {
			stream.eatWhile(/[^\s>]/);
			stream.next();
			token.type = "string";
			state.expect("selector");
			state.withinSelector = false;
		} else if (char === "|" && state.expects("variable")) {
			token.context = new Context(nextTemporaries, context, state);
			state.expect("variable");
			state.withinSelector = false;
		} else if (/[[\]{}()]/.test(char)) {
			token.type = "bracket";
			state.withinSelector = false;
			/[[{(]/.test(char) ? state.expect("variable") : state.expect("selector");
			if (char === "[") {
				state.indentation++;
			} else if (char === "]") {
				state.indentation = Math.max(0, state.indentation - 1);
			}
		} else if (char === "." || char === ";") {
			token.type = "separator";
			char === "." ? state.expect("variable") : state.expect("selector");
		} else if (char === "^") {
			token.type = "return";
			state.expect("variable");
			state.withinSelector = false;
		} else if (char === ":") {
			if (stream.peek() === "=") {
				stream.next();
				token.type = "assignment";
				state.expect("variable");
			}
		} else if (/\d/.test(char)) {
			stream.eatWhile(/[\w\d]/);
			token.type = "numeric";
			state.expect("selector");
		} else if (binary.test(char)) {
			stream.eatWhile(binary);
			token.type = "selector";
			state.withinSelector
				? state.expect("argument")
				: state.expect("variable");
		} else if (/[\w_]/.test(char)) {
			stream.eatWhile(/[\w\d_]/);
			const identifier = stream.current();
			token.value = identifier;
			if (reserved.test(identifier)) {
				token.type = "reserved";
				state.expect("selector");
				state.withinSelector = false;
			} else if (state.expects("variable") && state.isArgument(identifier)) {
				token.type = "argument";
				state.expect("selector");
				state.withinSelector = false;
			} else if (state.expects("variable") && state.isTemporary(identifier)) {
				token.type = "temporary";
				state.expect("selector");
				state.withinSelector = false;
			} else if (state.expects("argument")) {
				state.addArgument(identifier);
				token.type = "argument";
				state.expect(["selector", "variable"]);
			} else if (state.expects("selector") && stream.peek() === ":") {
				stream.next();
				token.type = "selector";
				state.withinSelector
					? state.expect("argument")
					: state.expect("variable");
			} else if (
				state.expects("selector") &&
				(state.first || !state.withinSelector)
			) {
				token.type = "selector";
				state.expect("selector");
				state.withinSelector = false;
			} else {
				token.type =
					identifier[0] === identifier[0].toUpperCase() ? "global" : "var";
				state.expect("selector");
				state.withinSelector = false;
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
			stream
				.current()
				.split(" ")
				.filter((t) => t.length > 0)
				.forEach((t) => state.addTemporary(t));
			token.type = "temporary";
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
			state.first = false;
			//console.log(token.value, token.type);
			return token.type;
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
