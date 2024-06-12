class Context {
	constructor(tokenizer, parent) {
		this.next = tokenizer;
		this.parent = parent;
	}
}

class Token {
	constructor(type, context) {
		this.type = type;
		this.context = context;
	}
}

class State {
	constructor() {
		this.context = new Context(next, null);
		this.first = true;
		this.expected = [];
		this.expects = (type) => {
			return this.expected.includes(type);
		};
		this.expect = (type) => {
			this.expected = typeof type === "string" ? [type] : type;
		};
		this.indentation = 0;
		this.userIndentationDelta = 0;
	}
	userIndent(indentation, indentUnit) {
		this.userIndentationDelta =
			indentation > 0 ? indentation / indentUnit - this.indentation : 0;
	}
}

const next = (stream, context, state) => {
	var token = new Token(null, context);
	var char = stream.next();
	if (char === "@") {
		token.type = "at";
		state.expect("class");
	} else if (char === "#") {
		token.type = "at";
		state.expect("selector");
	} else if (state.expects("class")) {
		stream.eatWhile(/[\w.]/);
		token.type = "class";
		state.expect([]);
	} else if (state.expects("selector")) {
		stream.eatWhile(/[\w:]/);
		token.type = "selector";
		state.expect([]);
	} else {
		token.type = "text";
		state.expect([]);
	}
	return token;
};

export const PromptParser = {
	name: "prompt",
	startState: () => {
		return new State();
	},
	token: (stream, state) => {
		state.userIndent(stream.indentation());
		if (stream.eatSpace()) return null;
		let token = state.context.next(stream, state.context, state);
		state.context = token.context;
		state.first = false;
		return token.type;
	},
	blankLine: (state) => {
		state.userIndent(0);
	},
	indent: (state, textAfter, cx) => {
		var i =
			state.context.next === next &&
			textAfter &&
			textAfter.charAt(0) === "]"
				? -1
				: state.userIndentationDelta;
		return (state.indentation + i) * cx.unit;
	},
	electricChars: "]",
};
