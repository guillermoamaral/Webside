export const smalltalkMonarchDefinition = {
	defaultToken: "",
	tokenizer: {
		root: [
			[/"/, { token: "comment", next: "@comment" }],
			[/'/, { token: "string", next: "@string" }],
			[/#[']/, { token: "keyword", next: "@symbol" }],
			[/#\w+/, "keyword"],
			[/\$./, "string"],
			[/[{}()[\]]/, "@brackets"],
			[/\^/, "keyword"],
			[/:=/, "assignment"],
			[/[+\-*/\\~<>=@%|&?!,]/, "selector"],
			[/\b(true|false|nil|self|super|thisContext)\b/, "reserved"],
			[/\|[^|]*\|/, "variable"],
			[/\b\d+(\.\d+)?\b/, "number"],
			[/\b[A-Z]\w*\b/, "variable"],

			// Improved keyword:argument matching with trailing delimiter support
			[
				/(\b[a-z_][a-zA-Z0-9_]*:)(\s*)([a-zA-Z_][a-zA-Z0-9_]*)(?=\s|[.\);\]]|$)/,
				["keyword", "", "argument"],
			],

			// Fallback identifiers
			[/\b[a-z_][a-zA-Z0-9_]*\b/, "variable"],
		],

		comment: [
			[/[^"\n]+/, "comment"],
			[/"/, { token: "comment", next: "@pop" }],
		],

		string: [
			[/[^']+/, "string"],
			[/'/, { token: "string", next: "@pop" }],
		],

		symbol: [
			[/[^']+/, "keyword"],
			[/'/, { token: "keyword", next: "@pop" }],
		],
	},
};
