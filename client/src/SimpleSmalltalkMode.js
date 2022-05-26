import CodeMirror from "codemirror";

CodeMirror.defineMode("smalltalk-method", function (config) {
	var binary = /[+\-/\\*~<>=@%|&?!,]/;
	var reserved = /true|false|nil|self|super|thisContext/;
	return {
		token: (stream, state) => {
			stream.next();
			return "global";
		},
	};
});

CodeMirror.defineMIME("text/x-stsrc", { name: "smalltalk-method" });
