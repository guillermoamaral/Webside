import React, { Component } from "react";
import CodeMirror from "codemirror";
import "../../SmalltalkMode.js";
import "../../SmalltalkMode.css";

require("codemirror/lib/codemirror.css");
require("codemirror/theme/material.css");
require("codemirror/mode/smalltalk/smalltalk.js");
require("codemirror/addon/search/searchcursor.js");
require("codemirror/addon/search/search.js");
require("codemirror/addon/search/jump-to-line.js");
require("codemirror/addon/search/match-highlighter.js");
require("codemirror/addon/edit/matchbrackets.js");
require("codemirror/addon/edit/closebrackets.js");
require("codemirror/addon/comment/comment.js");
require("codemirror/addon/selection/active-line.js");
require("codemirror/addon/display/fullscreen.js");
require("codemirror/addon/display/fullscreen.css");
require("codemirror/addon/scroll/annotatescrollbar.js");
require("codemirror/addon/lint/lint.js");
require("codemirror/addon/lint/lint.css");
require("codemirror/addon/merge/merge.js");
require("codemirror/addon/merge/merge.css");
require("codemirror/addon/fold/foldgutter.css");
require("codemirror/addon/fold/foldcode");
require("codemirror/addon/fold/brace-fold");
require("codemirror/addon/fold/comment-fold");

class CodeMerge extends Component {
	constructor(props) {
		super(props);
		this.ref = React.createRef();
	}

	componentDidMount() {
		this.ref.current.innerHTML = "";
		this.editor = CodeMirror.MergeView(
			this.ref.current,
			Object.assign(
				{},
				{
					lineNumbers: true,
					theme: "material",
					lineSeparator: "\r",
					value: this.props.leftCode || "no left source",
					orig: this.props.rightCode || "no right source",
					mode: "smalltalk-method",
					highlightDifferences: "highlight",
					connect: null,
					indentUnit: 10,
					revertButtons: false,
					styleActiveLine: true,
					lineWrap: true,
					matchTags: { bothTags: true },
					smartIndent: true,
					matchBrackets: true,
					foldGutter: true,
					lineWrapping: true,
					gutters: ["CodeMirror-linenumbers", "CodeMirror-foldgutter"],
				},
				this.props.options || {}
			)
		);
	}

	render() {
		if (this.editor) {
			this.editor.editor().setValue(this.props.leftCode);
			this.editor.rightOriginal().setValue(this.props.rightCode);
		}
		return <div ref={this.ref} className={this.props.styles.codeMirror}></div>;
	}
}

export default CodeMerge;
