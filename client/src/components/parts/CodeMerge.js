import React, { Component } from "react";
import CodeMirror from "codemirror";
import "../../SmalltalkMode.js";
import "../../SmalltalkMode.css";
import Scrollable from "../controls/Scrollable.js";
import PopupMenu from "../controls/PopupMenu";
//import CodeEditor from "./CodeEditor";
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
		this.state = {
			leftCode: "",
			rightCode: "",
			menuOpen: false,
			menuPosition: { x: null, y: null },
		};
	}

	static getDerivedStateFromProps(props, state) {
		return {
			leftCode: props.leftCode || "",
			rightCode: props.rightCode || "",
		};
	}

	componentDidMount() {
		this.ref.current.innerHTML = "";
		const { leftCode, rightCode } = this.state;
		this.editor = CodeMirror.MergeView(this.ref.current, {
			lineNumbers: true,
			theme: "material",
			//lineSeparator: "\r",
			value: leftCode,
			orig: rightCode,
			mode: "smalltalk-method",
			highlightDifferences: "align",
			//allowEditingOriginals: true,
			//connect,
			//collapseIdentical: "align",
			indentUnit: 10,
			revertButtons: true,
			//styleActiveLine: true,
			lineWrap: true,
			matchTags: { bothTags: true },
			smartIndent: true,
			matchBrackets: true,
			//foldGutter: true,
			lineWrapping: true,
			gutters: ["CodeMirror-linenumbers", "CodeMirror-foldgutter"],
		});
	}

	openMenu = (event) => {
		event.preventDefault();
		this.setState({
			menuOpen: true,
			menuPosition: { x: event.clientX - 2, y: event.clientY - 4 },
		});
	};

	closeMenu = () => {
		this.setState({ menuOpen: false });
	};

	menuOptions() {
		return [
			{ label: "Copy (Ctrl+c)", action: this.copyToClipboard },
			{ label: "Paste (Ctrl+v)", action: this.pasteFromClipboard },
			null,
			{ label: "Do it (Ctrl+d)", action: this.evaluateExpression },
			{ label: "Print it (Ctrl+p)", action: this.showEvaluation },
			{ label: "Inspect it (Ctrl+i)", action: this.inspectEvaluation },
			{ label: "Debug it (Ctrl+u)", action: this.debugExpression },
			{ label: "Profile it", action: this.profileExpression },
			{ label: "Google it", action: this.searchInGoogle },
			null,
			{ label: "Browse class (Ctrl+b)", action: this.browseClass },
			{ label: "Browse senders (Alt+n)", action: this.browseSenders },
			{
				label: "Browse implementors (Alt+m)",
				action: this.browseImplementors,
			},
			{
				label: "Browse class references (Alt+r)",
				action: this.browseClassReferences,
			},
			{
				label: "Search methods matching",
				action: this.browseMethodsMatching,
			},
			{
				label: "Search string references",
				action: this.browseStringReferences,
			},
		];
	}

	render() {
		const { leftCode, rightCode } = this.state;
		if (this.editor) {
			const left = this.editor.editor();
			left.setSize("100%", "100%");
			left.setValue(leftCode);
			const right = this.editor.rightOriginal();
			right.setSize("100%", "100%");
			right.setValue(rightCode);
			//this.editor.right.forceUpdate();
		}
		return (
			<Scrollable>
				<div
					ref={this.ref}
					style={{ height: "100%" }}
					className={this.props.styles.codeMirror}
					onContextMenu={(event) => {
						this.openMenu(event);
					}}
				></div>
				<PopupMenu
					options={this.menuOptions()}
					open={this.state.menuOpen}
					position={this.state.menuPosition}
					onClose={this.closeMenu}
				/>
			</Scrollable>
		);
	}
}

export default CodeMerge;
