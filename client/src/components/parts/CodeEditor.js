import React, { Component } from "react";
import { Grid, Box, IconButton, LinearProgress } from "@material-ui/core";
import AcceptIcon from "@material-ui/icons/CheckCircle";
import { Controlled as CodeMirror } from "react-codemirror2";
import PopupMenu from "../controls/PopupMenu";
import { ide } from "../IDE";
import Scrollable from "../controls/Scrollable";
import "../../SmalltalkMode.css";
import "../../SmalltalkMode.js";

require("diff-match-patch");
require("codemirror/lib/codemirror.css");
require("codemirror/mode/gas/gas.js");
require("codemirror/mode/python/python.js");
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
require("codemirror/addon/hint/show-hint");

function positionFromOffset(offset, source) {
	const lines = source.slice(0, offset).split("\r");
	return { line: lines.length - 1, ch: lines[lines.length - 1].length };
}

function rangeFromInterval(interval, source) {
	return {
		anchor: positionFromOffset(interval.start - 1, source),
		head: positionFromOffset(interval.end, source),
	};
}

class CodeEditor extends Component {
	constructor(props) {
		super(props);
		this.editor = null;
		this.selectsRanges = true;
		this.state = {
			originalSource: props.source,
			selectedInterval: null,
			dirty: false,
			source: props.source,
			selectedRanges: [],
			menuOpen: false,
			menuPosition: { x: null, y: null },
			evaluating: false,
			progress: false,
		};
	}

	static getDerivedStateFromProps(props, state) {
		if (
			/*!state.dirty &&*/
			props.source !== state.originalSource ||
			JSON.stringify(props.selectedInterval) !==
				JSON.stringify(state.selectedInterval) ||
			props.selectedWord !== state.selectedWord
		) {
			const source = props.source;
			const interval = props.selectedInterval;
			const ranges =
				source && interval ? [rangeFromInterval(interval, source)] : [];
			return {
				originalSource: source,
				selectedInterval: interval,
				selectedWord: props.selectedWord,
				source: source,
				selectedRanges: ranges,
				evaluating: props.evaluating,
				dirty: false,
			};
		}
		if (props.evaluating !== state.evaluating) {
			return {
				evaluating: props.evaluating,
			};
		}
		return null;
	}

	shouldComponentUpdate(nextProps, nextState) {
		if (
			nextProps.source !== this.props.source ||
			JSON.stringify(nextProps.selectedInterval) !==
				JSON.stringify(this.props.selectedInterval) ||
			nextProps.selectedWord !== this.props.selectedWord
		) {
			this.selectsRanges = true;
			const ranges =
				nextProps.source && nextProps.selectedInterval
					? [
							rangeFromInterval(
								nextProps.selectedInterval,
								nextProps.source
							),
					  ]
					: [];
			if (ranges) {
				//this.selectRanges(ranges);
			}
		}
		return true;
	}

	componentDidUpdate() {
		if (this.selectsRanges && this.state.selectedRanges) {
			this.selectRanges(this.state.selectedRanges);
		}
	}

	editorDidMount(editor) {
		this.editor = editor;
		this.editor.setSize("100%", "100%");
	}

	markOcurrences = () => {
		const keyword = this.editor.getSelection();
		var cursor = this.editor.getSearchCursor(keyword);
		while (cursor.findNext()) {
			this.editor.markText(cursor.from(), cursor.to(), {
				className: "highlight",
			});
		}
	};

	selectInterval(interval) {
		const range = this.rangeFromInterval(interval);
		this.selectRanges([range]);
	}

	selectWord(word) {
		const ranges = this.rangesContainingWord(word);
		this.selectRanges(ranges);
	}

	rangeFromInterval(interval) {
		return {
			anchor: this.positionFromOffset(interval.start - 1),
			head: this.positionFromOffset(interval.end),
		};
	}

	rangesContainingWord(word) {
		const ranges = [];
		if (!this.editor) {
			return;
		}
		var cursor = this.editor.getSearchCursor(word);
		while (cursor.findNext()) {
			ranges.push({
				anchor: cursor.from(),
				head: cursor.to(),
			});
		}
		return ranges;
	}

	positionFromOffset(offset) {
		const source = this.state.source || "";
		const lines = source.slice(0, offset).split("\r");
		return { line: lines.length - 1, ch: lines[lines.length - 1].length };
	}

	offsetFromPosition(position) {
		const lines = this.state.source.split("\r");
		var before = 0;
		for (var i = 0; i < position.line; i++) {
			before += lines[i].length + 1;
		}
		return before + position.ch;
	}

	traverseAst(node, block) {
		block(node);
		if (node.children) {
			node.children.forEach((n) => {
				this.traverseAst(n, block);
			});
		}
	}

	astNodeAtOffset(offset) {
		const ast = this.props.ast;
		var node;
		if (ast) {
			this.traverseAst(ast, (n) => {
				if (
					n.start <= offset &&
					offset <= n.end &&
					(!node || (node.start <= n.start && n.end <= node.end))
				) {
					node = n;
				}
			});
		}
		return node;
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
			{ label: "Senders (Alt+n)", action: this.browseSenders },
			{ label: "Implementors (Alt+m)", action: this.browseImplementors },
			{
				label: "Class references (Alt+r)",
				action: this.browseClassReferences,
			},
			{ label: "Methods matching", action: this.browseMethodsMatching },
			{ label: "String references", action: this.browseStringReferences },
		];
	}

	copyToClipboard = () => {
		const text = this.editor.getSelection();
		navigator.clipboard.writeText(text);
	};

	pasteFromClipboard = () => {
		navigator.clipboard.readText().then(
			(text) => {
				this.editor.replaceSelection(text);
			},
			(error) => console.log(error)
		);
	};

	acceptClicked = () => {
		if (this.props.onAccept) {
			this.props.onAccept(this.state.source);
		}
	};

	toggleFullScreen = () => {
		this.editor.setOption(
			"fullScreen",
			!this.editor.getOption("fullScreen")
		);
	};

	targetWord() {
		const selected = this.editor.getSelection();
		if (selected.length > 0) {
			return selected;
		}
		const stretch = this.editor.findWordAt(this.editor.getCursor());
		return this.editor.getRange(stretch.anchor, stretch.head);
	}

	targetSelector() {
		const selected = this.editor.getSelection();
		if (selected.length > 0) {
			return selected.trim();
		}
		const position = this.editor.getCursor();
		const offset = this.offsetFromPosition(position);
		const node = this.astNodeAtOffset(offset);
		if (node && (node.type === "Selector" || node.type === "Literal")) {
			return node.value;
		}
		const stretch = this.editor.findWordAt(position);
		const head = stretch.head;
		head.ch = head.ch + 1;
		const word = this.editor.getRange(stretch.anchor, head);
		return word.endsWith(":") ? word.slice(0, word.length - 1) : word;
	}

	searchInGoogle = () => {
		const url = "https://www.google.com/search?q=" + this.targetWord();
		window.open(url, "_blank").focus();
	};

	browseSenders = () => {
		ide.browseSenders(this.targetSelector());
		return false;
	};

	browseImplementors = () => {
		ide.browseImplementors(this.targetSelector());
	};

	browseClass = () => {
		ide.browseClass(this.targetWord());
	};

	browseClassReferences = () => {
		ide.browseClassReferences(this.targetWord());
	};

	browseMethodsMatching = () => {
		ide.browseMethodsMatching(this.targetWord());
	};

	browseStringReferences = () => {
		ide.browseStringReferences(this.targetWord());
	};

	selectedExpression() {
		const expression = this.editor.getSelection();
		if (expression.length > 0) {
			return expression;
		}
		const cursor = this.editor.getCursor();
		return this.editor.getLine(cursor.line);
	}

	debugExpression = async () => {
		const expression = this.selectedExpression();
		try {
			await ide.debugExpression(expression, this.props.context);
		} catch (error) {}
	};

	profileExpression = async () => {
		const expression = this.selectedExpression();
		try {
			await ide.profileExpression(expression, this.props.context);
		} catch (error) {}
	};

	evaluateExpression = async () => {
		const expression = this.selectedExpression();
		try {
			this.setState({ progress: true });
			await ide.evaluateExpression(
				expression,
				false,
				false,
				this.props.context
			);
			this.setState({ progress: false }, this.triggerOnEvaluate());
		} catch (error) {
			this.setState({ progress: false });
		}
	};

	triggerOnEvaluate() {
		if (this.props.onEvaluate) {
			this.props.onEvaluate();
		}
	}

	showEvaluation = async () => {
		const expression = this.selectedExpression();
		try {
			this.setState({ progress: true });
			const object = await ide.evaluateExpression(
				expression,
				false,
				false,
				this.props.context
			);
			this.setState({ progress: false });
			const cursor = this.editor.getCursor("to");
			if (this.editor.getSelection().length === 0) {
				cursor.ch = this.editor.getLine(cursor.line).length;
			}
			this.editor.replaceRange(" " + object.printString, cursor);
			const from = { ch: cursor.ch + 1, line: cursor.line };
			const to = {
				ch: from.ch + object.printString.length,
				line: from.line,
			};
			this.editor.setSelection(from, to);
		} catch (error) {
			this.setState({ progress: false });
		}
	};

	inspectEvaluation = async () => {
		const expression = this.selectedExpression();
		try {
			this.setState({ progress: true });
			const object = await ide.evaluateExpression(
				expression,
				false,
				true,
				this.props.context
			);
			this.setState({ progress: false });
			ide.openInspector(object);
		} catch (error) {
			this.setState({ progress: false });
		}
	};

	lintAnnotations = () => {
		if (!this.props.lintAnnotations) {
			return [];
		}
		return this.props.lintAnnotations.map((a) => {
			return {
				from: this.positionFromOffset(a.from - 1),
				to: this.positionFromOffset(a.to - 1),
				severity: a.severity,
				message: a.description,
			};
		});
	};

	setBreakpoint = (n) => {
		var info = this.editor.lineInfo(n);
		this.editor.setGutterMarker(
			n,
			"breakpoints",
			info.gutterMarkers ? null : this.makeMarker()
		);
	};

	makeMarker() {
		var marker = document.createElement("div");
		marker.style.color = "red";
		marker.innerHTML = "●";
		return marker;
	}

	renameTarget = () => {
		const target = this.targetWord();
		if (!target || target === "") {
			return;
		}
		if (this.props.onRename) {
			this.props.onRename(target);
		}
	};

	selectRanges(ranges) {
		const selectsRanges = this.selectsRanges;
		if (this.editor) {
			this.editor.setSelections(ranges);
		}
		this.selectsRanges = selectsRanges;
	}

	sourceChanged = (source) => {
		this.selectsRanges = false;
		this.setState(
			{
				source: source,
				dirty: true,
			},
			() => {
				if (this.props.onChange) {
					this.props.onChange(source);
				}
			}
		);
	};

	selectionChanged = (selection) => {
		//this.selectsRanges = false;
	};

	render() {
		const { source, selectedRanges, evaluating, progress, dirty } =
			this.state;
		const mode = this.props.mode || "smalltalk-method";
		const showAccept = this.props.showAccept;
		const acceptIcon = this.props.acceptIcon ? (
			React.cloneElement(this.props.acceptIcon)
		) : (
			<AcceptIcon
				size="large"
				color={dirty ? "primary" : "inherit"}
				style={{ fontSize: 30 }}
			/>
		);
		return (
			<Grid container spacing={1} style={{ height: "100%" }}>
				<Grid
					item
					xs={11}
					md={showAccept ? 11 : 12}
					lg={showAccept ? 11 : 12}
				>
					<Scrollable>
						<CodeMirror
							className={this.props.styles.codeMirror}
							options={{
								readOnly: evaluating || progress,
								mode: mode,
								theme: "material",
								lineSeparator: "\r",
								lineNumbers: this.props.lineNumbers,
								matchBrackets: true,
								autoCloseBrackets: true,
								//highlightSelectionMatches: true,
								highlightSelectionMatches: {
									annotateScrollbar: true,
								},
								indentUnit: 10,
								styleActiveLine: true,
								matchTags: { bothTags: true },
								lineWrapping: true,
								gutters: [
									"CodeMirror-lint-markers",
									"breakpoints",
								],
								lint: { getAnnotations: this.lintAnnotations },
								extraKeys: {
									"Ctrl-D": this.evaluateExpression,
									"Ctrl-I": this.inspectEvaluation,
									"Ctrl-P": this.showEvaluation,
									"Ctrl-U": this.debugExpression,
									"Ctrl-S": this.acceptClicked,
									"Ctrl-B": this.browseClass,
									"Alt-N": this.browseSenders,
									"Alt-M": this.browseImplementors,
									"Alt-R": this.browseClassReferences,
									"Ctrl-Q": this.markOcurrences,
									"Alt-Z": this.toggleFullScreen,
									F2: this.renameTarget,
								},
							}}
							value={source}
							selection={{ ranges: selectedRanges, focus: true }}
							editorDidMount={(editor) => {
								this.editorDidMount(editor);
							}}
							onGutterClick={(editor, n) => {
								this.setBreakpoint(n);
							}}
							onBeforeChange={(editor, data, value) => {
								this.sourceChanged(value);
							}}
							onChange={(editor, data, value) => {
								// console.log("onChange fired");
								// this.setState({ selectRanges: value === this.props.source });
							}}
							onContextMenu={(editor, event) => {
								this.openMenu(event);
							}}
							onSelection={(editor, selection) => {
								this.selectionChanged(selection);
							}}
							onCursorActivity={(editor, event) => {}}
						/>
					</Scrollable>
					{(evaluating || progress) && (
						<LinearProgress variant="indeterminate" />
					)}
				</Grid>
				{showAccept && (
					<Grid item xs={1} md={1} lg={1}>
						<Box display="flex" justifyContent="center">
							<IconButton
								color="inherit"
								onClick={this.acceptClicked}
							>
								{acceptIcon}
							</IconButton>
						</Box>
					</Grid>
				)}
				<PopupMenu
					options={this.menuOptions()}
					open={this.state.menuOpen}
					position={this.state.menuPosition}
					onClose={this.closeMenu}
				/>
			</Grid>
		);
	}
}

export default CodeEditor;
