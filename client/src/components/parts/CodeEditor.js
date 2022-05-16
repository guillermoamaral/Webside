import React, { Component } from "react";
import { Grid, Box, IconButton, LinearProgress } from "@material-ui/core";
import AcceptIcon from "@material-ui/icons/CheckCircle";
import { Controlled as CodeMirror } from "react-codemirror2";
import PopupMenu from "../controls/PopupMenu";
import { IDEContext } from "../IDEContext";
import Scrollable from "../controls/Scrollable";
import "../../smalltalk.css";

require("diff-match-patch");
require("codemirror/lib/codemirror.css");
require("codemirror/theme/material.css");
require("codemirror/mode/smalltalk/smalltalk.js");
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

class CodeEditor extends Component {
	static contextType = IDEContext;

	constructor(props) {
		super(props);
		this.editor = null;
		this.state = {
			originalSource: props.source,
			selectedInterval: null,
			selectRanges: true,
			dirty: false,
			source: props.source,
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
			props.selectedInterval !== state.selectedInterval ||
			props.selectedWord !== state.selectedWord
		) {
			console.log("derived state");
			return {
				originalSource: props.source,
				selectedInterval: props.selectedInterval,
				selectedWord: props.selectedWord,
				selectRanges: true,
				source: props.source,
				evaluating: props.evaluating,
			};
		}
		if (state.evaluating !== props.evaluating) {
			return {
				evaluating: props.evaluating,
			};
		}
		console.log("no derived state");
		return null;
	}

	editorDidMount(editor) {
		console.log("editorDidMount", this.editor, this.editor === editor);
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
		console.log("selectIterval", interval);
		const range = this.rangeFromInterval(interval);
		console.log("selecteRange", range.anchor, range.head);
		this.selectRanges([range]);
	}

	selectWord(word) {
		console.log("selectWord", word);
		const ranges = this.rangesContainingWord(word);
		this.selectRanges(ranges);
	}

	selectRanges(ranges) {
		if (this.editor) {
			console.log("settingSelections to editor");
			this.editor.setSelections(ranges);
		}
	}

	selectDefaultRanges() {
		const { selectRanges, selectedInterval, selectedWord } = this.state;
		if (selectRanges) {
			if (selectedInterval) {
				this.selectInterval(selectedInterval);
			}
			if (selectedWord) {
				this.selectWord(selectedWord);
			}
		}
	}

	rangeFromInterval(interval) {
		return {
			anchor: this.linePositionAtOffset(interval.start - 1),
			head: this.linePositionAtOffset(interval.end),
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

	linePositionAtOffset(offset) {
		var lines = this.state.source.slice(0, offset).split("\r");
		return { line: lines.length - 1, ch: lines[lines.length - 1].length };
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
			{ label: "Do it (Ctrl+d)", action: this.evaluateExpression },
			{ label: "Show it (Ctrl+s)", action: this.showEvaluation },
			{ label: "Inspect it (Ctrl+i)", action: this.inspectEvaluation },
			{ label: "Debug it (Ctrl+u)", action: this.debugExpression },
			{ label: "Profile it", action: this.profileExpression },
			null,
			{ label: "Browse class (Ctrl+b)", action: this.browseClass },
			{ label: "Senders (Alt+n)", action: this.browseSenders },
			{ label: "Implementors (Alt+m)", action: this.browseImplementors },
			{ label: "Class references (Alt+r)", action: this.browseReferences },
		];
	}

	sourceChanged = (source) => {
		console.log("sourceChanged", source);
		const handler = this.props.onChange;
		if (handler) {
			handler(source);
		} else {
			console.log("setting state after sourceChanged");
			this.setState(
				{
					source: source,
					dirty: true,
					selectRanges: false,
				},
				() => console.log(this.state)
			);
		}
	};

	acceptClicked = (event) => {
		const handler = this.props.onAccept;
		if (handler) {
			handler(this.state.source);
		}
	};

	toggleFullScreen = (event) => {
		this.editor.setOption("fullScreen", !this.editor.getOption("fullScreen"));
	};

	targetWord() {
		const word = this.editor.getSelection();
		if (word.length > 0) {
			return word;
		}
		const stretch = this.editor.findWordAt(this.editor.getCursor());
		return this.editor.getRange(stretch.anchor, stretch.head);
	}

	browseSenders = () => {
		this.context.browseSenders(this.targetWord());
	};

	browseImplementors = () => {
		this.context.browseImplementors(this.targetWord());
	};

	browseClass = () => {
		this.context.browseClass(this.targetWord());
	};

	browseReferences = () => {
		this.context.browseReferences(this.targetWord());
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
			await this.context.debugExpression(expression, this.props.context);
		} catch (error) {}
	};

	profileExpression = async () => {
		const expression = this.selectedExpression();
		try {
			await this.context.profileExpression(expression, this.props.context);
		} catch (error) {}
	};

	evaluateExpression = async () => {
		const expression = this.selectedExpression();
		try {
			this.setState({ progress: true });
			await this.context.evaluateExpression(
				expression,
				false,
				false,
				this.props.context
			);
			this.setState({ progress: false });
		} catch (error) {
			this.setState({ progress: false });
		}
	};

	showEvaluation = async () => {
		const expression = this.selectedExpression();
		try {
			this.setState({ progress: true });
			const object = await this.context.evaluateExpression(
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
			const to = { ch: from.ch + object.printString.length, line: from.line };
			this.editor.setSelection(from, to);
		} catch (error) {
			this.setState({ progress: false });
		}
	};

	inspectEvaluation = async () => {
		const expression = this.selectedExpression();
		try {
			this.setState({ progress: true });
			const object = await this.context.evaluateExpression(
				expression,
				false,
				true,
				this.props.context
			);
			this.setState({ progress: false });
			this.context.inspectObject(object);
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
				from: this.linePositionAtOffset(a.from - 1),
				to: this.linePositionAtOffset(a.to - 1),
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
		const handler = this.props.onRename;
		if (handler) {
			handler(target);
		}
	};

	selectionChanged = (selection) => {
		var m = "";
		selection.ranges.forEach(
			(r) =>
				(m =
					m +
					" from line " +
					r.anchor.line +
					" ch " +
					r.anchor.ch +
					" to line " +
					r.head.line +
					" ch " +
					r.head.ch)
		);
		console.log("selectionChanged", m);
		console.log(selection);
		if (selection.origin) {
			this.setState({ selectRanges: false });
		}
		// const { selectedInterval, selectedWord } = this.state;
		// const additional = [];
		// if (selectedInterval) {
		// 	additional.push(this.rangeFromInterval(selectedInterval));
		// }
		// if (selectedWord) {
		// 	this.rangesContainingWord(selectedWord).forEach((r) =>
		// 		additional.push(r)
		// 	);
		// }
		// if (additional.length > 0) {
		// 	console.log(additional);
		// 	selection.update(selection.ranges.concat(additional));
		// }
		// if (this.state.selectRanges) {
		// 	this.setState({ selectRanges: false });
		// }
	};

	render() {
		console.log("rendering CodeEditor");
		this.selectDefaultRanges();
		const { source, evaluating, progress } = this.state;
		const mode = this.props.mode || "smalltalk";
		const showAccept = this.props;
		const acceptIcon = this.props.acceptIcon ? (
			React.cloneElement(this.props.acceptIcon)
		) : (
			<AcceptIcon size="large" style={{ fontSize: 30 }} />
		);
		return (
			<Grid container spacing={1} style={{ height: "100%" }}>
				<Grid item xs={11} md={showAccept ? 11 : 12} lg={showAccept ? 11 : 12}>
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
								highlightSelectionMatches: { annotateScrollbar: true },
								indentUnit: 10,
								styleActiveLine: true,
								matchTags: { bothTags: true },
								lineWrapping: true,
								gutters: ["CodeMirror-lint-markers", "breakpoints"],
								lint: { getAnnotations: this.lintAnnotations },
								extraKeys: {
									"Ctrl-D": this.evaluateExpression,
									"Ctrl-I": this.inspectEvaluation,
									"Ctrl-S": this.showEvaluation,
									"Ctrl-P": this.showEvaluation,
									"Ctrl-U": this.debugExpression,
									"Alt-S": this.acceptClicked,
									"Ctrl-B": this.browseClass,
									"Alt-N": this.browseSenders,
									"Alt-M": this.browseImplementors,
									"Alt-R": this.browseReferences,
									"Ctrl-Q": this.markOcurrences,
									"Alt-Z": this.toggleFullScreen,
									F2: this.renameTarget,
								},
							}}
							value={source}
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
								this.setState({ selectRanges: value === this.props.source });
							}}
							onContextMenu={(editor, event) => {
								this.openMenu(event);
							}}
							onSelection={(editor, selection) => {
								this.selectionChanged(selection);
							}}
						/>
					</Scrollable>
					{(evaluating || progress) && (
						<LinearProgress variant="indeterminate" />
					)}
				</Grid>
				{showAccept && (
					<Grid item xs={1} md={1} lg={1}>
						<Box display="flex" justifyContent="center">
							<IconButton color="inherit" onClick={this.acceptClicked}>
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
