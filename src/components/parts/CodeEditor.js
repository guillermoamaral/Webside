import React, { Component } from "react";
import ReactDOM from "react-dom/client";
import { Box, IconButton, LinearProgress, Tooltip } from "@mui/material";
import AcceptIcon from "@mui/icons-material/CheckCircle";
import PlayIcon from "@mui/icons-material/PlayArrow";
import PauseIcon from "@mui/icons-material/Pause";
import PopupMenu from "../controls/PopupMenu";
import { ide } from "../IDE";
import ToolContainerContext from "../ToolContainerContext";
import Scrollable from "../controls/Scrollable";
import CodeMirror from "@uiw/react-codemirror";
import { EditorView, keymap, lineNumbers } from "@codemirror/view";
import { linter } from "@codemirror/lint";
import { acceptCompletion } from "@codemirror/autocomplete";
//import { LRLanguage, LanguageSupport } from "@codemirror/language";
//import { styleTags, tags } from "@lezer/highlight";
import { SmalltalkLexer } from "../../SmalltalkLexer";
import { Tag } from "@lezer/highlight";
import { createTheme } from "@uiw/codemirror-themes";
import { StreamLanguage } from "@codemirror/language";
//import { EditorSelection, EditorState, Prec } from "@codemirror/state";
import { Prec } from "@codemirror/state";
import {
	autocompletion,
	closeCompletion,
	startCompletion,
} from "@codemirror/autocomplete";
import { hoverTooltip } from "@codemirror/view";
import CodeTooltip from "./CodeTooltip";
import ImproveIcon from "@mui/icons-material/AutoFixHigh";
import TestIcon from "../icons/TestRunnerIcon";
import ExplainIcon from "@mui/icons-material/QuestionMark";
import StAST from "../../model/StAST";
import { ThemeProvider } from "@mui/material/styles";
import { withTheme } from "@emotion/react";
import { darken } from "@mui/system";

// import {
// 	hyperLinkExtension,
// 	hyperLinkStyle,
// } from "@uiw/codemirror-extensions-hyper-link";

// This code is intended to be used when defining a parser from a Lezer grammar.
// const parser = SmalltalkParser.configure({
// 	props: [
// 		styleTags({
// 			argument: tags.variableName,
// 			temporary: tags.variableName,
// 		}),
// 	],
// });
// const language = LRLanguage.define({ parser: parser });
// const smalltalk = new LanguageSupport(smalltalk);

// This code makes use of a CodeMirror5-like parser and should be replaced in the future by the Lezer grammar option abvove.

const newTags = {
	selector: Tag.define(),
	symbol: Tag.define(),
	argument: Tag.define(),
	temporary: Tag.define(),
	assignment: Tag.define(),
	string: Tag.define(),
	variable: Tag.define(),
	var: Tag.define(),
	meta: Tag.define(),
	bracket: Tag.define(),
	reserved: Tag.define(),
	self: Tag.define(),
	super: Tag.define(),
	true: Tag.define(),
	false: Tag.define(),
	nil: Tag.define(),
	thisContext: Tag.define(),
	return: Tag.define(),
	global: Tag.define(),
	number: Tag.define(),
	comment: Tag.define(),
	separator: Tag.define(),
};

class CodeEditor extends Component {
	static contextType = ToolContainerContext;

	constructor(props) {
		super(props);
		this.editorRef = React.createRef();
		this.editorView = null;
		this.selectsRanges = true;
		this.typingTimer = null;
		this.changeEventFrequency = 500; // miliseconds
		this.autocompletionTimer = null;
		this.tooltipRoot = null;
		this.state = {
			originalSource: props.originalSource ?? props.source,
			source: props.source,
			selectedInterval: null,
			dirty: false,
			selectedRanges: [],
			menuOpen: false,
			menuPosition: { x: null, y: null },
			evaluating: false,
			extendedOptions: [],
			currentEvaluation: null,
		};
		this.lastSelection = null;
	}

	static normalizeNewlines(source = "") {
		return source.replace(/(?<!\r)\n|\r(?!\n)/g, "\r");
	}

	static getDerivedStateFromProps(props, state) {
		const {
			source,
			originalSource = source,
			selectedInterval,
			selectedSelector,
			selectedIdentifier,
		} = props;
		if (
			//!state.dirty &&
			CodeEditor.normalizeNewlines(originalSource) !==
				CodeEditor.normalizeNewlines(state.originalSource) ||
			JSON.stringify(selectedInterval) !==
				JSON.stringify(state.selectedInterval) ||
			selectedSelector !== state.selectedSelector ||
			selectedIdentifier !== state.selectedIdentifier
		) {
			const ranges =
				source &&
				selectedInterval &&
				selectedInterval.start >= 1 &&
				selectedInterval.end <= source.length
					? [
							{
								from: selectedInterval.start - 1,
								anchor: selectedInterval.start - 1,
								to: selectedInterval.end,
								head: selectedInterval.end,
							},
					  ]
					: [];
			return {
				originalSource: originalSource ?? source,
				selectedInterval: selectedInterval,
				selectedSelector: selectedSelector,
				selectedIdentifier: selectedIdentifier,
				source: source,
				selectedRanges: ranges,
				dirty: false,
			};
		}
		return null;
	}

	componentDidMount() {
		this.initializeExtendedOptions();
		ide.onColorModeChange(this.colorModeChanged);
	}

	componentWillUnmount() {
		ide.removeColorModeChangeHandler(this.colorModeChanged);
	}

	colorModeChanged = () => {
		this.forceUpdate();
	};

	shouldComponentUpdate(nextProps, nextState) {
		if (this.state.dirty !== nextState.dirty) return true;
		if (
			nextProps.source !== this.props.source ||
			JSON.stringify(nextProps.selectedInterval) !==
				JSON.stringify(this.props.selectedInterval) ||
			nextProps.selectedSelector !== this.props.selectedSelector ||
			nextProps.selectedIdentifier !== this.props.selectedIdentifier
		) {
			this.selectsRanges = true;
			return true;
		}
		if (
			nextState.evaluating !== this.state.evaluating ||
			nextState.currentEvaluation !== this.state.currentEvaluation ||
			(typeof nextProps.evaluating === "boolean" &&
				nextProps.evaluating !== this.state.evaluating) ||
			(typeof nextProps.readOnly === "boolean" &&
				nextProps.readOnly !== this.props.readOnly)
		) {
			return true;
		}
		// Review this as responding false none of local setState() calls trigger a re-rendering
		// For instance, openMenu(), which is intended to open the popup menu, never gets a rerendering (and so the menu open)
		return false;
	}

	componentDidUpdate() {
		if (this.state.dirty) return;
		if (!this.selectsRanges || !this.editorView) return;
		const { selectedRanges, selectedSelector, selectedIdentifier } =
			this.state;
		if (selectedRanges) this.selectRanges(selectedRanges);
		if (selectedSelector) {
			const ranges = this.rangesContainingSelector(selectedSelector);
			this.selectRanges(ranges);
		}
		if (selectedIdentifier) {
			const ranges = this.rangesContainingIdentifier(selectedIdentifier);
			this.selectRanges(ranges);
		}
		const limit = this.editorView.state.doc.length;
		const range = this.lastSelection;
		if (
			range &&
			range.anchor >= 0 &&
			range.head >= 0 &&
			range.anchor <= limit &&
			range.head <= limit
		) {
			this.editorView.dispatch({
				selection: range,
				scrollIntoView: true,
			});
			this.lastSelection = null;
		}
	}

	async initializeExtendedOptions() {
		let extensions = await ide.fetchExtendedOptions("code");
		this.setState({ extendedOptions: extensions });
	}

	editor() {
		return this.editorRef.current;
	}

	currentState() {
		if (this.editorView && this.editorView.viewState) {
			return this.editorView.viewState.state;
		}
	}

	currentSelectionRange() {
		const state = this.currentState();
		if (state && state.selection) return state.selection.main;
	}

	selectedText() {
		const range = this.currentSelectionRange();
		return range ? this.textInRange(range) : "";
	}

	textInRange(range) {
		let source = this.normalizedSource();
		if (source) return source.slice(range.from, range.to);
	}

	selectRanges(ranges) {
		if (ranges.length === 0) return;
		// This hack sucks!
		// It was the way I found to set selection after a rendering: the value changes but editorView
		// has a delay in updating its state, and so the first attempt to set the selection might fail
		// (as the new selection might surpass the previous value boundaries)
		for (let i = 0; i < 2; i++) {
			setTimeout(() => {
				try {
					this.editorView?.dispatch({
						selection: {
							anchor: ranges[0].anchor,
							head: ranges[0].head,
						},
						scrollIntoView: true,
					});
				} catch (ignored) {}
			}, 200 * i);
		}
	}

	insertText(text, position) {
		this.editorView.dispatch({
			changes: { from: position, to: position, insert: text },
		});
	}

	ast() {
		const json = this.props.ast;
		if (!json) return;
		const ast = new StAST();
		ast.fromJson(json);
		return ast;
	}

	astRangesSatisfying(condition) {
		const ast = this.ast();
		if (!ast) return [];
		return ast.nodesSatisfying(condition).map((node) => {
			return { anchor: node.start - 1, head: node.end };
		});
	}

	rangesContainingSelector(selector) {
		const ranges = [];
		const ast = this.props.ast;
		if (!ast) {
			// Should try by using a SearchCursor and selector as a string or regex
			return ranges;
		}
		return this.astRangesSatisfying(
			(node) =>
				(node.type === "Selector" || node.type === "Literal") &&
				node.value === selector
		);
	}

	renameIdentifier = async (identifier) => {
		const replacement = await ide.prompt({
			title: "Replacement",
			defaultValue: identifier,
		});
		if (!replacement) return;
		const changes = this.rangesContainingIdentifier(identifier).map(
			(range) => {
				return {
					from: range.anchor,
					to: range.head,
					insert: replacement,
				};
			}
		);
		this.editorView.dispatch({
			changes: changes,
		});
	};

	rangesContainingIdentifier(identifier) {
		const ranges = [];
		const ast = this.props.ast;
		if (!ast) {
			// Should try by using a SearchCursor and selector as a string or regex
			return ranges;
		}
		return this.astRangesSatisfying(
			(node) => node.type === "Identifier" && node.value === identifier
		);
	}

	astNodeAtOffset(offset) {
		const ast = this.ast();
		if (!ast) return;
		return ast.nodeAt(offset);
	}

	openMenu = (event) => {
		event.preventDefault();
		event.stopPropagation();
		this.setState({
			menuOpen: true,
			menuPosition: { x: event.clientX - 2, y: event.clientY - 4 },
		});
		this.forceUpdate(); // Check this according to the default response in shouldComponentUpdate()
	};

	closeMenu = () => {
		this.setState({ menuOpen: false });
		this.forceUpdate(); // Check this according to the default response in shouldComponentUpdate()
	};

	menuOptions() {
		const shortcuts = ide.settings.section("shortcuts");
		const options = [
			{ label: "Copy", shortcut: "Ctrl+c", action: this.copyToClipboard },
			{
				label: "Paste",
				shortcut: "(Ctrl+v)",
				action: this.pasteFromClipboard,
			},
			null,
		];
		if (this.normalizedSource() === this.normalizedOriginalSource()) {
			const node = this.targetAstNode();
			if (node && node.type === "Identifier") {
				options.push({
					label: "Rename " + node.value + "",
					action: () => {
						this.renameIdentifier(node.value);
					},
				});
			}
		}
		const species = this.props.class;
		options.push(
			...[
				{
					label: "Do it",
					shortcut: shortcuts.get("evaluateExpression"),
					action: this.evaluateSelection,
				},
				{
					label: "Print it",
					shortcut: shortcuts.get("showEvaluation"),
					action: this.showEvaluation,
				},
				{
					label: "Inspect it",
					shortcut: shortcuts.get("inspectEvaluation"),
					action: this.inspectEvaluation,
				},
				{
					label: "Debug it",
					shortcut: shortcuts.get("debugExpression"),
					action: this.debugExpression,
				},
				{ label: "Profile it", action: this.profileExpression },
				{ label: "Google it", action: this.searchInGoogle },
				null,
				{
					label: "Browse class",
					shortcut: shortcuts.get("browseClass"),
					action: this.browseClass,
				},
				{
					label: "Browse senders",
					shortcut: shortcuts.get("browseSenders"),
					action: this.browseSenders,
				},
			]
		);
		if (species) {
			options.push({
				label: "Browse local senders",
				action: this.browseLocalSenders,
			});
		}
		options.push({
			label: "Browse implementors",
			shortcut: shortcuts.get("browseImplementors"),
			action: this.browseImplementors,
		});
		if (species) {
			options.push({
				label: "Browse local implementors",
				action: this.browseLocalImplementors,
			});
		}
		options.push(
			...[
				{
					label: "Browse class references",
					shortchut: shortcuts.get("browseClassReferences"),
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
				null,
				{
					label: "Toggle full view",
					shourtcut: shortcuts.get("toggleEditorFullView"),
					action: this.toggleFullView,
				},
			]
		);
		const extended = ide.extensionMenuOptions(
			this.state.extendedOptions,
			this.performExtendedOption
		);
		return options.concat(extended);
	}

	performExtendedOption = async (option) => {
		const selection = this.selectedText();
		if (selection === "") return;
		const range = this.currentSelectionRange();
		const element = {
			className: this.props.class ? this.props.class.name : null,
			selector: this.props.method ? this.props.method.selector : null,
			sourceInterval: { from: range.from, to: range.to },
			sourceCode: selection,
		};
		await ide.performExtendedOption(option, element);
		if (this.props.onExtendedOptionPerform)
			this.props.onExtendedOptionPerform();
	};

	copyToClipboard = () => {
		const text = this.selectedText();
		navigator.clipboard.writeText(text);
	};

	pasteFromClipboard = () => {
		// OUTDATED
		navigator.clipboard.readText().then(
			(text) => {
				this.editor.replaceSelection(text);
			},
			(error) => console.log(error)
		);
	};

	acceptClicked = () => {
		if (this.props.onAccept) this.props.onAccept(this.normalizedSource());
	};

	wordUnderCursor() {
		return this.wordAt(this.currentPosition());
	}

	wordAt(position) {
		const state = this.currentState();
		if (state) {
			const range = state.wordAt(position);
			if (range) return this.textInRange(range);
		}
	}

	targetWord() {
		const selected = this.selectedText();
		if (selected && selected.length > 0) return selected;
		return this.wordUnderCursor();
	}

	currentPosition() {
		const range = this.currentSelectionRange();
		if (range) return range.from;
	}

	async targetSelector() {
		const ast = this.ast();
		if (!this.state.dirty && ast) {
			const range = this.currentSelectionRange();
			const node =
				range && range.from < range.to
					? ast.selectorInRage(range)
					: this.targetAstNode();
			if (node && (node.type === "Selector" || node.type === "Literal"))
				return node.value;
		}
		try {
			let selector;
			const selection = this.selectedText();
			if (selection.length > 0) {
				selector = await ide.backend.selectorInSource(selection);
			} else {
				selector = await ide.backend.selectorInSource(
					this.normalizedSource(),
					this.currentPosition()
				);
			}
			if (selector !== null) return selector;
		} catch (error) {}
		return this.targetWord();
	}

	targetAstNode() {
		const position = this.currentPosition();
		if (position) {
			return this.astNodeAtOffset(position + 1);
		}
	}

	searchInGoogle = () => {
		const url = "https://www.google.com/search?q=" + this.targetWord();
		window.open(url, "_blank").focus();
	};

	browseSenders = async () => {
		const selector = await this.targetSelector();
		this.context.browseSenders(selector);
	};

	browseLocalSenders = async () => {
		const selector = await this.targetSelector();
		this.context.browseLocalSenders(selector, this.props.class.name);
	};

	browseImplementors = async () => {
		const selector = await this.targetSelector();
		this.context.browseImplementors(selector);
	};

	browseLocalImplementors = async () => {
		const selector = await this.targetSelector();
		this.context.browseLocalImplementors(selector, this.props.class.name);
	};

	browseClass = () => {
		const target = this.targetWord();
		target
			? this.context.browseClass(target)
			: this.context.openClassBrowser();
	};

	browseClassReferences = () => {
		this.context.browseClassReferences(this.targetWord());
	};

	browseMethodsMatching = () => {
		this.context.browseMethodsMatching(this.targetWord());
	};

	browseStringReferences = () => {
		this.context.browseStringReferences(this.targetWord());
	};

	selectedExpression() {
		const expression = this.selectedText();
		if (expression.length > 0) return expression;
		const line = this.currentLine();
		if (line) return line.text;
	}

	currentLine() {
		const range = this.currentSelectionRange();
		if (range) return this.currentState().doc.lineAt(range.head);
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
			await this.context.profileExpression(
				expression,
				this.props.context
			);
		} catch (error) {}
	};

	playClicked = async (editor, event) => {
		if (event) event.preventDefault();
		const object = await this.evaluateExpression(
			this.normalizedSource(),
			true
		);
		if (object && this.props.onEvaluate) this.props.onEvaluate(object);
	};

	pauseClicked = async (editor, event) => {
		if (event) event.preventDefault();
		const evaluation = this.state.currentEvaluation;
		if (!evaluation) return;
		try {
			const paused = await ide.backend.pauseEvaluation(evaluation.id);
			this.setState({
				currentEvaluation: { ...evaluation, state: paused.state },
			});
			const d = await ide.backend.createDebugger(evaluation.id);
			this.context.openDebugger(d.id, d.description);
		} catch (error) {
			ide.reportError(error);
		}
	};

	updatePlay = async () => {
		const evaluation = this.state.currentEvaluation;
		if (!evaluation) return;
		try {
			const updated = await ide.backend.evaluation(evaluation.id);
			this.setState({
				currentEvaluation: { ...evaluation, state: updated.state },
			});
		} catch (ignored) {}
	};

	evaluateExpression = async (expression, pin) => {
		this.setState({ evaluating: true });
		if (!expression || expression.length === 0) return;
		let object;
		try {
			const evaluation = {
				expression: expression,
				sync: false,
				pin: pin,
				context: this.props.context,
			};
			let issued = await ide.backend.issueEvaluation(evaluation);
			evaluation.id = issued.id;
			evaluation.state = issued.state;
			this.setState({ currentEvaluation: evaluation });
			object = await this.context.waitForEvaluationResult(evaluation);
			if (!pin) {
				try {
					await ide.backend.unpinObject(object.id);
				} catch (ignored) {}
			}
			this.setState({ evaluating: false, currentEvaluation: null });
		} catch (error) {
			object = null;
			this.context.reportError(error);
			this.setState({ evaluating: false, currentEvaluation: null });
		}
		return object;
	};

	evaluateSelection = async () => {
		const expression = this.selectedExpression();
		await this.evaluateExpression(expression, false);
	};

	showEvaluation = async () => {
		const range = this.currentSelectionRange();
		let position;
		if (range && range.from < range.to) {
			position = Math.max(range.head, range.anchor);
		} else {
			const line = this.currentLine();
			position = line ? line.to : 0;
		}
		const expression = this.selectedExpression();
		const object = await this.evaluateExpression(expression, false);
		if (object) {
			this.insertText(" " + object.printString, position);
			this.selectRanges([
				{
					anchor: position + 1,
					head: position + object.printString.length + 1,
				},
			]);
		}
	};

	inspectEvaluation = async () => {
		const expression = this.selectedExpression();
		const object = await this.evaluateExpression(expression, true);
		if (object) {
			this.context.openInspector(object);
		}
	};

	browseGlobal = async (name) => {
		if (!name) return;
		let species;
		try {
			species = await ide.backend.classNamed(name);
		} catch (error) {}
		if (species) {
			return this.context.browseClass(species.name);
		}
		const object = await this.evaluateExpression(name, true);
		if (object) {
			this.context.openInspector(object);
		}
	};

	annotations = () => {
		if (this.state.dirty || !this.props.annotations) {
			return [];
		}
		return this.props.annotations.map((a) => {
			return {
				from: a.from - 1,
				to: a.to - 1,
				severity: a.type,
				message: a.description,
			};
		});
	};

	setBreakpoint = (n) => {
		// OUTDATED
		var info = this.editor.lineInfo(n);
		this.editor.setGutterMarker(
			n,
			"breakpoints",
			info.gutterMarkers ? null : this.makeMarker()
		);
	};

	makeMarker() {
		// OUTDATED
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

	normalizedSource() {
		return CodeEditor.normalizeNewlines(this.state.source);
	}

	normalizedOriginalSource() {
		return CodeEditor.normalizeNewlines(this.state.originalSource);
	}

	triggerOnChange = (source) => {
		if (this.props.onChange) {
			clearTimeout(this.typingTimer);
			this.typingTimer = setTimeout(() => {
				this.props.onChange(CodeEditor.normalizeNewlines(source));
			}, this.props.changeEventLaps || this.changeEventFrequency);
		}
	};

	sourceChanged = (source) => {
		this.selectsRanges = false;
		if (this.editorView) {
			this.lastSelection = this.editorView.state.selection.main;
		}
		this.setState(
			{
				source: source,
				dirty: true,
			},
			this.triggerOnChange(source)
		);
	};

	editorUpdated = (update) => {
		if (update.transactions.find((t) => t.selection)) {
			this.selectsRanges = false;
		}
	};

	adaptShortcut(shortcut) {
		const parts = shortcut.split("+");
		return parts[0] + "-" + parts[1];
	}

	extraKeys() {
		const shortcuts = ide.settings.section("shortcuts");
		return [
			{
				key: this.adaptShortcut(shortcuts.get("evaluateExpression")),
				run: this.evaluateSelection,
			},
			{
				key: this.adaptShortcut(shortcuts.get("inspectEvaluation")),
				run: this.inspectEvaluation,
			},
			{
				key: this.adaptShortcut(shortcuts.get("showEvaluation")),
				run: this.showEvaluation,
			},
			{
				key: this.adaptShortcut(shortcuts.get("debugExpression")),
				run: this.debugExpression,
			},
			{
				key: this.adaptShortcut(shortcuts.get("acceptCode")),
				run: this.acceptClicked,
				preventDefault: true,
			},
			{
				key: this.adaptShortcut(shortcuts.get("browseClass")),
				run: this.browseClass,
				preventDefault: true,
				stopPropagation: true,
			},
			{
				key: this.adaptShortcut(shortcuts.get("browseSenders")),
				run: this.browseSenders,
			},
			{
				key: this.adaptShortcut(shortcuts.get("browseImplementors")),
				run: this.browseImplementors,
			},
			{
				key: this.adaptShortcut(shortcuts.get("browseClassReferences")),
				run: this.browseClassReferences,
			},
			{ key: "F2", run: this.renameTarget },
			{
				key: "Tab",
				run: acceptCompletion,
			},
			{
				key: this.adaptShortcut(shortcuts.get("toggleEditorFullView")),
				run: this.toggleFullView,
			},
		];
	}

	theme() {
		const { readOnly, fontSize } = this.props;
		const appearance = ide.settings.section("appearance");
		const mode = appearance.section(appearance.get("mode"));
		let background = mode.get("background");
		if (readOnly) background = darken(background, 0.05);
		const styles = [
			"selectorStyle",
			"symbolStyle",
			"argumentStyle",
			"temporaryStyle",
			"assignmentStyle",
			"stringStyle",
			"variableStyle",
			"metaStyle",
			"bracketStyle",
			"selfStyle",
			"superStyle",
			"trueStyle",
			"falseStyle",
			"nilStyle",
			"thisContextStyle",
			"returnStyle",
			"globalStyle",
			"numberStyle",
			"commentStyle",
		];
		const params = {
			theme: appearance.get("mode"),
			settings: {
				fontSize: fontSize,
				fontFamily: appearance.get("fontFamily"),
				background: background,
				foreground: "#75baff",
				caret:
					mode.name === "light" ? "black" : mode.get("primaryColor"),
				selection: mode.get("selectionColor"),
				selectionMatch: "#ef9b9b50",
				lineHighlight: "#8a91991a",
				gutterBackground: background,
				gutterBorder: background,
				gutterForeground: "#8a919966",
			},
			styles: [],
		};
		styles.forEach((s) => {
			const setting = mode.setting(s);
			params.styles.push({
				tag: newTags[s.replace("Style", "")],
				color: setting.color,
				fontStyle: setting.italic ? "italic" : "normal",
				fontWeight: setting.bold ? "bold" : "normal",
			});
		});
		// Check whether this is necessary...
		params.styles.push({
			tag: newTags.var,
			color: mode.setting("variableStyle").color,
			fontStyle: mode.setting("variableStyle").style,
		});
		return createTheme(params);
	}

	defaultTooltipSpecFor = async (word, position) => {
		const node = this.astNodeAtOffset(position);
		if (node && node.type === "Selector" && node.value.includes(word)) {
			return {
				title: node.value,
				titleAction: (s) => this.context.browseImplementors(s),
				actions: [
					{
						label: "Implementors",
						handler: (s) => this.context.browseImplementors(s),
					},
					{
						label: "Senders",
						handler: (s) => this.context.browseSenders(s),
					},
				],
			};
		}
		var species;
		try {
			species = await ide.backend.classNamed(word);
		} catch (ignored) {}
		if (species) {
			const comment = species.comment || "";
			return {
				title: species.name,
				titleAction: (c) => this.context.browseClass(c),
				description: comment.length > 0 ? comment : null,
				code: species.definition,
				actions: [
					{
						label: "Browse",
						handler: (c) => this.context.browseClass(c),
					},
					{
						label: "References",
						handler: (c) => this.context.browseClassReferences(c),
					},
				],
			};
		}
	};

	tooltipSpecAt = async (position) => {
		const word = this.wordAt(position);
		if (!word) return;
		var handler = this.props.onTooltipShow;
		var tip;
		if (handler) tip = await handler(word);
		if (!tip) tip = await this.defaultTooltipSpecFor(word, position);
		if (!tip) return;
		if (typeof tip == "string") {
			tip = {
				title: word,
				description: tip,
				titleAction: this.props.onTooltipClick,
			};
		}
		const max = 400;
		if (tip.description && tip.description.length > max) {
			tip.description = tip.description.substr(0, max - 1) + "…";
		}
		return tip;
	};

	showsTooltip() {
		let show = ide.settings.section("editor").get("tooltips");
		return show && !this.props.noTooltips;
	}

	tooltip() {
		const { theme } = this.props;
		const ref = React.createRef();
		return hoverTooltip(
			async (view, pos, side) => {
				if (!this.showsTooltip()) return null;
				const spec = await this.tooltipSpecAt(pos);
				if (!spec) return null;
				return {
					pos: pos,
					end: pos + spec.title.length - 1,
					above: true,
					arrow: true,
					create: (view) => {
						let container =
							document.getElementById("tooltip-container");
						if (!container) {
							console.error(
								"Tooltip container not found! Creating one..."
							);
							container = document.createElement("div");
							container.id = "tooltip-container";
							document.body.appendChild(container); // Ensure it exists
						}
						const dom = document.createElement("div");
						container.appendChild(dom);
						let root;
						try {
							root = ReactDOM.createRoot(dom);
						} catch (error) {
							console.error(
								"Failed to create React root for tooltip:",
								error
							);
							return { dom }; // Failsafe return
						}
						root.render(
							<ThemeProvider theme={theme}>
								<ToolContainerContext.Provider
									value={this.context}
								>
									<CodeTooltip
										title={spec.title}
										titleAction={spec.titleAction}
										description={spec.description}
										code={spec.code}
										object={spec.object}
										actions={spec.actions}
										inspectorRef={ref}
									/>
								</ToolContainerContext.Provider>
							</ThemeProvider>
						);
						return {
							dom,
							destroy: () => {
								if (root) {
									try {
										if (ref && ref.current)
											ref.current.aboutToClose();
										root.unmount();
									} catch (error) {
										console.warn(
											"Tooltip unmount failed:",
											error
										);
									}
								}
								dom.remove();
							},
						};
					},
				};
			},
			{ hideOnChange: true, hoverTime: 100 }
		);
	}

	explainCode = async () => {
		ide.explainCode(this.normalizedSource());
	};

	testCode = async () => {
		ide.testCode(this.normalizedSource());
	};

	improveCode = async () => {
		ide.improveCode(this.normalizedSource());
	};

	completionSource = async (context) => {
		const { enableAutocompletion } = this.props;
		const enabled =
			enableAutocompletion !== undefined
				? enableAutocompletion
				: ide.settings.section("editor").get("autocompletion");
		if (!enabled) return null;
		const word = context.matchBefore(/[^\s]*/);
		if (!word || word.from === word.to || word.text.trim().length <= 0)
			return null;
		const classname = this.props.class ? this.props.class.name : null;
		let options;
		try {
			options = await ide.backend.autocompletions(
				classname,
				this.normalizedSource(),
				word.to
			);
		} catch (error) {
			return null;
		}
		// Remove perfect matches (this prevents completion menu appear when the target word is completed)
		options = options.filter((o) => o.label !== word.text);
		if (options.length <= 0) return null;
		// Hide descriptions by now
		options.forEach((o) => (o.detail = ""));
		return {
			from: word.from,
			options: options,
			filter: false,
		};
	};

	customCompletionDisplay() {
		return EditorView.updateListener.of(({ view, docChanged }) => {
			if (docChanged) {
				// when a completion is active each keystroke triggers the
				// completion source function, to avoid it we close any open
				// completion inmediatly.
				closeCompletion(view);
				this.delayedStartCompletion(view);
			}
		});
	}

	delayedStartCompletion = (view) => {
		if (!this.state.dirty) {
			return;
		}
		clearTimeout(this.autocompletionTimer);
		this.autocompletionTimer = setTimeout(() => {
			startCompletion(view);
		}, 350);
	};

	includeColonInSelection = () => {
		const range = this.currentSelectionRange();
		if (this.textInRange({ from: range.to, to: range.to + 1 }) === ":") {
			this.selectRanges([
				{
					anchor: range.from,
					head: range.to + 1,
				},
			]);
		}
	};

	lexer() {
		const lexer = SmalltalkLexer(this.props.useMethodLexer);
		lexer.tokenTable = newTags;
		return StreamLanguage.define(lexer);
	}

	toggleFullView = () => {
		if (this.props.onFullViewToggle) this.props.onFullViewToggle();
	};

	effectiveLineNumbers() {
		const preference = this.props.showLineNumbers;
		if (typeof preference === "boolean") return preference;
		return ide.settings.section("editor").get("lineNumbers");
	}

	lineGutter = () => {
		if (this.effectiveLineNumbers()) return lineNumbers();
		return EditorView.theme({
			".cm-gutters": {
				display: "none",
			},
		});
	};

	focusEditor = () => {
		if (this.editorView) {
			this.editorView.focus();
		}
	};

	render() {
		console.log("rendering code editor");
		const {
			source,
			evaluating,
			currentEvaluation,
			dirty,
			menuOpen,
			menuPosition,
		} = this.state;
		const { showAccept, showPlay, showAssistant, readOnly, noScroll } =
			this.props;
		const showCodeAssistant = showAssistant && ide.usesCodeAssistant();
		const showButtons = showAccept || showPlay || showAssistant;
		const menuOptions = this.menuOptions();
		return (
			<Box
				display="flex"
				flexDirection="row"
				style={{ width: "100%", height: "100%" }}
			>
				<Box flexGrow={1}>
					<Scrollable disabled={noScroll}>
						<CodeMirror
							ref={this.editorRef}
							width="100%"
							height="100%"
							extensions={[
								this.lexer(),
								EditorView.lineWrapping,
								this.lineGutter(),
								linter(this.annotations),
								Prec.highest(keymap.of(this.extraKeys())),
								this.tooltip(),
								autocompletion({
									activateOnTyping: false,
									override: [this.completionSource],
								}),
								this.customCompletionDisplay(),
							]}
							theme={this.theme()}
							value={source}
							//selection={EditorSelection.cursor(source.length)}
							onChange={this.sourceChanged}
							onContextMenu={this.openMenu}
							onCreateEditor={(view, state) => {
								this.editorView = view;
							}}
							onUpdate={(update) => this.editorUpdated(update)}
							onDoubleClick={this.includeColonInSelection}
							readOnly={readOnly || evaluating}
							basicSetup={{
								lineNumbers: false, // controlled via extensions
								closeBrackets: true,
								bracketMatching: true,
								highlightActiveLine: false,
								//allowMultipleSelections: true,
								drawSelection: true,
								//autocompletion: true,
							}}
						/>
					</Scrollable>
					<div id="tooltip-container"></div>
					{evaluating && <LinearProgress variant="indeterminate" />}
				</Box>
				{showButtons && (
					<Box
						display="flex"
						flexDirection="column"
						sx={{ height: "100%" }}
					>
						{showAccept && (
							<Box display="flex" justifyContent="center">
								<IconButton
									color="inherit"
									onClick={this.acceptClicked}
								>
									<AcceptIcon
										size="large"
										color={dirty ? "primary" : "inherit"}
										style={{ fontSize: 30 }}
									/>
								</IconButton>
							</Box>
						)}
						{showPlay && !evaluating && (
							<Box display="flex" justifyContent="center">
								<IconButton
									color="inherit"
									onClick={this.playClicked}
								>
									<PlayIcon
										size="large"
										color={dirty ? "primary" : "inherit"}
									/>
								</IconButton>
							</Box>
						)}
						{showPlay && evaluating && (
							<Box display="flex" justifyContent="center">
								<IconButton
									color="inherit"
									onClick={this.pauseClicked}
									disabled={
										!(
											currentEvaluation &&
											["pending", "evaluating"].includes(
												currentEvaluation.state
											)
										)
									}
								>
									<PauseIcon
										size="large"
										color={dirty ? "primary" : "inherit"}
										style={{ fontSize: 30 }}
									/>
								</IconButton>
							</Box>
						)}
						{showCodeAssistant && (
							<Box mb={1} display="flex" flexDirection="column">
								<Tooltip
									title="Explain this code"
									placement="top"
								>
									<IconButton
										color="inherit"
										onClick={this.explainCode}
									>
										<ExplainIcon />
									</IconButton>
								</Tooltip>
								<Tooltip
									title="Write test for this code"
									placement="top"
								>
									<IconButton
										color="inherit"
										onClick={this.testCode}
									>
										<TestIcon />
									</IconButton>
								</Tooltip>
								<Tooltip
									title="Improve this code"
									placement="top"
								>
									<IconButton
										color="inherit"
										onClick={this.improveCode}
									>
										<ImproveIcon />
									</IconButton>
								</Tooltip>
							</Box>
						)}
					</Box>
				)}
				{menuOptions && menuOptions.length > 0 && (
					<PopupMenu
						options={menuOptions}
						open={menuOpen}
						position={menuPosition}
						onClose={this.closeMenu}
					/>
				)}
			</Box>
		);
	}
}

export { CodeEditor };
export default withTheme(CodeEditor);
