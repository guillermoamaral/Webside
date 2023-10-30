import React, { Component } from "react";
import ReactDOM from "react-dom/client";
import {
	Grid,
	Box,
	IconButton,
	LinearProgress,
	SpeedDial,
	SpeedDialAction,
} from "@mui/material";
import AcceptIcon from "@mui/icons-material/CheckCircle";
import PopupMenu from "../controls/PopupMenu";
import { ide } from "../IDE";
import ToolContainerContext from "../ToolContainerContext";
import Scrollable from "../controls/Scrollable";
import CodeMirror from "@uiw/react-codemirror";
import { EditorView, keymap } from "@codemirror/view";
import { linter, lintGutter } from "@codemirror/lint";
//import { material } from "@uiw/codemirror-theme-material";
//import { LRLanguage, LanguageSupport } from "@codemirror/language";
//import { styleTags, tags } from "@lezer/highlight";
import { SmalltalkParser } from "../../SmalltalkParser";
import { Tag } from "@lezer/highlight";
import { createTheme } from "@uiw/codemirror-themes";
import { StreamLanguage } from "@codemirror/language";
//import { EditorSelection, EditorState, Prec } from "@codemirror/state";
import { Prec } from "@codemirror/state";
//import { throwStatement } from "@babel/types";
import {
	autocompletion,
	closeCompletion,
	startCompletion,
} from "@codemirror/autocomplete";
import { hoverTooltip } from "@codemirror/view";
import CodeTooltip from "./CodeTooltip";
import ChatGPTIcon from "../icons/ChatGPTIcon";
import ImproveIcon from "@mui/icons-material/AutoFixHigh";
import TestRunnerIcon from "../icons/TestRunnerIcon";
import DescriptionIcon from "@mui/icons-material/Description";

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

// This code makes use of a CodeMirror 5-like parser and should be replaced in the future by the Lezer grammar option abvove.

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
	return: Tag.define(),
	global: Tag.define(),
	number: Tag.define(),
	comment: Tag.define(),
	separator: Tag.define(),
};
SmalltalkParser.tokenTable = newTags;
const smalltalk = StreamLanguage.define(SmalltalkParser);

class CodeEditor extends Component {
	static contextType = ToolContainerContext;

	constructor(props) {
		super(props);
		this.editorRef = React.createRef();
		this.editorView = null;
		this.selectsRanges = true;
		this.typingTimer = null;
		this.autocompletionTimer = null;
		this.state = {
			originalSource: props.source,
			source: props.source,
			selectedInterval: null,
			dirty: false,
			selectedRanges: [],
			menuOpen: false,
			menuPosition: { x: null, y: null },
			evaluating: false,
			progress: false,
		};
	}

	static getDerivedStateFromProps(props, state) {
		const {
			source,
			selectedInterval,
			selectedSelector,
			selectedIdentifier,
			evaluating,
		} = props;
		if (
			//!state.dirty &&
			source !== state.originalSource ||
			JSON.stringify(selectedInterval) !==
				JSON.stringify(state.selectedInterval) ||
			selectedSelector !== state.selectedSelector ||
			selectedIdentifier !== state.selectedIdentifier
		) {
			const ranges =
				source && selectedInterval
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
				originalSource: source,
				selectedInterval: selectedInterval,
				selectedSelector: selectedSelector,
				selectedIdentifier: selectedIdentifier,
				source: source,
				selectedRanges: ranges,
				evaluating: evaluating,
				dirty: false,
			};
		}
		if (evaluating !== undefined && evaluating !== state.evaluating) {
			return {
				evaluating: evaluating,
			};
		}
		return null;
	}

	shouldComponentUpdate(nextProps, nextState) {
		// if (this.state.dirty) {
		// 	return false;
		// }
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
			nextState.progress !== this.state.progress ||
			nextProps.evaluating !== this.props.evaluating ||
			nextState.evaluating !== this.state.evaluating
		) {
			return true;
		}
		// Review this as responding false none of local setState() calls trigger a re-rendering
		// For instance, openMenu(), which is intended to open the popup menu, never gets a rerendering (and so the menu open)
		return false;
	}

	componentDidUpdate() {
		if (!this.selectsRanges) {
			//const source = this.state.source;
			// this.selectRanges([
			// 	{
			// 		anchor: source.length - 1,
			// 		head: source.length - 1,
			// 	},
			// ]);
			return;
		}
		const { selectedRanges, selectedSelector, selectedIdentifier } =
			this.state;
		if (selectedRanges) {
			this.selectRanges(selectedRanges);
		}
		if (selectedSelector) {
			const ranges = this.rangesContainingSelector(selectedSelector);
			this.selectRanges(ranges);
		}
		if (selectedIdentifier) {
			const ranges = this.rangesContainingIdentifier(selectedIdentifier);
			this.selectRanges(ranges);
		}
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
		return this.state.source.slice(range.from, range.to);
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
				} catch (error) {}
			}, 200 * i);
		}
	}

	inserText(text, position) {
		this.editorView.dispatch({
			changes: { from: position, to: position, insert: text },
		});
	}

	astRangesSatisfying(condition) {
		const ranges = [];
		const ast = this.props.ast;
		if (!ast) {
			return ranges;
		}
		this.traverseAst(ast, (node) => {
			if (condition(node)) {
				ranges.push({ anchor: node.start - 1, head: node.end });
			}
		});
		return ranges;
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
		var replacement;
		try {
			replacement = await ide.prompt({
				title: "Replacement",
				defaultValue: identifier,
			});
		} catch (error) {}
		if (!replacement) {
			return;
		}
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
		this.forceUpdate(); // Check this according to the default response in shouldComponentUpdate()
	};

	closeMenu = () => {
		this.setState({ menuOpen: false });
		this.forceUpdate(); // Check this according to the default response in shouldComponentUpdate()
	};

	menuOptions() {
		const shortcuts = ide.settings.section("shortcuts");
		const options = [
			{ label: "Copy (Ctrl+c)", action: this.copyToClipboard },
			{ label: "Paste (Ctrl+v)", action: this.pasteFromClipboard },
			null,
		];
		if (this.state.source === this.state.originalSource) {
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
		options.push(
			...[
				{
					label:
						"Do it (" + shortcuts.get("evaluateExpression") + ")",
					action: this.evaluateSelection,
				},
				{
					label: "Print it (" + shortcuts.get("showEvaluation") + ")",
					action: this.showEvaluation,
				},
				{
					label:
						"Inspect it (" +
						shortcuts.get("inspectEvaluation") +
						")",
					action: this.inspectEvaluation,
				},
				{
					label:
						"Debug it (" + shortcuts.get("debugExpression") + ")",
					action: this.debugExpression,
				},
				{ label: "Profile it", action: this.profileExpression },
				{ label: "Google it", action: this.searchInGoogle },
				null,
				{
					label:
						"Browse class (" + shortcuts.get("browseClass") + ")",
					action: this.browseClass,
				},
				{
					label:
						"Browse senders (" +
						shortcuts.get("browseSenders") +
						")",
					action: this.browseSenders,
				},
				{
					label:
						"Browse implementors (" +
						shortcuts.get("browseImplementors") +
						")",
					action: this.browseImplementors,
				},
				{
					label:
						"Browse class references (" +
						shortcuts.get("browseClassReferences") +
						")",
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
			]
		);
		return options;
	}

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

	acceptClicked = (editor, event) => {
		if (event) {
			event.preventDefault();
		}
		if (this.props.onAccept) {
			this.props.onAccept(this.state.source);
		}
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
		if (selected.length > 0) {
			return selected;
		}
		return this.wordUnderCursor();
	}

	currentPosition() {
		const range = this.currentSelectionRange();
		if (range) return range.from;
	}

	targetSelector() {
		const selected = this.selectedText();
		if (selected.length > 0) {
			return selected.trim();
		}
		const node = this.targetAstNode();
		if (node && (node.type === "Selector" || node.type === "Literal")) {
			return node.value;
		}
		return this.targetWord();
	}

	targetAstNode() {
		const position = this.currentPosition();
		if (position) {
			return this.astNodeAtOffset(position);
		}
	}

	searchInGoogle = () => {
		const url = "https://www.google.com/search?q=" + this.targetWord();
		window.open(url, "_blank").focus();
	};

	browseSenders = () => {
		this.context.browseSenders(this.targetSelector());
	};

	browseImplementors = () => {
		this.context.browseImplementors(this.targetSelector());
	};

	browseClass = (e, f) => {
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
		if (expression.length > 0) {
			return expression;
		}
		return this.currentLine();
	}

	currentLine() {
		const range = this.currentSelectionRange();
		if (range) {
			return this.currentState().doc.lineAt(range.head).text;
		}
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

	evaluateExpression = async (expression, sync, pin) => {
		var object;
		try {
			this.setState({ progress: true });
			object = await this.context.evaluateExpression(
				expression,
				sync,
				pin,
				this.props.context
			);
			this.setState({ progress: false }, this.triggerOnEvaluate());
		} catch (error) {
			object = null;
			this.setState({ progress: false });
		}
		return object;
	};

	evaluateSelection = async () => {
		const expression = this.selectedExpression();
		await this.evaluateExpression(expression, false, false);
		this.triggerOnEvaluate();
	};

	triggerOnEvaluate() {
		if (this.props.onEvaluate) {
			this.props.onEvaluate();
		}
	}

	showEvaluation = async () => {
		const range = this.currentSelectionRange();
		const expression = this.selectedExpression();
		const object = await this.evaluateExpression(expression, false, false);
		if (object) {
			this.inserText(
				" " + object.printString,
				Math.max(range.head, range.anchor)
			);
		}
	};

	inspectEvaluation = async () => {
		const expression = this.selectedExpression();
		const object = await this.evaluateExpression(expression, false, true);
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
		const object = await this.evaluateExpression(name, false, true);
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

	triggerOnChange = () => {
		if (this.props.onChange) {
			clearTimeout(this.typingTimer);
			this.typingTimer = setTimeout(() => {
				this.props.onChange(this.state.source);
			}, 2000);
		}
	};

	sourceChanged = (source) => {
		const adapted = source.replace(/(?<!\r)\n|\r(?!\n)/g, "\r");
		this.selectsRanges = false;
		const changed = !this.state.dirty;
		this.setState(
			{
				source: adapted,
				dirty: true,
			},
			this.triggerOnChange
		);
		if (changed) {
			//this.forceUpdate();
			// Check this according to the default response in shouldComponentUpdate()
		}
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
			},
			{
				key: this.adaptShortcut(shortcuts.get("browseClass")),
				run: this.browseClass,
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
		];
	}

	theme() {
		const appearance = ide.settings.section("appearance");
		const mode = appearance.section(appearance.get("mode"));
		const colors = mode.section("colors");
		const code = mode.section("code");
		const background = colors.get("background");
		return createTheme({
			theme: appearance.get("mode"),
			settings: {
				//fontFamily: appearance.get("fontfamily"),
				background: background,
				foreground: "#75baff",
				caret: mode === "light" ? "black" : colors.get("primaryColor"),
				selection: mode === "light" ? "blue" : "grey",
				selectionMatch: mode === "light" ? "blue" : "grey",
				lineHighlight: "#8a91991a",
				gutterBackground: background,
				gutterBorder: background,
				gutterForeground: "#8a919966",
			},
			styles: [
				{ tag: newTags.selector, color: code.get("selector") },
				{ tag: newTags.symbol, color: code.get("symbol") },
				{ tag: newTags.argument, color: code.get("argument") },
				{ tag: newTags.temporary, color: code.get("temporary") },
				{ tag: newTags.assignment, color: code.get("assignment") },
				{ tag: newTags.string, color: code.get("string") },
				{ tag: newTags.variable, color: code.get("variable") },
				{ tag: newTags.var, color: code.get("variable") },
				{ tag: newTags.meta, color: code.get("meta") },
				{ tag: newTags.bracket, color: code.get("bracket") },
				{ tag: newTags.reserved, color: code.get("reserved") },
				{ tag: newTags.return, color: code.get("return") },
				{ tag: newTags.global, color: code.get("global") },
				{ tag: newTags.number, color: code.get("number") },
				{
					tag: newTags.comment,
					color: code.get("comment"),
					fontStyle: "italic",
				},
				{ tag: newTags.separator, color: code.get("separator") },
			],
		});
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
		} catch (error) {}
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
		if (handler) {
			tip = await handler(word);
		}
		if (!tip) {
			tip = await this.defaultTooltipSpecFor(word, position);
		}
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

	tooltip() {
		return hoverTooltip(
			async (view, pos, side) => {
				if (this.props.noTooltips) return null;
				const spec = await this.tooltipSpecAt(pos);
				if (!spec) return null;
				return {
					pos: pos,
					end: pos + spec.title.length - 1,
					above: true,
					arrow: true,
					create(view) {
						let dom = document.createElement("div");
						const root = ReactDOM.createRoot(dom);
						root.render(
							<CodeTooltip
								title={spec.title}
								titleAction={spec.titleAction}
								description={spec.description}
								code={spec.code}
								actions={spec.actions}
							/>
						);
						return { dom };
					},
				};
			},
			{ hideOnChange: true, hoverTime: 100 }
		);
	}

	assistantActions() {
		if (!ide.usesCodeAssistant()) return [];
		return [
			{
				icon: <ImproveIcon />,
				name: "Improve",
				handler: this.improveSource,
			},
			{
				icon: <TestRunnerIcon />,
				name: "Suggest a test",
				handler: this.testSource,
			},
			{
				icon: <DescriptionIcon />,
				name: "Explain",
				handler: this.explainSource,
			},
		];
	}

	improveSource = async () => {
		const source = this.state.source;
		const assistant = ide.codeAssistant();
		this.setState({ progress: true });
		const improved = await assistant.improveCode(source);
		this.setState({
			source: source + '\r\r"Improved version"\r' + improved,
			progress: false,
		});
	};

	explainSource = async () => {
		const source = this.state.source;
		const assistant = ide.codeAssistant();
		this.setState({ progress: true });
		const description = await assistant.explainCode(source);
		this.setState({
			source: source + '\r\r"Explanation"\r"' + description + '"',
			progress: false,
		});
	};

	testSource = async () => {
		const source = this.state.source;
		const assistant = ide.codeAssistant();
		this.setState({ progress: true });
		const test = await assistant.testCode(source);
		this.setState({
			source: source + '\r\r"Possible unit test"\r' + test,
			progress: false,
		});
	};

	completionSource = async (context) => {
		if (!ide.settings.section("code").get("autocompletion")) return null;
		const word = context.matchBefore(/[^\s]*/);
		if (!word || word.from === word.to || word.text.trim().length <= 0)
			return null;
		const classname = this.props.class ? this.props.class.name : null;
		var options;
		try {
			options = await ide.backend.autocompletions(
				classname,
				this.state.source,
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
		if (this.textInRange({ from: range.to, to: range.to + 1 }) == ":") {
			this.selectRanges([
				{
					anchor: range.from,
					head: range.to + 1,
				},
			]);
		}
	};

	render() {
		console.log("rendering code editor");
		const { source, evaluating, progress, dirty, menuOpen, menuPosition } =
			this.state;
		const { showAccept, showAssistant, readOnly } = this.props;
		const showCodeAssistant = showAssistant && ide.usesCodeAssistant();
		const showButtons = showAccept || showAssistant;
		const lineNumbers = this.props.lineNumbers === true;
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
			<Grid container spacing={0} style={{ height: "100%" }}>
				<Grid
					item
					xs={11}
					md={showAccept ? 11 : 12}
					lg={showAccept ? 11 : 12}
				>
					<Scrollable>
						<CodeMirror
							ref={this.editorRef}
							width="100%"
							height="100%"
							extensions={[
								smalltalk,
								EditorView.lineWrapping,
								//EditorState.lineSeparator.of("\r"),
								lintGutter(),
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
							onContextMenu={(event) => {
								this.openMenu(event);
							}}
							onCreateEditor={(view, state) => {
								this.editorView = view;
							}}
							onUpdate={(update) => this.editorUpdated(update)}
							onDoubleClick={this.includeColonInSelection}
							readOnly={readOnly || evaluating || progress}
							basicSetup={{
								lineNumbers: lineNumbers,
								closeBrackets: true,
								bracketMatching: true,
								highlightActiveLine: false,
								//allowMultipleSelections: true,
								drawSelection: true,
								//autocompletion: true,
							}}
						/>
					</Scrollable>
					{(evaluating || progress) && (
						<LinearProgress variant="indeterminate" />
					)}
				</Grid>
				{showButtons && (
					<Grid item xs={1} md={1} lg={1}>
						<Box
							display="flex"
							justifyContent="space-between"
							flexDirection="column"
							sx={{ height: "100%" }}
						>
							{showAccept && (
								<Box display="flex" justifyContent="center">
									<IconButton
										color="inherit"
										onClick={this.acceptClicked}
									>
										{acceptIcon}
									</IconButton>
								</Box>
							)}
							{showCodeAssistant && (
								<Box mb={1}>
									<SpeedDial
										ariaLabel="CodeAssistant actions"
										icon={<ChatGPTIcon />}
										direction="up"
										FabProps={{
											color: "inherited",
											size: "small",
										}}
									>
										{this.assistantActions().map(
											(action) => (
												<SpeedDialAction
													key={action.name}
													icon={action.icon}
													tooltipTitle={action.name}
													onClick={action.handler}
												/>
											)
										)}
									</SpeedDial>
								</Box>
							)}
						</Box>
					</Grid>
				)}
				<PopupMenu
					options={this.menuOptions()}
					open={menuOpen}
					position={menuPosition}
					onClose={this.closeMenu}
				/>
			</Grid>
		);
	}
}

export default CodeEditor;

export { smalltalk };
