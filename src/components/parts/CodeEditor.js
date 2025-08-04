import { Component } from "react";
import ReactDOM from "react-dom/client";
import { ide } from "../IDE";
import ToolContainerContext from "../ToolContainerContext";
import StAST from "../../model/StAST";
import { DndProvider } from "react-dnd";
import { HTML5Backend } from "react-dnd-html5-backend";
import CodeTooltip from "./CodeTooltip";
import { ThemeProvider } from "@mui/material/styles";

const ShowDebugInfo = true;

class CodeEditor extends Component {
	static contextType = ToolContainerContext;

	constructor(props) {
		super(props);
		this.editor = null;
		this.typingTimer = null;
		this.changeEventFrequency = 500; // miliseconds
		this.autocompletionTimer = null;
		this.selectsRanges = true;
		this.state = {
			originalSource: props.originalSource ?? props.source,
			source: props.source,
			selectedInterval: null,
			dirty: false,
			showAnnotations: true,
			menuOpen: false,
			menuPosition: { x: null, y: null },
			evaluating: false,
			extendedOptions: [],
			currentEvaluation: null,
		};
	}

	// Configuration

	settings() {
		return this.props.settings ? this.props.settings : ide.settings;
	}

	showLineNumbers() {
		const preference = this.props.showLineNumbers;
		if (typeof preference === "boolean") return preference;
		return this.settings().section("editor").get("showLineNumbers");
	}

	async initializeExtendedOptions() {
		let extensions = await ide.fetchExtendedOptions("code");
		this.setState({ extendedOptions: extensions });
	}

	// Menues and shortcuts

	menuOptions() {
		const shortcuts = this.settings().section("shortcuts");
		const options = [
			{
				label: "Save",
				shortcut: shortcuts.get("acceptCode"),
				action: this.acceptSource,
			},
			null,
			{ label: "Copy", shortcut: "Ctrl+c", action: this.copyToClipboard },
			{
				label: "Paste",
				shortcut: "Ctrl+v",
				action: this.pasteFromClipboard,
			},
			null,
		];
		if (ShowDebugInfo) {
			options.push({
				label: "Show debug info",
				shortcut: "Ctrl+k",
				action: this.showDebugInfo,
			});
		}
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

	shortcuts = () => {
		return this.menuOptions().filter((option) => option && option.shortcut);
	};

	// Source access and manipulation

	wordUnderCursor() {
		const position = this.currentPosition();
		if (!position) return;
		return this.wordAtPosition(position);
	}

	targetWord() {
		const selected = this.selectedText();
		if (selected && selected.length > 0) return selected;
		return this.wordUnderCursor();
	}

	selectedExpression() {
		const expression = this.selectedText();
		if (expression.length > 0) return expression;
		return this.currentLine();
	}

	textInRange(range) {
		let source = this.normalizedSource();
		if (source) return source.slice(range.from, range.to);
	}

	async targetSelector() {
		const ast = this.ast();
		if (!this.state.dirty && ast) {
			const range = this.currentSelectionRange();
			const node =
				range && range.from < range.to
					? ast.selectorInRage(range.from, range.to)
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

	source() {
		return this.state.source || "";
	}

	normalizedSource(source = this.source()) {
		return CodeEditor.normalizeNewlines(source);
	}

	normalizedOriginalSource() {
		return CodeEditor.normalizeNewlines(this.state.originalSource);
	}

	static normalizeNewlines(source = "") {
		return source.replace(/\r?\n/g, "\r");
	}

	includeColonInSelection = () => {
		const range = this.currentSelectionRange();
		if (this.textInRange({ from: range.to, to: range.to + 1 }) === ":") {
			this.selectRanges([
				{
					from: range.from,
					to: range.to + 1,
				},
			]);
		}
	};

	selectIntervals(intervals) {
		const source = this.normalizedSource();
		if (!source || intervals.length === 0) return;
		const ranges = intervals
			.filter(
				(interval) =>
					interval.start >= 1 && interval.end <= source.length
			)
			.map((interval) => {
				return {
					from: interval.start - 1,
					to: interval.end,
				};
			});
		this.selectRanges(ranges);
	}

	selectInterval(interval) {
		this.selectIntervals([interval]);
	}

	selectSelector(selector) {
		if (!selector) return;
		const intervals = this.astIntervalsWithSelector(selector);
		this.selectIntervals(intervals);
	}

	selectIdentifier(identifier) {
		if (!identifier) return;
		const intervals = this.astIntervalsWithIdentifier(identifier);
		this.selectIntervals(intervals);
	}

	// Event handlers

	async showDebugInfo() {
		console.log("Current source:", this.normalizedSource());
		console.log("Current position:", this.currentPosition());
		console.log("Word under cursor:", this.wordUnderCursor());
		console.log("Selected text:", this.selectedText());
		console.log("Target selector:", await this.targetSelector());
		console.log("Target node:", this.targetAstNode());
		console.log("Target node value:", this.targetAstNode()?.value);
		console.log("Current selection range:", this.currentSelectionRange());
		console.log("Current line range:", this.currentLineRange());
		console.log("Current line:", this.currentLine());
		console.log("Target word:", this.targetWord());
		console.log("Selected expression:", this.selectedExpression());
	}

	triggerOnChange(source) {
		if (this.props.onChange) {
			clearTimeout(this.typingTimer);
			this.typingTimer = setTimeout(() => {
				this.props.onChange(CodeEditor.normalizeNewlines(source));
			}, this.props.changeEventLaps || this.changeEventFrequency);
		}
	}

	sourceChanged(source) {
		this.setState(
			{
				source,
				dirty: true,
				showAnnotations: false,
			},
			() => this.triggerOnChange(source)
		);
	}

	acceptSource() {
		if (this.props.onAccept) this.props.onAccept(this.normalizedSource());
		this.setState({ showAnnotations: true });
	}

	openMenu = (event) => {
		event.preventDefault();
		event.stopPropagation();
		this.setState({
			menuOpen: true,
			menuPosition: { x: event.clientX - 2, y: event.clientY - 4 },
		});
	};

	menuOptionClicked = (option) => {
		if (option?.action) option.action.call(this);
	};

	closeMenu = () => {
		this.setState({ menuOpen: false });
	};

	async browseClass(name) {
		if (name === undefined) name = this.targetWord();
		name ? this.context.browseClass(name) : this.context.openClassBrowser();
	}

	async browseSenders(selector) {
		if (selector === undefined) selector = await this.targetSelector();
		this.context.browseSenders(selector);
	}

	async browseLocalSenders(selector) {
		if (selector === undefined) selector = await this.targetSelector();
		this.context.browseLocalSenders(selector, this.props.class.name);
	}

	async browseImplementors(selector) {
		if (selector === undefined) selector = await this.targetSelector();
		this.context.browseImplementors(selector);
	}

	async browseLocalImplementors(selector) {
		if (selector === undefined) selector = await this.targetSelector();
		this.context.browseLocalImplementors(selector, this.props.class.name);
	}

	async browseClassReferences(name) {
		if (name === undefined) name = this.targetWord();
		this.context.browseClassReferences(name);
	}

	async browseMethodsMatching(text) {
		if (text === undefined) text = this.targetWord();
		this.context.browseMethodsMatching(text);
	}

	async browseStringReferences(text) {
		if (text === undefined) text = this.targetWord();
		this.context.browseStringReferences(text);
	}

	searchInGoogle(text) {
		if (text === undefined) text = this.targetWord();
		const url = "https://www.google.com/search?q=" + text;
		window.open(url, "_blank").focus();
	}

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

	explainCode = async () => {
		const source = this.normalizedSource();
		ide.explainCode(source);
	};

	testCode = async () => {
		const source = this.normalizedSource();
		ide.testCode(source);
	};

	improveCode = async () => {
		const source = this.normalizedSource();
		ide.improveCode(source);
	};

	toggleFullView() {
		if (this.props.onFullViewToggle) this.props.onFullViewToggle();
	}

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

	async evaluateSelection() {
		const expression = this.selectedExpression();
		await this.evaluateExpression(expression, false);
	}

	async showEvaluation() {
		const range = this.currentSelectionRange();
		let position;
		if (range && range.from < range.to) {
			position = Math.max(range.from, range.to);
		} else {
			const range = this.currentLineRange();
			position = range ? range.to : 0;
		}
		const expression = this.selectedExpression();
		const object = await this.evaluateExpression(expression, false);
		if (object) {
			this.insertText(" " + object.printString, position);
			this.selectRanges([
				{
					from: position + 1,
					to: position + object.printString.length + 1,
				},
			]);
		}
	}

	async inspectEvaluation() {
		const expression = this.selectedExpression();
		const object = await this.evaluateExpression(expression, true);
		if (object) {
			this.context.openInspector(object);
		}
	}

	async debugExpression() {
		const expression = this.selectedExpression();
		try {
			await this.context.debugExpression(expression, this.props.context);
		} catch (error) {}
	}

	async profileExpression() {
		const expression = this.selectedExpression();
		try {
			await this.context.profileExpression(
				expression,
				this.props.context
			);
		} catch (error) {}
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

	copyToClipboard() {
		const text = this.selectedText();
		navigator.clipboard.writeText(text);
	}

	pasteFromClipboard() {
		navigator.clipboard.readText().then(
			(text) => this.replaceSelectionWith(text),
			(error) => console.log(error)
		);
	}

	playClicked = async (event) => {
		if (event) event.preventDefault();
		const object = await this.evaluateExpression(
			this.normalizedSource(),
			true
		);
		if (object && this.props.onEvaluate) this.props.onEvaluate(object);
	};

	pauseClicked = async (event) => {
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

	// AST manipulation...

	ast() {
		const json = this.props.ast;
		if (!json) return;
		const ast = new StAST();
		ast.fromJson(json);
		return ast;
	}

	astIntervalsSatisfying(condition) {
		const ast = this.ast();
		if (!ast) return [];
		return ast.nodesSatisfying(condition).map((node) => {
			return { start: node.start, end: node.end };
		});
	}

	astIntervalsWithSelector(selector) {
		const ranges = [];
		const ast = this.props.ast;
		if (!ast) {
			// We could search for identifiers in the source code
			// using some smart mechanism
			return ranges;
		}
		return this.astIntervalsSatisfying(
			(node) =>
				(node.type === "Selector" || node.type === "Literal") &&
				node.value === selector
		);
	}

	astIntervalsWithIdentifier(identifier) {
		const ranges = [];
		const ast = this.props.ast;
		if (!ast) {
			// We could search for identifiers in the source code
			// using some smart mechanism
			return ranges;
		}
		return this.astIntervalsSatisfying(
			(node) => node.type === "Identifier" && node.value === identifier
		);
	}

	targetAstNode() {
		const position = this.currentPosition();
		if (position) return this.astNodeAtOffset(position + 1);
	}

	astNodeAtOffset(offset) {
		const ast = this.ast();
		if (!ast) return;
		return ast.nodeAt(offset);
	}

	// Autocompletion

	usesAutocompletion() {
		const { enableAutocompletion } = this.props;
		return enableAutocompletion !== undefined
			? enableAutocompletion
			: this.settings().section("editor").get("useAutocompletion");
	}

	async getCompletions(source, position) {
		const classname =
			this.props.class && this.isInMethod()
				? this.props.class.name
				: null;
		let completions = [];
		try {
			completions = await ide.backend.autocompletions(
				classname,
				source,
				position
			);
		} catch (ignored) {}
		return completions;
	}

	// Tooltips

	showsTooltip() {
		let show = this.settings().section("editor").get("showTooltips");
		return show && !this.props.noTooltips;
	}

	tooltipSpec = async (position) => {
		const word = this.wordAtPosition(position);
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

	defaultTooltipSpecFor = async (word, position) => {
		const node = this.astNodeAtOffset(position);
		if (node && node.type === "Selector" && node.value.includes(word)) {
			return {
				title: node.value,
				titleAction: (s) => this.browseImplementors(s),
				actions: [
					{
						label: "Implementors",
						handler: (s) => this.browseImplementors(s),
					},
					{
						label: "Senders",
						handler: (s) => this.browseSenders(s),
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
				titleAction: (c) => this.browseClass(c),
				description: comment.length > 0 ? comment : null,
				code: species.definition,
				actions: [
					{
						label: "Browse",
						handler: (c) => this.browseClass(c),
					},
					{
						label: "References",
						handler: (c) => this.browseClassReferences(c),
					},
				],
			};
		}
	};

	ensureTooltipContainer() {
		let container = document.getElementById("tooltip-container");
		if (!container) {
			console.warn("Tooltip container not found! Creating one...");
			container = document.createElement("div");
			container.id = "tooltip-container";
			document.body.appendChild(container); // Ensure it exists
		}
		return container;
	}

	isInMethod() {
		return this.props.inMethod;
	}

	renderTooltip(spec, dom, inspectorRef) {
		const root = ReactDOM.createRoot(dom);
		root.render(
			<DndProvider backend={HTML5Backend}>
				<ThemeProvider theme={this.props.theme}>
					<ToolContainerContext.Provider value={this.context}>
						<CodeTooltip
							title={spec.title}
							titleAction={spec.titleAction}
							description={spec.description}
							code={spec.code}
							object={spec.object}
							actions={spec.actions}
							inspectorRef={inspectorRef}
						/>
					</ToolContainerContext.Provider>
				</ThemeProvider>
			</DndProvider>
		);
		return root;
	}
}

export default CodeEditor;
