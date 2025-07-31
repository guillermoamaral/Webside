import React from "react";
import ReactDOM from "react-dom/client";
import { Box, IconButton, LinearProgress, Tooltip } from "@mui/material";
import AcceptIcon from "@mui/icons-material/CheckCircle";
import PlayIcon from "@mui/icons-material/PlayArrow";
import PauseIcon from "@mui/icons-material/Pause";
import ImproveIcon from "@mui/icons-material/AutoFixHigh";
import TestIcon from "../icons/TestRunnerIcon";
import ExplainIcon from "@mui/icons-material/QuestionMark";
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
import { tokenTypes } from "../../SmalltalkTokenizer";
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
import { ThemeProvider } from "@mui/material/styles";
import { withTheme } from "@emotion/react";
import { darken } from "@mui/system";
import CodeEditor from "./CodeEditor";

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

const lezerTags = {};
tokenTypes.forEach((type) => (lezerTags[type] = Tag.define()));
lezerTags.var = Tag.define();

class CodeMirrorEditor extends CodeEditor {
	static contextType = ToolContainerContext;

	constructor(props) {
		super(props);
		this.state = {
			source: props.source,
			originalSource: props.originalSource ?? props.source,
			dirty: false,
			selectedInterval: null,
			selectedIdentifier: null,
			selectedSelector: null,
			menuOpen: false,
			menuPosition: { x: null, y: null },
			evaluating: false,
			extendedOptions: [],
			currentEvaluation: null,
			showAnnotations: true,
		};
		this.lastSelection = null;
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
			return {
				originalSource: originalSource ?? source,
				source: source,
				selectedInterval: selectedInterval,
				selectedSelector: selectedSelector,
				selectedIdentifier: selectedIdentifier,
				dirty: false,
				showAnnotations: true,
			};
		}
		return null;
	}

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
				nextProps.readOnly !== this.props.readOnly) ||
			nextState.showAnnotations !== this.state.showAnnotations
		) {
			return true;
		}
		// Review this as responding false none of local setState() calls trigger a re-rendering
		// For instance, openMenu(), which is intended to open the popup menu, never gets a rerendering (and so the menu open)
		return false;
	}

	componentDidUpdate() {
		if (this.state.dirty) return;
		if (!this.selectsRanges || !this.editor) return;
		const { selectedInterval, selectedSelector, selectedIdentifier } =
			this.state;
		if (selectedInterval) this.selectInterval(selectedInterval);
		if (selectedSelector) this.selectSelector(selectedSelector);
		if (selectedIdentifier) this.selectIdentifier(selectedIdentifier);
		const limit = this.editor.state.doc.length;
		const range = this.lastSelection;
		if (
			range &&
			range.anchor >= 0 &&
			range.head >= 0 &&
			range.anchor <= limit &&
			range.head <= limit
		) {
			this.editor.dispatch({
				selection: range,
				scrollIntoView: true,
			});
			this.lastSelection = null;
		}
	}

	componentDidMount() {
		this.initializeExtendedOptions();
		ide.onColorModeChange(this.colorModeChanged);
	}

	componentWillUnmount() {
		ide.removeColorModeChangeHandler(this.colorModeChanged);
	}

	// Configuration

	lexer() {
		const lexer = SmalltalkLexer(this.isInMethod());
		lexer.tokenTable = lezerTags;
		return StreamLanguage.define(lexer);
	}

	lineGutter = () => {
		if (this.showLineNumbers()) return lineNumbers();
		return EditorView.theme({
			".cm-gutters": {
				display: "none",
			},
		});
	};

	currentState() {
		if (this.editor && this.editor.viewState) {
			return this.editor.viewState.state;
		}
	}

	adaptShortcut(shortcut) {
		const parts = shortcut.split("+");
		return parts[0] + "-" + parts[1];
	}

	extraKeys() {
		const extrakeys = this.shortcuts().map((shorcut) => {
			return {
				key: this.adaptShortcut(shorcut.shortcut),
				run: () => {
					shorcut.action.call(this);
				},
				preventDefault: true,
				stopPropagation: true,
			};
		});
		extrakeys.push({ key: "F2", run: this.renameTarget });
		extrakeys.push({
			key: "Tab",
			run: acceptCompletion,
		});
		return extrakeys;
	}

	theme() {
		const { readOnly, fontSize } = this.props;
		const appearance = this.settings().section("appearance");
		const mode = appearance.section(appearance.get("mode"));
		let background = mode.get("background");
		if (readOnly) background = darken(background, 0.05);
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
		tokenTypes.forEach((type) => {
			const setting = mode.setting(type + "Style");
			params.styles.push({
				tag: lezerTags[type],
				color: setting.color,
				fontStyle: setting.italic ? "italic" : "normal",
				fontWeight: setting.bold ? "bold" : "normal",
			});
		});
		const setting = mode.setting("variableStyle");
		params.styles.push({
			tag: lezerTags.var,
			color: setting.color,
			fontStyle: setting.italic ? "italic" : "normal",
			fontWeight: setting.bold ? "bold" : "normal",
		});
		return createTheme(params);
	}

	// Source access and manipulation

	currentPosition() {
		const range = this.currentSelectionRange();
		if (range) return range.from;
	}

	wordAtPosition(position) {
		const state = this.currentState();
		if (state) {
			const range = state.wordAt(position);
			if (range) return this.textInRange(range);
		}
	}

	selectedText() {
		const range = this.currentSelectionRange();
		return range ? this.textInRange(range) : "";
	}

	currentLine() {
		const range = this.currentSelectionRange();
		if (!range) return;
		const state = this.currentState();
		if (!state) return;
		const line = state.doc.lineAt(range.head);
		if (line) return line.text;
	}

	currentLineRange() {
		const range = this.currentSelectionRange();
		if (!range) return;
		const state = this.currentState();
		if (!state) return;
		const line = state.doc.lineAt(range.head);
		return { from: line.from, to: line.to };
	}

	currentSelectionRange() {
		const state = this.currentState();
		if (state && state.selection) return state.selection.main;
	}

	selectRanges(ranges) {
		if (!ranges || ranges.length === 0) return;
		// This hack sucks!
		// It was the only way to set selection after a rendering: the value changes but editorView
		// has a delay in updating its state, and so the first attempt to set the selection might fail
		// (as the new selection might surpass the previous value boundaries)
		for (let i = 0; i < 2; i++) {
			setTimeout(() => {
				try {
					this.editor?.dispatch({
						selection: {
							anchor: ranges[0].from,
							head: ranges[0].to,
						},
						scrollIntoView: true,
					});
				} catch (ignored) {}
			}, 200 * i);
		}
	}

	insertText(text, position) {
		this.editor.dispatch({
			changes: { from: position, to: position, insert: text },
		});
	}

	replaceSelectionWith = (text) => {
		if (!this.editor) return;
		const range = this.currentSelectionRange();
		const state = this.currentState();
		const limit = state.doc.length;
		const from = Math.max(0, Math.min(range.from, limit));
		const to = Math.max(0, Math.min(range.to, limit));
		this.editor.dispatch({
			changes: {
				from,
				to,
				insert: text,
			},
			selection: {
				anchor: from + text.length,
			},
		});
		this.editor.focus(); // Check if this is needed
	};

	// Event handlers

	colorModeChanged = () => {
		this.forceUpdate();
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

	sourceChanged(source) {
		this.selectsRanges = false;
		if (this.editor) this.lastSelection = this.editor.state.selection.main;
		super.sourceChanged(source);
	}

	editorUpdated = (update) => {
		if (update.transactions.find((t) => t.selection)) {
			this.selectsRanges = false;
		}
	};

	focusEditor = () => {
		if (this.editor) this.editor.focus();
	};

	renameIdentifier = async (identifier) => {
		const replacement = await ide.prompt({
			title: "Replacement",
			defaultValue: identifier,
		});
		if (!replacement) return;
		const changes = this.astIntervalsWithIdentifier(identifier).map(
			(interval) => {
				return {
					from: interval.start,
					to: interval.end,
					insert: replacement,
				};
			}
		);
		this.editor.dispatch({
			changes: changes,
		});
	};

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

	// Autocompletion and tooltips

	completionResult = async (context) => {
		if (!this.usesAutocompletion()) return null;
		const word = context.matchBefore(/[^\s]*/);
		if (!word || word.from === word.to || word.text.trim().length <= 0)
			return null;
		let completions = await this.getCompletions(
			this.normalizedSource(),
			word.to
		);
		// Remove perfect matches (this prevents completion menu appear when the target word is completed)
		completions = completions.filter((o) => o.label !== word.text);
		if (completions.length <= 0) return null;
		// Hide descriptions by now
		completions.forEach((o) => (o.detail = ""));
		return {
			from: word.from,
			options: completions,
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
		if (!this.state.dirty) return;
		clearTimeout(this.autocompletionTimer);
		this.autocompletionTimer = setTimeout(() => {
			startCompletion(view);
		}, 350);
	};

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

	// Linting annotations

	annotations = () => {
		if (!this.state.showAnnotations || !this.props.annotations) return [];
		return this.props.annotations.map((a) => {
			return {
				from: a.from - 1,
				to: a.to - 1,
				severity: a.type,
				message: a.description,
			};
		});
	};

	// Rendering

	render() {
		console.log("rendering code mirror editor");
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
									override: [this.completionResult],
								}),
								this.customCompletionDisplay(),
							]}
							theme={this.theme()}
							value={source}
							//selection={EditorSelection.cursor(source.length)}
							onChange={(value) => this.sourceChanged(value)}
							onContextMenu={this.openMenu}
							onCreateEditor={(view, state) => {
								this.editor = view;
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
									onClick={() => this.acceptSource()}
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
						onOptionClick={this.menuOptionClicked}
					/>
				)}
			</Box>
		);
	}
}

export { CodeMirrorEditor };
export default withTheme(CodeMirrorEditor);
