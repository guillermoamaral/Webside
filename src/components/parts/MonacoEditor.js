import React from "react";
import { Box, IconButton, LinearProgress, Tooltip } from "@mui/material";
import AcceptIcon from "@mui/icons-material/CheckCircle";
import PlayIcon from "@mui/icons-material/PlayArrow";
import PauseIcon from "@mui/icons-material/Pause";
import ImproveIcon from "@mui/icons-material/AutoFixHigh";
import TestIcon from "../icons/TestRunnerIcon";
import ExplainIcon from "@mui/icons-material/QuestionMark";
import PopupMenu from "../controls/PopupMenu";
import * as monaco from "monaco-editor";
import ToolContainerContext from "../ToolContainerContext";
import { smalltalkMonarchDefinition } from "../../SmalltalkMonarch";
import { ide } from "../IDE";
import { darken } from "@mui/system";
import CodeEditor from "./CodeEditor";
import { tokenize, tokenTypes } from "../../SmalltalkTokenizer";

let smalltalkRegistered = false;

class MonacoEditor extends CodeEditor {
	static contextType = ToolContainerContext;
	static openedInstances = new Map();
	static activeEditor = null;

	constructor(props) {
		super(props);
		this.containerRef = React.createRef();
		this.editor = null;
		this.decorations = [];
		this.hoverDecoration = [];
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
	}

	componentDidMount() {
		ide.onColorModeChange(this.colorModeChanged);
		this.defineTheme();
		this.initializeEditor();
		this.injectStyles();
		this.addCommands(this.editor);
		this.setDecorations(this.editor);
		this.updateAnnotations(this.editor);
	}

	componentDidUpdate(prevProps) {
		console.log(
			"MonacoEditor componentDidUpdate",
			this.props.source,
			prevProps.source,
			this.state.source
		);
		if (
			prevProps.source !== this.props.source &&
			this.props.source !== this.state.source
		) {
			this.setState({ source: this.props.source }, () => {
				if (this.editor) {
					this.editor.setValue(this.state.source);
					this.setDecorations(this.editor);
					this.updateAnnotations(this.editor);
				}
			});
		}
		if (this.editor) {
			this.editor.layout();
			this.editor.focus();
		}
	}

	componentWillUnmount() {
		this.clearHoverDecoration(this.editor);
		this.editor?.dispose();
		this.editor = null;
		ide.removeColorModeChangeHandler(this.colorModeChanged);
		this.resizeObserver?.disconnect();
	}

	static getActiveEditor() {
		return MonacoEditor.activeEditor;
	}

	static setActiveEditor(editor) {
		MonacoEditor.activeEditor = editor;
	}

	static getInstanceForEditor(editor) {
		const model = editor.getModel();
		if (!model) return null;
		return MonacoEditor.openedInstances.get(model.uri.toString());
	}

	// Configuration

	initializeEditor() {
		this.registerLanguage();
		this.createEditor(this.containerRef.current);
		this.setupEditor(this.editor);
		this.registerEditor(this.editor);
	}

	registerLanguage = () => {
		if (!smalltalkRegistered) {
			monaco.languages.register({ id: "smalltalk" });
			monaco.languages.setMonarchTokensProvider(
				"smalltalk",
				smalltalkMonarchDefinition
			);
			monaco.languages.registerCompletionItemProvider("smalltalk", {
				triggerCharacters: [".", " "],
				provideCompletionItems: async (model, position) => {
					const instance = MonacoEditor.openedInstances.get(
						model.uri.toString()
					);
					if (!instance) return { suggestions: [] };
					return instance.completionList(model, position);
				},
			});
			smalltalkRegistered = true;
		}
	};

	editorOptions() {
		const appearance = this.settings().section("appearance");
		return {
			readOnly: this.props.readOnly || false,
			theme: "webside",
			contextmenu: false,
			fontFamily: appearance.get("fontFamily"),
			fontSize: appearance.get("fontSize"),
			lineNumbers: this.showLineNumbers?.() ? "on" : "off",
			minimap: { enabled: false },
			scrollBeyondLastLine: false,
			//glyphMargin: true,
			renderValidationDecorations: "on",
		};
	}

	createEditor = (container) => {
		this.editor = monaco.editor.create(container, {
			...this.editorOptions(),
			value: this.state.source,
			language: "smalltalk",
		});
		this.registerEditor(this.editor);
		MonacoEditor.setActiveEditor(this.editor);
	};

	setupEditor(editor) {
		editor.onDidFocusEditorText(() => MonacoEditor.setActiveEditor(editor));
		editor.onDidChangeModelContent(() => {
			const value = editor.getValue();
			this.sourceChanged(value);
			this.setDecorations(editor);
			this.updateAnnotations(editor);
		});
		editor.onMouseMove((event) => {
			MonacoEditor.setActiveEditor(editor);
			this.mouseMoved(event, editor);
		});
		editor.onMouseDown((event) => {
			editor?.focus();
			MonacoEditor.setActiveEditor(editor);
			this.mouseClicked(event, editor);
		});
		editor.onContextMenu((event) => {
			editor?.focus();
			if (!event.target?.position) return;
			MonacoEditor.setActiveEditor(editor);
			this.openMenu(event.event.browserEvent);
		});
		this.resizeObserver = new ResizeObserver(() => {
			requestAnimationFrame(() => {
				editor?.focus();
				editor?.layout();
			});
		});
		this.resizeObserver.observe(this.containerRef.current);
	}

	registerEditor(editor) {
		const model = editor?.getModel();
		if (model) MonacoEditor.openedInstances.set(model.uri.toString(), this);
	}

	defineTheme() {
		const appearance = this.settings().section("appearance");
		const mode = appearance.section(appearance.get("mode"));
		let background = mode.get("background");
		//if (this.props.readOnly) background = darken(background, 0.5);
		const foreground = mode.get("primaryColor");
		const border = darken(background, 0.2);
		const selection = mode.get("selectionColor");
		const text = mode.get("primaryText");
		monaco.editor.defineTheme("webside", {
			base: "vs-dark",
			inherit: false,
			rules: tokenTypes
				.filter((type) => {
					mode.get(`${type}Color`);
				})
				.map((type) => ({
					token: type,
					foreground: mode.get(`${type}Color`).replace("#", ""),
				})),
			semanticHighlighting: true,
			colors: {
				"editor.background": background,
				"editor.foreground": foreground,
				"editorLineNumber.foreground": darken(foreground, 0.3),
				"editorLineNumber.activeForeground": foreground,
				"editor.selectionBackground": selection,
				"editor.inactiveSelectionBackground": darken(selection, 0.2),
				"editorWidget.background": background,
				"editorWidget.border": border,
				"input.background": background,
				"input.foreground": foreground,
				"input.border": border,
				focusBorder: background,
				"editor.findMatchBackground": "#ffff0077",
				"editor.findMatchHighlightBackground": "#ffff0044",
				"scrollbarSlider.background": "#ffffff22",
				"scrollbarSlider.hoverBackground": "#ffffff33",
				"scrollbarSlider.activeBackground": "#ffffff44",
				"editorSuggestWidget.border": border,
				"editorSuggestWidget.foreground": text,
				"editorSuggestWidget.selectedForeground": text,
				"widget.border": border,
				"editorMarkerNavigationError.background": border,
				"editorError.border": border,
				"editorWarning.border": border,
				"editorInfo.border": border,
				"editorError.foreground": "#ff5370",
				"editorWarning.foreground": "#ffcb6b",
				"editorInfo.foreground": "#82aaff",
				"editorMarkerNavigationError.background": "#ff5370",
				"editorMarkerNavigationWarning.background": "#ffcb6b",
				"editorMarkerNavigationInfo.background": "#82aaff",
			},
		});
	}

	injectStyles() {
		let { readOnly, fontSize } = this.props;
		const appearance = this.settings().section("appearance");
		const mode = appearance.section(appearance.get("mode"));
		let background = mode.get("background");
		if (readOnly) background = darken(background, 0.05);
		const fontFamily = appearance.get("fontFamily");
		if (!fontSize) fontSize = appearance.get("fontSize");
		const primary = mode.get("primaryColor");
		const text = mode.get("primaryText");
		const gray = "#888";
		const border = gray;
		let rules = `
		.monaco-editor,
		.monaco-editor-background,
		.monaco-editor .margin,
		.monaco-editor .glyph-margin,
		.monaco-editor .sticky-scroll,
		.monaco-editor .sticky-widget,
		.monaco-editor .scroll-decoration,
		.monaco-editor .overlayWidgets,
		.monaco-editor .contentWidgets {
			background: ${background} !important;
			color: ${primary};
		}
		.monaco-editor .line-numbers {
			color: ${gray} !important;
		}
		.monaco-editor .find-widget {
			border: 1px solid ${border} !important;
			background-color: ${background} !important;
			box-shadow: none !important;
		}
		.monaco-editor .monaco-findInput .monaco-inputbox {
			border: 1px solid ${border} !important;
			background-color: ${background} !important;
			box-shadow: none !important;
		}
		.monaco-editor .monaco-findInput .monaco-inputbox .input {
			color: ${gray} !important;
			background-color: ${background} !important;
		}
		.monaco-editor .monaco-findInput .monaco-inputbox .input:focus {
			border: 1px solid ${primary} !important;
			outline: none !important;
		}
		.monaco-editor .monaco-findInput .monaco-inputbox .input.invalid,
		.monaco-editor .monaco-findInput .monaco-inputbox .input.invalid:focus {
			border: 1px solid ${gray} !important;
			box-shadow: none !important;
			outline: none !important;
		}
		.monaco-editor .find-widget .message {
			color: ${gray} !important;
		}
		.monaco-editor .find-widget .message.error {
			color: ${gray} !important;
		}
		.monaco-editor .find-widget::before,
		.monaco-editor .find-widget > .monaco-sash {
			display: none !important;
		}
		.class-link {
			text-decoration: underline;
			cursor: pointer;
		}
		.monaco-editor .monaco-list .monaco-list-row {
			color: ${text} !important;
		}
		.monaco-editor .suggest-widget {
			border: 1px solid ${gray} !important;
		}

		.monaco-editor .squiggly-error {
			border-bottom: 2px dotted ${mode.get("errorColor")} !important;
		}
		.monaco-editor .squiggly-warning {
			border-bottom: 2px dotted ${mode.get("warningColor")} !important;
		}
		.monaco-editor .squiggly-info {
			border-bottom: 2px dotted ${mode.get("infoColor")} !important;
		}
		.monaco-editor .margin .codicon-error,
		.monaco-editor .margin .codicon-warning,
		.monaco-editor .margin .codicon-info {
			color: ${gray} !important; /* mismo gris que el de los números de línea */
		}
		.monaco-editor .marker-widget {
			color: ${text} !important;
			background: ${background} !important;
			border: 1px solid ${border} !important;
		}

		.monaco-editor .monaco-hover {
			border: 1px solid ${border} !important;
			background: ${background} !important;
			color: ${text} !important;
		}

		.monaco-editor .monaco-hover .hover-contents {
			color: ${text} !important;
			border-color: transparent !important;
			box-shadow: none !important;
		}

		.monaco-editor .monaco-hover .hover-row {
			border-left: none !important;
		}
		
		.monaco-editor .monaco-hover .hover-row:first-child {
			border-top: none !important;
		}`;
		tokenTypes.forEach((type) => {
			const setting = mode.setting(`${type}Style`);
			if (setting) {
				rules += `\n.${type} {
				color: ${setting.color};
				font-style: ${setting.italic ? "italic" : "normal"};
				font-weight: ${setting.bold ? "bold" : "normal"};
			}`;
			}
		});
		const existing = document.getElementById("monaco-theme-dynamic");
		if (existing) existing.remove();
		const styleTag = document.createElement("style");
		styleTag.id = "monaco-theme-dynamic";
		styleTag.innerHTML = rules;
		document.head.appendChild(styleTag);
	}

	addCommands(editor) {
		if (!editor) return;
		this.shortcuts().forEach(({ shortcut, action }) => {
			const keys = this.adaptShortcut(shortcut);
			if (keys) {
				editor.addCommand(keys, () => {
					MonacoEditor.dispatchCommand(action);
				});
			}
		});
	}

	static dispatchCommand(action) {
		const editor = MonacoEditor.getActiveEditor();
		const instance = MonacoEditor.getInstanceForEditor(editor);
		console.log("Dispatching command");
		console.log(editor?.getValue(), editor?.getPosition());
		console.log(
			editor?.getModel()?.uri.toString(),
			instance,
			instance.editor === editor
		);
		if (!instance) return;
		action.bind(instance)();
	}

	adaptShortcut(shortcut) {
		if (!shortcut || typeof shortcut !== "string") return null;
		const parts = shortcut.toLowerCase().split("+");
		let mod = 0;
		let key = null;
		parts.forEach((part) => {
			switch (part.trim()) {
				case "ctrl":
					mod |= monaco.KeyMod.CtrlCmd;
					break;
				case "shift":
					mod |= monaco.KeyMod.Shift;
					break;
				case "alt":
					mod |= monaco.KeyMod.Alt;
					break;
				default:
					const upper = part.trim().toUpperCase();
					if (upper.length === 1 && upper >= "A" && upper <= "Z") {
						key = monaco.KeyCode["Key" + upper];
					} else if (!isNaN(Number(upper))) {
						key = monaco.KeyCode["Digit" + upper];
					} else if (upper === "ENTER") {
						key = monaco.KeyCode.Enter;
					} else if (upper === "TAB") {
						key = monaco.KeyCode.Tab;
					} else if (upper === "ESC" || upper === "ESCAPE") {
						key = monaco.KeyCode.Escape;
					} else if (upper.startsWith("F") && upper.length <= 3) {
						const fn = parseInt(upper.slice(1), 10);
						if (fn >= 1 && fn <= 12) key = monaco.KeyCode["F" + fn];
					}
			}
		});
		return key == null ? null : mod | key;
	}

	setDecorations(editor) {
		const model = editor.getModel();
		if (!editor || !model) return;
		this.clearHoverDecoration(editor);
		const source = model.getValue();
		const tokens = tokenize(source, this.isInMethod());
		const decorations = tokens.map((token) => {
			const start = model.getPositionAt(token.start);
			const end = model.getPositionAt(token.end);
			let classname = token.type;
			if (classname === "var") classname = "variable";
			return {
				range: new monaco.Range(
					start.lineNumber,
					start.column,
					end.lineNumber,
					end.column
				),
				options: { inlineClassName: classname },
			};
		});
		this.decorations = editor.deltaDecorations(
			this.decorations,
			decorations
		);
	}

	clearHoverDecoration(editor) {
		const model = editor.getModel();
		if (!model) return;
		this.hoverDecoration = model.deltaDecorations(this.hoverDecoration, []);
	}

	// Source access and manipulation

	source() {
		const editor = MonacoEditor.getActiveEditor();
		if (!editor) return;
		return editor.getValue();
	}

	currentPosition = () => {
		const editor = MonacoEditor.getActiveEditor();
		if (!editor) return;
		const model = editor.getModel();
		if (!model) return;
		const position = editor.getPosition(); // { lineNumber, column }
		const offset = model.getOffsetAt(position);
		return offset;
	};

	wordAtPosition = (offset) => {
		const editor = MonacoEditor.getActiveEditor();
		if (!editor) return;
		const model = editor.getModel();
		const position = model.getPositionAt(offset);
		const info = model.getWordAtPosition(position);
		if (info) return info.word;
	};

	selectedText = () => {
		const editor = MonacoEditor.getActiveEditor();
		if (!editor) return "";
		const model = editor.getModel();
		const selection = editor.getSelection();
		if (!model || !selection) return "";
		return model.getValueInRange(selection);
	};

	currentLine = () => {
		const editor = MonacoEditor.getActiveEditor();
		if (!editor) return;
		const model = editor.getModel();
		const position = editor.getPosition(); // { lineNumber, column }
		if (!model || !position) return;
		return model.getLineContent(position.lineNumber);
	};

	currentLineRange = () => {
		const editor = MonacoEditor.getActiveEditor();
		if (!editor) return null;
		const model = editor.getModel();
		const position = editor.getPosition();
		if (!model || !position) return null;
		const lineNumber = position.lineNumber;
		const from = model.getOffsetAt({ lineNumber, column: 1 });
		const to = model.getOffsetAt({
			lineNumber,
			column: model.getLineContent(lineNumber).length + 1,
		});
		return { from, to };
	};

	currentSelectionRange = () => {
		const editor = MonacoEditor.getActiveEditor();
		if (!editor) return;
		editor.focus();
		const model = editor.getModel();
		const selection = editor.getSelection();
		if (!model || !selection) return;
		const startOffset = model.getOffsetAt({
			lineNumber: selection.startLineNumber,
			column: selection.startColumn,
		});
		const endOffset = model.getOffsetAt({
			lineNumber: selection.endLineNumber,
			column: selection.endColumn,
		});
		return { from: startOffset, to: endOffset };
	};

	selectRanges = (ranges) => {
		const editor = MonacoEditor.getActiveEditor();
		if (!editor || !ranges?.length) return;
		const model = editor.getModel();
		if (!model) return;
		const { from, to } = ranges[0];
		const start = model.getPositionAt(from);
		const end = model.getPositionAt(to);
		const selection = new monaco.Selection(
			start.lineNumber,
			start.column,
			end.lineNumber,
			end.column
		);
		editor.setSelection(selection);
		editor.revealRangeInCenter(selection);
	};

	insertText = (text, position) => {
		const editor = MonacoEditor.getActiveEditor();
		if (!editor) return;
		const model = editor.getModel();
		if (!model) return;
		const pos = model.getPositionAt(position);
		editor.executeEdits(null, [
			{
				range: new monaco.Range(
					pos.lineNumber,
					pos.column,
					pos.lineNumber,
					pos.column
				),
				text,
				forceMoveMarkers: true,
			},
		]);
		editor.focus();
	};

	replaceSelectionWith = (text) => {
		const editor = MonacoEditor.getActiveEditor();
		if (!editor) return;
		const selection = editor.getSelection();
		if (!selection) return;
		editor.executeEdits(null, [
			{
				range: selection,
				text: text,
				forceMoveMarkers: true,
			},
		]);
		editor.focus(); // Check if this is needed
	};

	// Event handlers

	colorModeChanged = () => {
		this.defineTheme();
		monaco.editor.setTheme("webside");
		this.injectStyles();
		this.setDecorations(this.editor);
	};

	mouseMoved = async (event, editor) => {
		if (!event.event.ctrlKey) return;
		const model = editor.getModel();
		if (!model) return;
		const pos = event.target.position;
		if (!pos) return this.clearHoverDecoration(editor);
		const info = model.getWordAtPosition(pos);
		if (!info) return this.clearHoverDecoration(editor);
		const word = info.word;
		if (!/^[A-Z][a-zA-Z0-9_]*$/.test(word))
			return this.clearHoverDecoration(editor);
		let species;
		try {
			species = await ide.backend.classNamed(word);
		} catch (ignored) {}
		if (!species) {
			return this.clearHoverDecoration(editor);
		}
		const range = new monaco.Range(
			pos.lineNumber,
			info.startColumn,
			pos.lineNumber,
			info.endColumn
		);
		this.hoverDecoration = editor.deltaDecorations(this.hoverDecoration, [
			{
				range,
				options: {
					inlineClassName: "class-link",
					hoverMessage: {
						value: `Browse class **${word}**`,
					},
				},
			},
		]);
	};

	mouseClicked = (event, editor) => {
		if (event.event.detail === 2) {
			this.includeColonInSelection();
			return;
		}
		if (!event.event.ctrlKey) return;
		const model = editor.getModel();
		if (!model) return;
		const target = event.target;
		if (target.type !== monaco.editor.MouseTargetType.CONTENT_TEXT) return;
		const info = model.getWordAtPosition(target.position);
		if (!info || !/^[A-Z][a-zA-Z0-9_]*$/.test(info.word)) return;
		this.clearHoverDecoration(editor);
		this.context.browseClass(info.word);
	};

	// Autocompletion and tooltips

	completionList = async (model, position) => {
		const offset = model.getOffsetAt(position);
		const source = model.getValue();
		const word = model.getWordUntilPosition(position);
		const list = { suggestions: [] };
		if (word.word.length < 2) return list;
		try {
			const completions = await this.getCompletions(source, offset);
			const range = {
				startLineNumber: position.lineNumber,
				startColumn: word.startColumn,
				endLineNumber: position.lineNumber,
				endColumn: word.endColumn,
			};
			list.suggestions = completions.map((c) => ({
				label: c.label,
				kind: this.completionKind(c.type),
				insertText: c.label,
				range: range,
			}));
		} catch (ignored) {}
		return list;
	};

	completionKind(type) {
		switch (type) {
			case "class":
				return monaco.languages.CompletionItemKind.Class;
			case "method":
				return monaco.languages.CompletionItemKind.Method;
			case "variable":
				return monaco.languages.CompletionItemKind.Variable;
			case "keyword":
				return monaco.languages.CompletionItemKind.Keyword;
			default:
				return monaco.languages.CompletionItemKind.Text;
		}
	}

	// Linting annotations

	updateAnnotations = (editor) => {
		const annotations = this.props.annotations;
		if (!editor || !annotations) return;
		const model = editor.getModel();
		if (!model) return;
		const markers = annotations.map((a) => {
			const start = model.getPositionAt(a.from - 1);
			const end = model.getPositionAt(a.to - 1);
			return {
				severity: this.annotationSeverity(a.type),
				message: a.description,
				startLineNumber: start.lineNumber,
				startColumn: start.column,
				endLineNumber: end.lineNumber,
				endColumn: end.column,
			};
		});
		monaco.editor.setModelMarkers(model, "owner", markers);
	};

	annotationSeverity = (type) => {
		switch (type) {
			case "error":
				return monaco.MarkerSeverity.Error;
			case "warning":
				return monaco.MarkerSeverity.Warning;
			case "info":
				return monaco.MarkerSeverity.Info;
			default:
				return monaco.MarkerSeverity.Hint;
		}
	};

	// Rendering

	// render() {
	// 	return (
	// 		<Box
	// 			ref={this.containerRef}
	// 			sx={{
	// 				width: "100%",
	// 				height: "100%",
	// 				outline: "none",
	// 				border: "none",
	// 			}}
	// 		/>
	// 	);
	// }

	render() {
		const { evaluating, currentEvaluation, dirty, menuOpen, menuPosition } =
			this.state;
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
					<Box
						ref={this.containerRef}
						sx={{
							width: "100%",
							height: "100%",
							outline: "none",
							border: "none",
						}}
					/>
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

export default MonacoEditor;
