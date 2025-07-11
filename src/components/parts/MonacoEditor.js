import React from "react";
import { Box } from "@mui/material";
import * as monaco from "monaco-editor";
import ToolContainerContext from "../ToolContainerContext";
import { smalltalkMonarchDefinition } from "../../SmalltalkMonarch";
import { ide } from "../IDE";
import { darken } from "@mui/system";
import CodeEditor from "./CodeEditor";
import { tokenize } from "../../SmalltalkTokenizer";

class MonacoEditor extends CodeEditor {
	static contextType = ToolContainerContext;

	constructor(props) {
		super(props);
		this.state = {
			source: props.source || "",
		};
		this.containerRef = React.createRef();
		this.editorRef = null;
		this.decorations = [];
		this.hoverDecoration = [];
		this.tagNames = [
			"selector",
			"symbol",
			"argument",
			"temporary",
			"assignment",
			"string",
			"variable",
			"meta",
			"bracket",
			"self",
			"super",
			"true",
			"false",
			"nil",
			"thisContext",
			"return",
			"global",
			"number",
			"comment",
		];
	}

	componentDidMount() {
		ide.onColorModeChange(this.colorModeChanged);

		monaco.languages.register({ id: "smalltalk" });
		monaco.languages.setMonarchTokensProvider(
			"smalltalk",
			smalltalkMonarchDefinition
		);

		this.defineTheme();

		const appearance = this.settings().section("appearance");

		this.editorRef = monaco.editor.create(this.containerRef.current, {
			value: this.source(),
			language: "smalltalk",
			readOnly: this.props.readOnly || false,
			fontFamily: appearance.get("fontFamily"),
			fontSize: appearance.get("fontSize"),
			theme: "webside",
			minimap: { enabled: false },
			scrollBeyondLastLine: false,
			lineNumbers: this.showLineNumbers() ? "on" : "off",
		});

		this.resizeObserver = new ResizeObserver(() => {
			requestAnimationFrame(() => {
				this.editorRef?.layout();
			});
		});

		this.resizeObserver.observe(this.containerRef.current);

		this.editorRef.onDidChangeModelContent(() => {
			const value = this.editorRef.getValue();
			this.sourceChanged(value);
			this.setDecorations();
		});

		this.editorRef.onMouseMove(this.mouseMoved);

		this.editorRef.onMouseDown(this.mouseClicked);

		this.injectStyles();
		this.addCommands();
		this.setDecorations();
	}

	componentDidUpdate(prevProps) {
		if (prevProps.source !== this.props.source) {
			this.setState({ source: this.props.source || "" }, () => {
				if (this.editorRef) {
					this.editorRef.setValue(this.state.source);
					this.setDecorations();
				}
			});
		}
		if (this.editorRef) {
			this.editorRef.layout();
			this.editorRef.focus();
		}
	}

	componentWillUnmount() {
		console.log("MonacoEditor componentWillUnmount");
		this.clearHoverDecoration();
		this.editorRef?.dispose();
		ide.removeColorModeChangeHandler(this.colorModeChanged);
		this.resizeObserver?.disconnect();
	}

	// Configuration

	defineTheme() {
		const appearance = this.settings().section("appearance");
		const mode = appearance.section(appearance.get("mode"));
		let background = mode.get("background");
		if (this.props.readOnly) background = darken(background, 0.05);
		const foreground = mode.get("primaryColor");
		const border = darken(background, 0.2);
		const selection = mode.get("selectionColor");
		monaco.editor.defineTheme("webside", {
			base: "vs-dark",
			inherit: true,
			rules: [],
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
		const border = darken(background, 0.3);
		const gray = "#888";

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
	`;

		this.tagNames.forEach((name) => {
			const setting = mode.setting(`${name}Style`);
			if (!setting) return;
			rules += `
			.${name} {
				color: ${setting.color};
				font-style: ${setting.italic ? "italic" : "normal"};
				font-weight: ${setting.bold ? "bold" : "normal"};
			}
		`;
		});

		const existing = document.getElementById("monaco-theme-dynamic");
		if (existing) existing.remove();

		const styleTag = document.createElement("style");
		styleTag.id = "monaco-theme-dynamic";
		styleTag.innerHTML = rules;
		document.head.appendChild(styleTag);
	}

	addCommands() {
		const editor = this.editorRef;
		if (!editor) return;
		this.shortcuts().forEach(({ shortcut, action }) => {
			const keys = this.adaptShortcut(shortcut);
			if (keys) {
				editor.addCommand(keys, () => {
					action();
				});
			}
		});
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

	setDecorations() {
		const editor = this.editorRef;
		const model = editor.getModel();
		if (!editor || !model) return;

		this.clearHoverDecoration();
		const code = model.getValue();
		const tokens = tokenize(code);

		const newDecorations = tokens.map((token) => {
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
			newDecorations
		);
	}

	clearHoverDecoration() {
		const model = this.editorRef.getModel();
		if (!model) return;

		this.hoverDecoration = model.deltaDecorations(this.hoverDecoration, []);
	}

	// Source access and manipulation

	currentPosition = () => {
		const editor = this.editorRef;
		if (!editor) return;
		const model = editor.getModel();
		if (!model) return;
		const position = editor.getPosition(); // { lineNumber, column }
		const offset = model.getOffsetAt(position);
		return offset;
	};

	wordAtPosition = (position) => {
		const model = this.editorRef.getModel();
		const info = model.getWordAtPosition(position);
		if (info) return info.word;
	};

	selectedText = () => {
		const editor = this.editorRef;
		if (!editor) return "";
		const model = editor.getModel();
		const selection = editor.getSelection();
		if (!model || !selection) return "";
		return model.getValueInRange(selection);
	};

	currentLine = () => {
		const editor = this.editorRef;
		if (!editor) return;
		const model = editor.getModel();
		const position = editor.getPosition(); // { lineNumber, column }
		if (!model || !position) return;
		return model.getLineContent(position.lineNumber);
	};

	currentLineRange = () => {
		const editor = this.editorRef;
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
		const editor = this.editorRef;
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
		if (!this.editorRef || !ranges?.length) return;
		const editor = this.editorRef;
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
		const editor = this.editorRef;
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

	// Event handlers

	colorModeChanged = () => {
		this.defineTheme();
		monaco.editor.setTheme("webside");
		this.injectStyles();
		this.setDecorations();
	};

	mouseMoved = async (e) => {
		if (!e.event.ctrlKey) return;
		const editor = this.editorRef;
		const model = editor.getModel();
		if (!model) return;
		const pos = e.target.position;
		if (!pos) return this.clearHoverDecoration();
		const wordInfo = model.getWordAtPosition(pos);
		if (!wordInfo) return this.clearHoverDecoration();
		const word = wordInfo.word;
		if (!/^[A-Z][a-zA-Z0-9_]*$/.test(word))
			return this.clearHoverDecoration();
		let species;
		try {
			species = await ide.backend.classNamed(word);
		} catch (ignored) {}
		if (!species) {
			return this.clearHoverDecoration();
		}

		const range = new monaco.Range(
			pos.lineNumber,
			wordInfo.startColumn,
			pos.lineNumber,
			wordInfo.endColumn
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

	mouseClicked = (e) => {
		if (!e.event.ctrlKey) return;
		const editor = this.editorRef;
		const model = editor.getModel();
		if (!model) return;
		const target = e.target;
		if (target.type !== monaco.editor.MouseTargetType.CONTENT_TEXT) return;
		const word = model.getWordAtPosition(target.position);
		if (!word || !/^[A-Z][a-zA-Z0-9_]*$/.test(word.word)) return;
		this.clearHoverDecoration();
		this.context.browseClass(word.word);
	};

	// Rendering

	render() {
		return (
			<Box
				ref={this.containerRef}
				sx={{
					width: "100%",
					height: "100%",
					outline: "none",
					border: "none",
				}}
			/>
		);
	}
}

export default MonacoEditor;
