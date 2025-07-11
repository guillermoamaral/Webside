import React from "react";
import * as monaco from "monaco-editor";
import { Box } from "@mui/material";
import Scrollable from "../controls/Scrollable";
import PopupMenu from "../controls/PopupMenu";
import ToolContainerContext from "../ToolContainerContext";
import { smalltalkMonarchDefinition } from "../../SmalltalkMonarch";
import CodeEditor from "./CodeEditor";

class MonacoDiffEditor extends CodeEditor {
	static contextType = ToolContainerContext;
	containerRef = React.createRef();
	editorOriginal = null;
	editorModified = null;
	decorationsOriginal = [];
	decorationsModified = [];

	state = {
		menuOpen: false,
		menuPosition: { x: null, y: null },
	};

	componentDidMount() {
		monaco.languages.register({ id: "smalltalk" });
		monaco.languages.setMonarchTokensProvider(
			"smalltalk",
			smalltalkMonarchDefinition
		);

		this.editorOriginal = monaco.editor.create(
			this.containerRef.current.children[0],
			{
				value: this.props.leftCode || "",
				language: "smalltalk",
				readOnly: true,
				theme: "vs-dark",
				fontSize: 14,
				minimap: { enabled: false },
				scrollBeyondLastLine: false,
			}
		);

		this.editorModified = monaco.editor.create(
			this.containerRef.current.children[1],
			{
				value: this.props.rightCode || "",
				language: "smalltalk",
				readOnly: false,
				theme: "vs-dark",
				fontSize: 14,
				minimap: { enabled: false },
				scrollBeyondLastLine: false,
			}
		);

		this.editorModified.addCommand(
			monaco.KeyMod.CtrlCmd | monaco.KeyCode.Enter,
			() => this.props.onAccept?.(this.editorModified.getValue())
		);

		this.editorModified.addAction({
			id: "inspect-it",
			label: "Inspect It",
			keybindings: [monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyI],
			run: () => this.props.onExtendedOptionPerform?.("inspect"),
		});

		this.editorModified.onDidChangeModelContent(() => {
			this.applyHighlights();
		});

		this.applyHighlights();
	}

	componentDidUpdate(prevProps) {
		if (prevProps.leftCode !== this.props.leftCode) {
			this.editorOriginal.setValue(this.props.leftCode || "");
		}
		if (prevProps.rightCode !== this.props.rightCode) {
			this.editorModified.setValue(this.props.rightCode || "");
		}
		this.applyHighlights();
	}

	componentWillUnmount() {
		this.editorOriginal?.dispose();
		this.editorModified?.dispose();
	}

	extractSymbols(code) {
		const temporals = new Set();
		const parameters = new Set();
		const lines = code.split(/\r?\n/);

		for (let line of lines) {
			const trimmed = line.trim();
			if (trimmed.startsWith("|") && trimmed.endsWith("|")) {
				trimmed
					.slice(1, -1)
					.trim()
					.split(/\s+/)
					.forEach((t) => temporals.add(t));
			}
		}

		const header = lines.slice(0, 6).join(" ");
		const re = /\b([a-zA-Z0-9_]+):\s*([a-zA-Z0-9_]+)/g;
		let match;
		while ((match = re.exec(header))) {
			parameters.add(match[2]);
		}

		return { temporals, parameters };
	}

	applyHighlights() {
		const apply = (editor, code, decorations, isLeft) => {
			const model = editor.getModel();
			if (!model) return;
			const { temporals, parameters } = this.extractSymbols(code);
			const newDecorations = [];

			const classRegex = /\b[A-Z][a-zA-Z0-9_]*\b/g;
			const identifierRegex = /\b[a-zA-Z_][a-zA-Z0-9_]*\b/g;
			const lines = code.split(/\r?\n/);

			for (let lineNumber = 1; lineNumber <= lines.length; lineNumber++) {
				const line = lines[lineNumber - 1];
				let match;

				while ((match = classRegex.exec(line))) {
					const start = match.index + 1;
					const end = start + match[0].length;
					newDecorations.push({
						range: new monaco.Range(
							lineNumber,
							start,
							lineNumber,
							end
						),
						options: {
							inlineClassName: "class-link",
							hoverMessage: {
								value: `Go to class **${match[0]}** (Ctrl+Click)`,
							},
						},
					});
				}

				while ((match = identifierRegex.exec(line))) {
					const name = match[0];
					const start = match.index + 1;
					const end = start + name.length;
					if (parameters.has(name)) {
						newDecorations.push({
							range: new monaco.Range(
								lineNumber,
								start,
								lineNumber,
								end
							),
							options: { inlineClassName: "param-symbol" },
						});
					} else if (temporals.has(name)) {
						newDecorations.push({
							range: new monaco.Range(
								lineNumber,
								start,
								lineNumber,
								end
							),
							options: { inlineClassName: "temp-symbol" },
						});
					}
				}
			}

			editor.onMouseDown((e) => {
				if (!e.event.ctrlKey) return;
				const word = model.getWordAtPosition(e.target.position);
				if (!word || !/^[A-Z]/.test(word.word)) return;
				this.context.ide?.browseClass(word.word);
			});

			if (isLeft) {
				this.decorationsOriginal = editor.deltaDecorations(
					decorations,
					newDecorations
				);
			} else {
				this.decorationsModified = editor.deltaDecorations(
					decorations,
					newDecorations
				);
			}
		};

		apply(
			this.editorOriginal,
			this.props.leftCode || "",
			this.decorationsOriginal,
			true
		);
		apply(
			this.editorModified,
			this.props.rightCode || "",
			this.decorationsModified,
			false
		);
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

	render() {
		const { menuOpen, menuPosition } = this.state;
		const menuOptions = this.props.menuOptions?.() || [];

		return (
			<Box
				display="flex"
				flexDirection="column"
				style={{ height: "100%" }}
			>
				<Scrollable>
					<div
						ref={this.containerRef}
						style={{ display: "flex", height: "100%" }}
						onContextMenu={this.openMenu}
					>
						<div style={{ flex: 1, height: "100%" }} />
						<div style={{ flex: 1, height: "100%" }} />
					</div>
					{menuOptions.length > 0 && (
						<PopupMenu
							options={menuOptions}
							open={menuOpen}
							position={menuPosition}
							onClose={this.closeMenu}
						/>
					)}
				</Scrollable>
			</Box>
		);
	}
}

export default MonacoDiffEditor;
