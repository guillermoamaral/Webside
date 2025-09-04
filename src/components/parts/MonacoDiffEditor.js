import * as monaco from "monaco-editor";
import React from "react";
import { MonacoEditor } from "./MonacoEditor";
import { Box, LinearProgress } from "@mui/material";
import { ide } from "../IDE";
import PopupMenu from "../controls/PopupMenu";
import { withTheme } from "@emotion/react";

class MonacoDiffEditor extends MonacoEditor {
	constructor(props) {
		super(props);
		this.containerRef = React.createRef();
		this.editor = null;
		this.leftEditor = null;
		this.rightEditor = null;
		this.leftDecorations = [];
		this.rightDecorations = [];
		this.state = {
			dirty: false,
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
		this.updateOverlays(this.leftEditor);
		this.updateOverlays(this.rightEditor);
	}

	componentDidUpdate(prevProps) {
		const { leftSource, rightSource } = this.props;
		if (
			prevProps.leftSource !== leftSource ||
			prevProps.rightSource !== rightSource
		) {
			this.updateEditor(leftSource, rightSource);
		}
	}

	componentWillUnmount() {
		this.clearHoverDecoration(this.leftEditor);
		this.clearHoverDecoration(this.rightEditor);
		this.editor?.dispose();
		ide.removeColorModeChangeHandler(this.colorModeChanged);
		this.resizeObserver?.disconnect();
	}

	// Configuration

	createEditor = (container) => {
		this.editor = monaco.editor.createDiffEditor(container, {
			...this.editorOptions(),
			enableSplitViewResizing: true,
			renderSideBySide: true,
			automaticLayout: true,
		});
		const { leftSource, rightSource } = this.props;
		const original = monaco.editor.createModel(leftSource, "smalltalk");
		const modified = monaco.editor.createModel(rightSource, "smalltalk");
		this.editor.setModel({
			original: original,
			modified: modified,
		});
	};

	setupEditor(editor) {
		this.leftEditor = editor.getOriginalEditor();
		this.rightEditor = editor.getModifiedEditor();
		super.setupEditor(this.leftEditor);
		super.setupEditor(this.rightEditor);
	}

	registerEditor(ignored) {
		super.registerEditor(this.leftEditor);
		super.registerEditor(this.rightEditor);
	}

	addCommands = (ignored) => {
		super.addCommands(this.leftEditor);
		super.addCommands(this.rightEditor);
	};

	unregisterEditor(ignored) {
		super.unregisterEditor(this.leftEditor);
		super.unregisterEditor(this.rightEditor);
	}

	// Source access and manipulation

	updateEditor(leftSource, rightSource) {
		if (!this.editor) return;

		const original = monaco.editor.createModel(leftSource, "smalltalk");
		const modified = monaco.editor.createModel(rightSource, "smalltalk");

		const pair = this.editor.getModel();
		const oldOriginal = pair?.original;
		const oldModified = pair?.modified;

		if (this.leftEditor) this.leftEditor.setModel(null);
		if (this.rightEditor) this.rightEditor.setModel(null);

		this.editor.setModel({
			original,
			modified,
		});

		this.leftEditor = this.editor.getOriginalEditor();
		this.rightEditor = this.editor.getModifiedEditor();

		if (oldOriginal) oldOriginal.dispose();
		if (oldModified) oldModified.dispose();

		requestAnimationFrame(() => {
			if (!this.editor) return;

			this.editor.layout();
			this.leftEditor = this.editor.getOriginalEditor();
			this.rightEditor = this.editor.getModifiedEditor();

			this.updateOverlays(this.leftEditor);
			this.updateOverlays(this.rightEditor);
		});
	}

	// Events handlers

	colorModeChanged = () => {
		this.defineTheme();
		monaco.editor.setTheme("webside");
		this.injectStyles();
		this.updateOverlays(this.leftEditor);
		this.updateOverlays(this.rightEditor);
	};

	// Autocompletion and tooltips

	// Rendering

	render() {
		const { evaluating, currentEvaluation, dirty, menuOpen, menuPosition } =
			this.state;
		const menuOptions = this.menuOptions();
		return (
			<Box
				display="flex"
				flexDirection="row"
				style={{ width: "100%", height: "100%" }}
			>
				<Box
					ref={this.containerRef}
					flexGrow={1}
					sx={{
						width: "100%",
						height: "100%",
						outline: "none",
						border: "none",
					}}
				/>
				{evaluating && <LinearProgress variant="indeterminate" />}
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

export { MonacoDiffEditor };
export default withTheme(MonacoDiffEditor);
