import * as monaco from "monaco-editor";
import React from "react";
import { MonacoEditor } from "./MonacoEditor";
import { Box, LinearProgress } from "@mui/material";
import { ide } from "../IDE";
import PopupMenu from "../controls/PopupMenu";

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
			this.setState(
				{
					leftSource: leftSource,
					rightSource: rightSource,
				},
				() => {
					if (this.leftEditor) {
						this.leftEditor.setValue(rightSource);
						setTimeout(() => this.leftEditor?.layout(), 0);
						this.updateOverlays(this.leftEditor);
					}
					if (this.rightEditor) {
						this.rightEditor.setValue(leftSource);
						setTimeout(() => this.rightEditor?.layout(), 0);
						this.updateOverlays(this.rightEditor);
					}
				}
			);
		}
		//this.refreshLayout(this.editor);
		this.refreshLayout(this.leftEditor);
		this.refreshLayout(this.rightEditor);
		//this.editor.focus();
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
		});
		const { leftSource, rightSource } = this.props;
		const originalModel = monaco.editor.createModel(
			rightSource,
			"smalltalk"
		);
		const modifiedModel = monaco.editor.createModel(
			leftSource,
			"smalltalk"
		);
		this.editor.setModel({
			original: originalModel,
			modified: modifiedModel,
		});
	};

	setupEditor(editor) {
		this.leftEditor = editor.getOriginalEditor();
		this.rightEditor = editor.getModifiedEditor();
		super.setupEditor(this.leftEditor);
		super.setupEditor(this.rightEditor);
		this.resizeObserver = new ResizeObserver(() => {
			requestAnimationFrame(() => {
				//this.refreshLayout(this.editor);
				this.refreshLayout(this.leftEditor);
				this.refreshLayout(this.rightEditor);
			});
		});
		this.resizeObserver.observe(this.containerRef.current);
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

	injectStyles() {
		super.injectStyles();
		const styleElement = document.createElement("style");
		styleElement.innerHTML = `
        .monaco-diff-editor .editor.original {
            left: 50% !important;
            right: 0 !important;
        }
        .monaco-diff-editor .editor.modified {
            left: 0 !important;
            right: 50% !important;
        }`;
		document.head.appendChild(styleElement);
	}

	// Events handlers

	colorModeChanged = () => {
		this.defineTheme();
		monaco.editor.setTheme("webside");
		this.injectStyles();
		this.updateOverlays(this.leftEditor);
		this.updateOverlays(this.rightEditor);
	};

	async acceptSource() {
		if (!this.props.class) return;
		const source = this.normalizedSource();
		const pack = this.props.package;
		const species = this.props.class;
		const packagename = pack ? pack.name : species.package;
		const category = this.props.category;
		const result = await this.context.compileMethod(
			species.name,
			packagename,
			category,
			source
		);
		if (!result) return;
		if (result.hasError()) {
			const data = result.error.data;
			if (data && data.interval) {
				this.annotations = [
					{
						from: data.interval.start,
						to: data.interval.end,
						type: "error",
						description: data.description,
					},
				];
			} else {
				const description = data ? data.description : null;
				ide.reportError(description || "Unknown compilation error");
			}
		} else {
			if (this.props.onMethodCompile)
				this.props.onMethodCompile(result.method);
		}
	}

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
				<div id="tooltip-container"></div>
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

export default MonacoDiffEditor;
