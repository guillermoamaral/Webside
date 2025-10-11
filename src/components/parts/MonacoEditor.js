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
import { withTheme } from "@emotion/react";

let smalltalkRegistered = false;
//let tooltipRegistered = false;

class MonacoEditor extends CodeEditor {
    static contextType = ToolContainerContext;
    static openedInstances = new Map();
    static activeEditor = null;

    constructor(props) {
        super(props);
        this.containerRef = React.createRef();
        this.decorations = new Map();
        this.hoverDecoration = [];
        this.hoverAction = null;

        this.currentTooltipWidget = null;
        this.currentTooltipRef = null;
        this.currentTooltipRoot = null;
        this.tooltipTimeout = null;
        this.isTooltipHovered = false;

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
    }

    componentDidMount() {
        super.componentDidMount();
        this.defineTheme();
        this.initializeEditor();
        this.injectStyles();
        this.updateOverlays(this.editor);
        this.setupVisibilityHandlers();
        this.setupVisibilityCheck();
    }

    componentWillUnmount() {
        super.componentWillUnmount();
        this.hideTooltip(this.editor);
        this.clearHoverDecoration(this.editor);
        this.unregisterEditor(this.editor);
        this.disableWordHighlighter(this.editor);
        this.cleanupVisibilityHandlers();
        this.cleanupVisibilityCheck();
        this.editor?.dispose();
        this.editor = null;
    }

    componentDidUpdate(prevProps) {
        const newSource = this.props.source;
        if (
            this.normalizedSource(prevProps.source) !==
                this.normalizedSource(newSource) &&
            this.normalizedSource(this.props.source) !==
                this.normalizedSource(this.state.source)
        ) {
            this.setState(
                {
                    originalSource: this.props.originalSource ?? newSource,
                    source: this.props.source,
                    dirty: false,
                    showAnnotations: true,
                },
                () => {
                    if (this.editor) {
                        this.updatingFromProps = true;
                        this.updateEditor(this.editor, newSource);
                        this.selectsRanges = true;
                        this.updateSelections();
                    }
                }
            );
        }
        if (
            JSON.stringify(prevProps.selectedInterval) !==
                JSON.stringify(this.props.selectedInterval) ||
            prevProps.selectedSelector !== this.props.selectedSelector ||
            prevProps.selectedIdentifier !== this.props.selectedIdentifier
        ) {
            this.selectsRanges = true;
            this.updateSelections();
        }
        this.forceEditorRedraw();
        this.updateOverlays(this.editor);
        this.disableWordHighlighter(this.editor);
    }

    static getActiveEditor() {
        return MonacoEditor.activeEditor;
    }

    setupVisibilityHandlers() {
        this.handleVisibilityChange = () => {
            if (!document.hidden && this.editor) {
                setTimeout(() => {
                    this.forceEditorRedraw();
                }, 100);
            }
        };

        this.handleWindowFocus = () => {
            if (this.editor && this.editor.getModel()) {
                setTimeout(() => {
                    this.forceEditorRedraw();
                }, 50);
            }
        };

        this.handleIntersectionChange = (entries) => {
            entries.forEach((entry) => {
                if (entry.isIntersecting && this.editor) {
                    setTimeout(() => {
                        this.forceEditorRedraw();
                    }, 100);
                }
            });
        };

        document.addEventListener(
            "visibilitychange",
            this.handleVisibilityChange
        );
        window.addEventListener("focus", this.handleWindowFocus);

        if (this.containerRef.current) {
            this.intersectionObserver = new IntersectionObserver(
                this.handleIntersectionChange,
                {
                    threshold: 0.1,
                }
            );
            this.intersectionObserver.observe(this.containerRef.current);
        }
    }

    cleanupVisibilityHandlers() {
        if (this.handleVisibilityChange) {
            document.removeEventListener(
                "visibilitychange",
                this.handleVisibilityChange
            );
        }
        if (this.handleWindowFocus) {
            window.removeEventListener("focus", this.handleWindowFocus);
        }
        if (this.intersectionObserver) {
            this.intersectionObserver.disconnect();
        }
    }

    setupVisibilityCheck() {
        this.visibilityCheckInterval = setInterval(() => {
            if (
                this.editor &&
                this.editor.getModel() &&
                this.containerRef.current
            ) {
                const rect = this.containerRef.current.getBoundingClientRect();
                if (rect.width > 0 && rect.height > 0) {
                    const editorElement =
                        this.containerRef.current.querySelector(
                            ".monaco-editor"
                        );
                    if (editorElement) {
                        const editorRect =
                            editorElement.getBoundingClientRect();
                        if (editorRect.width === 0 || editorRect.height === 0) {
                            this.forceEditorRedraw();
                        }
                    }
                }
            }
        }, 1000);
    }

    cleanupVisibilityCheck() {
        if (this.visibilityCheckInterval) {
            clearInterval(this.visibilityCheckInterval);
        }
    }

    forceEditorRedraw() {
        if (!this.editor || !this.editor.getModel()) {
            return;
        }
        try {
            this.editor.layout();
            this.editor.render();
            this.updateOverlays(this.editor);
            const container = this.containerRef.current;
            if (container) {
                const rect = container.getBoundingClientRect();
                if (rect.width === 0 || rect.height === 0) {
                    setTimeout(() => {
                        if (this.editor && this.editor.getModel()) {
                            this.editor.layout();
                        }
                    }, 200);
                }
            }
        } catch (error) {
            console.warn("Error forcing editor redraw:", error);
        }
    }

    static setActiveEditor(editor) {
        // console.log(
        // 	"setting active editor",
        // 	editor?.getModel()?.uri.toString()
        // );
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
        this.addCommands(this.editor);
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
                    const result = await instance.completionList(
                        model,
                        position
                    );
                    return result;
                },
            });
            smalltalkRegistered = true;
        }
    };

    editorOptions() {
        const appearance = this.settings().section("appearance");
        // console.log(
        // 	this.props.readOnly === undefined ? false : this.props.readOnly
        // );
        return {
            readOnly:
                this.props.readOnly === undefined ? false : this.props.readOnly,
            theme: "webside",
            contextmenu: false,
            fontFamily: appearance.get("fontFamily"),
            fontSize: appearance.get("fontSize"),
            lineNumbers: this.showLineNumbers?.() ? "on" : "off",
            minimap: { enabled: false },
            scrollBeyondLastLine: false,
            renderValidationDecorations: "on",
            stickyScroll: {
                enabled: false,
            },
            wordHighlighter: false,
            wordWrap: "on",
            fixedOverflowWidgets: true,
            suggest: { showStatusBar: false },
        };
    }

    createEditor = (container) => {
        this.editor = monaco.editor.create(container, {
            ...this.editorOptions(),
            value: this.state.source,
            language: "smalltalk",
        });
        this.disableWordHighlighter(this.editor);
        MonacoEditor.setActiveEditor(this.editor);
    };

    setupEditor(editor) {
        // const proto = editor.getModel().constructor.prototype;
        // if (!proto.__patched) {
        // 	proto.getLineMaxColumn2 = proto.getLineMaxColumn;
        // 	proto.getLineMaxColumn = function (lineNumber) {
        // 		if (lineNumber < 1 || lineNumber > this.getLineCount()) {
        // 			console.log("daleeeeeeeeeeee", lineNumber);
        // 			console.log(
        // 				this.getValue(),
        // 				this.getLineCount(),
        // 				lineNumber
        // 			);
        // 		}
        // 		return proto.getLineMaxColumn2.call(this, lineNumber);
        // 	};
        // 	proto.__patched = true;
        // }

        editor.onDidFocusEditorText(() => {
            if (this.state?.menuOpen) {
                //"Ignoring focus during context menu"
                return;
            }
            MonacoEditor.setActiveEditor(editor);
        });
        editor.onDidChangeModel(() => {
            this.disableWordHighlighter(editor);
        });
        editor.onDidChangeModelContent(() => {
            MonacoEditor.setActiveEditor(editor);
            if (this.updatingFromProps) {
                this.updatingFromProps = false;
                return;
            }
            this.selectsRanges = false;
            const value = editor.getValue();
            this.sourceChanged(value);
            this.updateOverlays(editor);
        });
        editor.onMouseMove((event) => {
            MonacoEditor.setActiveEditor(editor);
            this.mouseMoved(event, editor);
        });
        editor.onMouseLeave(() => {
            this.clearTooltipTimeout();
            this.tooltipTimeout = setTimeout(() => {
                if (!this.isTooltipHovered) this.hideTooltip(editor);
            }, 150);
        });
        editor.onMouseDown((event) => {
            this.selectsRanges = false;
            editor.focus();
            MonacoEditor.setActiveEditor(editor);
            //this.showDebugInfo();
            this.mouseClicked(event, editor);
            if (this.currentTooltipWidget && !this.isTooltipHovered) {
                this.hideTooltip(editor);
            }
        });
        editor.onContextMenu((event) => {
            if (!event.target?.position) return;
            MonacoEditor.setActiveEditor(editor);
            this.openMenu(event.event.browserEvent);
        });
        this.disableStickyScroll(editor);
    }

    disableStickyScroll(editor) {
        editor?.updateOptions({ stickyScroll: { enabled: false } });
        if (editor?._contributions?.["stickyScrollController"]) {
            editor._contributions["stickyScrollController"].dispose();
            delete editor._contributions["stickyScrollController"];
        }
    }

    disableWordHighlighter(editor) {
        const wh = editor?._contributions?.["wordHighlighter"];
        if (wh && typeof wh.dispose === "function") {
            if (!wh._isDisposed) {
                try {
                    wh.dispose();
                    wh._isDisposed = true;
                } catch (e) {
                    if (e?.message?.includes("Canceled")) return;
                    throw e;
                }
            }
            delete editor._contributions["wordHighlighter"];
        }
    }

    registerEditor(editor) {
        const model = editor?.getModel();
        if (model) MonacoEditor.openedInstances.set(model.uri.toString(), this);
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

    unregisterEditor(editor) {
        const model = editor?.getModel();
        if (model) MonacoEditor.openedInstances.delete(model.uri.toString());
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
        const error = mode.get("errorColor");
        const warning = mode.get("warningColor");
        const info = mode.get("infoColor");
        monaco.editor.defineTheme("webside", {
            base: "vs-dark",
            inherit: false,
            rules: tokenTypes
                .filter((type) => mode.get(`${type}Color`))
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
                "editorError.foreground": error,
                "editorWarning.foreground": warning,
                "editorInfo.foreground": info,
                "editorMarkerNavigationError.background": error,
                "editorMarkerNavigationWarning.background": warning,
                "editorMarkerNavigationInfo.background": info,
            },
        });
    }

    completionIconStyles() {
        const kinds = ["class", "method", "variable", "constant"];
        let rules = "";
        kinds.forEach((kind) => {
            const icon = ide.iconNamed(kind);
            if (icon && icon.data) {
                rules += `
				.monaco-editor .suggest-widget .icon.${kind} {
				background-image: url('data:image/png;base64,${icon.data}') !important;
				background-size: 16px 16px;
				background-repeat: no-repeat;
				background-position: center;
				}`;
            }
        });
        return rules;
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
        const selection = mode.get("selectionColor");
        const error = mode.get("errorColor");
        const warning = mode.get("warningColor");
        const info = mode.get("infoColor");
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
		.monaco-editor .selectionHighlight,
		.monaco-editor .selected-text {
			background-color: ${selection} !important;
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
		.hover-link {
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
			border-bottom: 2px dotted ${error} !important;
		}
		.monaco-editor .squiggly-warning {
			border-bottom: 2px dotted ${warning} !important;
		}
		.monaco-editor .squiggly-info {
			border-bottom: 2px dotted ${info} !important;
		}
		.monaco-editor .squiggly-error,
		.monaco-editor .squiggly-warning,
		.monaco-editor .squiggly-info {
			text-decoration: none !important;
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
		.monaco-editor .monaco-hover .hover-row.status-bar {
			border-top: none !important;   /* quita la línea */
			background: transparent !important; /* asegura que no pinte rojo */
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
		.monaco-hover .hover-contents .hover-row.separator {
			border: none !important;
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
        const styleElement = document.createElement("style");
        styleElement.id = "monaco-theme-dynamic";
        rules += this.completionIconStyles();
        styleElement.innerHTML = rules;
        document.head.appendChild(styleElement);
    }

    static dispatchCommand(action) {
        const editor = MonacoEditor.getActiveEditor();
        const instance = MonacoEditor.getInstanceForEditor(editor);
        if (!instance) return;
        // console.log(
        // 	"Dispatching command from",
        // 	editor?.getModel()?.uri.toString(),
        // 	instance,
        // 	action,
        // 	editor?.getValue()
        // );
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

    // Highlighting and annotations

    updateOverlays(editor) {
        if (!editor) return;
        const model = editor.getModel();
        if (!model || model.isDisposed()) return;
        if (model.getLineCount() === 0) return;
        if (model.getValueLength() === 0) return;
        try {
            this.updateDecorations(editor);
            this.updateAnnotations(editor);
            editor.layout();
        } catch (err) {
            console.warn("Skipped overlay update:", err);
        }
    }

    updateDecorations(editor) {
        const model = editor?.getModel();
        if (!editor || !model) return;
        this.clearDecorations(editor);
        this.clearHoverDecoration(editor);
        const source = model.getValue();
        const tokens = tokenize(source, this.isInMethod());
        const limit = model.getValueLength();
        const decorations = tokens
            .map((token) => {
                const startOffset = token.start;
                const endOffset = token.end;
                if (
                    startOffset < 0 ||
                    endOffset <= startOffset ||
                    endOffset > limit
                ) {
                    console.warn("Invalid token range", token, "limit:", limit);
                    return null;
                }
                let start, end;
                try {
                    start = model.getPositionAt(startOffset);
                    end = model.getPositionAt(endOffset);
                } catch (err) {
                    console.warn(
                        "Failed to get positions for token",
                        token,
                        err
                    );
                    return null;
                }
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
            })
            .filter(Boolean);
        const uri = model.uri.toString();
        const previous = this.decorations.get(uri) || [];
        const updated = editor.deltaDecorations(previous, decorations);
        this.decorations.set(uri, updated);
    }

    clearDecorations(editor) {
        if (!editor) return;
        const model = editor.getModel();
        if (!model) return;
        const uri = model.uri.toString();
        const previous = this.decorations.get(uri) || [];
        editor.deltaDecorations(previous, []);
        this.decorations.set(uri, []);
    }

    clearHoverDecoration(editor) {
        if (!editor) return;
        this.hoverAction = null;
        this.hoverDecoration = editor.deltaDecorations(
            this.hoverDecoration,
            []
        );
    }

    async updateHoverDecoration(editor, position) {
        const model = editor.getModel();
        if (!model || !position) return this.clearHoverDecoration(editor);
        const offset = this.normalizedOffset(model.getOffsetAt(position));
        const node = this.astNodeAtOffset(offset);
        let l1, l2, c1, c2, message;
        if (node && node.type === "Selector") {
            const position1 = model.getPositionAt(
                this.denormalizedOffset(node.start - 1)
            );
            const position2 = model.getPositionAt(
                this.denormalizedOffset(node.end)
            );
            if (!position1 || !position2)
                return this.clearHoverDecoration(editor);
            l1 = position1.lineNumber;
            c1 = position1.column;
            l2 = position2.lineNumber;
            c2 = position2.column;
            this.hoverAction = () => {
                this.browseImplementors(node.value);
            };
            message = `Browse implementors of **${node.value}**`;
        } else {
            const info = model.getWordAtPosition(position);
            if (!info) return this.clearHoverDecoration(editor);
            let species;
            try {
                species = await ide.backend.classNamed(info.word);
            } catch (ignored) {}
            if (!species) return this.clearHoverDecoration(editor);
            l1 = position.lineNumber;
            c1 = info.startColumn;
            l2 = position.lineNumber;
            c2 = info.endColumn;
            message = `Browse class **${info.word}**`;
            this.hoverAction = () => {
                this.browseClass(info.word);
            };
        }
        this.hoverDecoration = editor.deltaDecorations(this.hoverDecoration, [
            {
                range: new monaco.Range(l1, c1, l2, c2),
                options: {
                    inlineClassName: "hover-link",
                    hoverMessage: { value: message },
                },
            },
        ]);
    }

    updateAnnotations = (editor) => {
        if (!editor) return;
        const model = editor.getModel();
        if (!model) return;
        this.clearAnnotations(editor);
        const annotations = this.props.annotations;
        if (!annotations || !this.state.showAnnotations) return;
        const markers = annotations.map((a) => {
            const start = model.getPositionAt(
                this.denormalizedOffset(a.from - 1)
            );
            const end = model.getPositionAt(this.denormalizedOffset(a.to - 1));
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

    clearAnnotations(editor) {
        if (!editor) return;
        const model = editor.getModel();
        if (!model) return;
        monaco.editor.setModelMarkers(model, "owner", []);
    }

    // Source access and manipulation

    source() {
        const editor = MonacoEditor.getActiveEditor();
        if (editor) return editor.getValue();
    }

    resetEditor(editor) {
        if (!editor) return;
        const model = editor.getModel();
        if (!model || model.isDisposed()) return;
        this.clearDecorations(editor);
        this.clearAnnotations(editor);
        editor.setScrollTop(0);
        editor.setScrollLeft(0);
    }

    updateEditor(editor, source) {
        // Super defensive approach to clean everything before changing the value
        this.resetEditor(editor);
        try {
            editor.setValue(source);
        } catch (error) {
            console.warn("Error setting editor value:", error);
        }
        setTimeout(() => editor.layout(), 0);
        this.updateOverlays(editor);
    }

    currentPosition() {
        const editor = MonacoEditor.getActiveEditor();
        if (!editor) return;
        const model = editor.getModel();
        if (!model) return;
        const position = editor.getPosition(); // { lineNumber, column }
        const offset = model.getOffsetAt(position);
        return this.normalizedOffset(offset);
    }

    wordAtPosition(offset) {
        const editor = MonacoEditor.getActiveEditor();
        if (!editor) return;
        const model = editor.getModel();
        const position = model.getPositionAt(this.denormalizedOffset(offset));
        const info = model.getWordAtPosition(position);
        if (info) return info.word;
    }

    selectedText() {
        const editor = MonacoEditor.getActiveEditor();
        if (!editor) return "";
        const model = editor.getModel();
        const selection = editor.getSelection();
        if (!model || !selection) return "";
        return model.getValueInRange(selection);
    }

    currentLine() {
        const editor = MonacoEditor.getActiveEditor();
        if (!editor) return;
        const model = editor.getModel();
        const position = editor.getPosition(); // { lineNumber, column }
        if (!model || !position) return;
        return model.getLineContent(position.lineNumber);
    }

    currentLineRange() {
        const editor = MonacoEditor.getActiveEditor();
        if (!editor) return null;
        const model = editor.getModel();
        const position = editor.getPosition();
        if (!model || !position) return;
        const lineNumber = position.lineNumber;
        const from = model.getOffsetAt({ lineNumber, column: 1 });
        const line = model.getLineContent(lineNumber);
        const to = model.getOffsetAt({
            lineNumber,
            column: line.length + 1,
        });
        return {
            from: this.normalizedOffset(from),
            to: this.normalizedOffset(to),
        };
    }

    currentSelectionRange() {
        const editor = MonacoEditor.getActiveEditor();
        if (!editor) return;
        editor.focus();
        const model = editor.getModel();
        const selection = editor.getSelection();
        if (!model || !selection) return;
        const from = model.getOffsetAt({
            lineNumber: selection.startLineNumber,
            column: selection.startColumn,
        });
        const to = model.getOffsetAt({
            lineNumber: selection.endLineNumber,
            column: selection.endColumn,
        });
        return {
            from: this.normalizedOffset(from),
            to: this.normalizedOffset(to),
        };
    }

    insertText(text, offset) {
        const editor = MonacoEditor.getActiveEditor();
        if (!editor) return;
        const model = editor.getModel();
        if (!model) return;
        const position = model.getPositionAt(this.denormalizedOffset(offset));
        editor.executeEdits(null, [
            {
                range: new monaco.Range(
                    position.lineNumber,
                    position.column,
                    position.lineNumber,
                    position.column
                ),
                text,
                forceMoveMarkers: true,
            },
        ]);
    }

    replaceSelectionWith(text) {
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
    }

    selectRanges = (ranges) => {
        const editor = MonacoEditor.getActiveEditor();
        if (!editor || !ranges?.length) return;
        const model = editor.getModel();
        if (!model) return;
        const selections = ranges.map(({ from, to }) => {
            const start = model.getPositionAt(this.denormalizedOffset(from));
            const end = model.getPositionAt(this.denormalizedOffset(to));
            return new monaco.Selection(
                start.lineNumber,
                start.column,
                end.lineNumber,
                end.column
            );
        });
        editor.setSelections(selections);
        editor.revealRangeInCenter(selections[0]);
    };

    normalizedOffset(offset) {
        const source = this.source();
        if (offset < 0 || offset > source.length) return;
        return offset - (source.slice(0, offset).match(/\r\n/g) || []).length;
    }

    denormalizedOffset(offset) {
        const source = this.normalizedSource();
        if (offset < 0 || offset > source.length) return;
        return (source.slice(0, offset).match(/\r/g) || []).length + offset;
    }

    updateSelections() {
        if (!this.selectsRanges) return;
        const { selectedInterval, selectedSelector, selectedIdentifier } =
            this.props;
        if (selectedInterval) this.selectInterval(selectedInterval);
        if (selectedSelector) this.selectSelector(selectedSelector);
        if (selectedIdentifier) this.selectIdentifier(selectedIdentifier);
    }

    // Event handlers

    colorModeChanged = () => {
        this.defineTheme();
        monaco.editor.setTheme("webside");
        this.injectStyles();
        this.updateOverlays(this.editor);
    };

    mouseMoved = async (event, editor) => {
        const position = event.target.position;
        if (!position) return;
        if (event.event.ctrlKey)
            return await this.updateHoverDecoration(editor, position);
        if (!this.showsTooltip()) return;
        this.clearTooltipTimeout();
        this.tooltipTimeout = setTimeout(
            () => this.showTooltip(editor, position),
            150
        );
    };

    mouseClicked = (event, editor) => {
        if (event.event.detail === 2) return this.includeColonInSelection();
        if (!event.event.ctrlKey) return;
        if (this.hoverAction) this.hoverAction();
        this.clearHoverDecoration(editor);
    };

    // Autocompletion

    async completionList(model, position) {
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
    }

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

    // Tooltips

    async showTooltip(editor, position) {
        if (!this.showsTooltip()) return;
        const model = editor.getModel();
        if (!model || !position) return;
        const offset = this.normalizedOffset(model.getOffsetAt(position));
        const spec = await this.tooltipSpec(offset);
        if (!spec) return;
        if (this.currentTooltipWidget) {
            editor.removeContentWidget(this.currentTooltipWidget);
            this.currentTooltipRoot?.unmount();
            this.currentTooltipWidget = null;
            this.currentTooltipRoot = null;
        }
        const container = this.ensureTooltipContainer();
        container.innerHTML = "";
        this.currentTooltipRef = React.createRef();
        this.currentTooltipRoot = this.renderTooltip(
            spec,
            container,
            this.currentTooltipRef,
            () => this.hideTooltip(editor)
        );
        this.currentTooltipWidget = {
            getId: () => "code.tooltip.widget",
            getDomNode: () => container,
            getPosition: () => ({
                position,
                preference: [
                    //monaco.editor.ContentWidgetPositionPreference.EXACT,
                    monaco.editor.ContentWidgetPositionPreference.ABOVE,
                    monaco.editor.ContentWidgetPositionPreference.BELOW,
                ],
            }),
            allowEditorOverflow: true,
        };
        editor.addContentWidget(this.currentTooltipWidget);
        container.addEventListener("mouseenter", () => {
            this.isTooltipHovered = true;
            this.clearTooltipTimeout();
        });
        container.addEventListener("mouseleave", () => {
            this.isTooltipHovered = false;
            this.clearTooltipTimeout();
            this.tooltipTimeout = setTimeout(
                () => this.hideTooltip(editor),
                500
            );
        });
    }

    clearTooltipTimeout() {
        if (this.tooltipTimeout) {
            clearTimeout(this.tooltipTimeout);
            this.tooltipTimeout = null;
        }
    }

    hideTooltip(editor) {
        this.clearTooltipTimeout();
        if (this.currentTooltipWidget) {
            try {
                editor.removeContentWidget(this.currentTooltipWidget);
            } catch (ignored) {}
            this.currentTooltipWidget = null;
        }
        if (this.currentTooltipRoot) {
            try {
                this.currentTooltipRoot.unmount();
            } catch (ignored) {}
            this.currentTooltipRoot = null;
        }
        if (this.currentTooltipRef && this.currentTooltipRef.current) {
            this.currentTooltipRef.current.aboutToClose();
            this.currentTooltipRef = null;
        }
        this.isTooltipHovered = false;
    }

    // Rendering

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
                <Box
                    display="flex"
                    flexDirection="column"
                    sx={{ flex: "1 1 0%", minWidth: 0 }}
                >
                    <Box
                        flexGrow={1}
                        ref={this.containerRef}
                        sx={{
                            width: "100%",
                            height: "100%",
                            minHeight: "30px",
                            overflow: "hidden",
                            minWidth: 0,
                        }}
                    />
                    {evaluating && (
                        <Box>
                            <LinearProgress variant="indeterminate" />
                        </Box>
                    )}
                </Box>
                {showButtons && (
                    <Box
                        display="flex"
                        flexDirection="column"
                        sx={{ height: "100%", flexShrink: 0 }}
                    >
                        {showAccept && (
                            <Box display="flex" justifyContent="center">
                                <Tooltip title="Save" placement="top">
                                    <IconButton
                                        color="inherit"
                                        onClick={() => this.acceptSource()}
                                    >
                                        <AcceptIcon
                                            size="large"
                                            color={
                                                dirty ? "primary" : "inherit"
                                            }
                                            style={{ fontSize: 30 }}
                                        />
                                    </IconButton>
                                </Tooltip>
                            </Box>
                        )}
                        {showPlay && !evaluating && (
                            <Box display="flex" justifyContent="center">
                                <Tooltip title="Evaluate" placement="top">
                                    <IconButton
                                        color="inherit"
                                        onClick={this.playClicked}
                                    >
                                        <PlayIcon
                                            size="large"
                                            color={
                                                dirty ? "primary" : "inherit"
                                            }
                                        />
                                    </IconButton>
                                </Tooltip>
                            </Box>
                        )}
                        {showPlay && evaluating && (
                            <Box display="flex" justifyContent="center">
                                <Tooltip
                                    title="Pause evaluation"
                                    placement="top"
                                >
                                    <IconButton
                                        color="inherit"
                                        onClick={this.pauseClicked}
                                        disabled={
                                            !(
                                                currentEvaluation &&
                                                [
                                                    "pending",
                                                    "evaluating",
                                                ].includes(
                                                    currentEvaluation.state
                                                )
                                            )
                                        }
                                    >
                                        <PauseIcon
                                            size="large"
                                            color={
                                                dirty ? "primary" : "inherit"
                                            }
                                            style={{ fontSize: 30 }}
                                        />
                                    </IconButton>
                                </Tooltip>
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

export { MonacoEditor };
export default withTheme(MonacoEditor);
