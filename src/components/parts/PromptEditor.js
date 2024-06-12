import React, { Component } from "react";
import { Box } from "@mui/material";
import { ide } from "../IDE";
import ToolContainerContext from "../ToolContainerContext";
import Scrollable from "../controls/Scrollable";
import CodeMirror from "@uiw/react-codemirror";
import { EditorView, keymap } from "@codemirror/view";
import { acceptCompletion } from "@codemirror/autocomplete";
import { PromptParser } from "../../PromptParser";
import { Tag } from "@lezer/highlight";
import { createTheme } from "@uiw/codemirror-themes";
import { StreamLanguage } from "@codemirror/language";
import { Prec } from "@codemirror/state";
import {
	autocompletion,
	closeCompletion,
	startCompletion,
} from "@codemirror/autocomplete";

PromptParser.tokenTable = {
	at: Tag.define(),
	class: Tag.define(),
	selector: Tag.define(),
	text: Tag.define(),
};

class PromptEditor extends Component {
	static contextType = ToolContainerContext;

	constructor(props) {
		super(props);
		this.editorRef = React.createRef();
		this.editorView = null;
		this.typingTimer = null;
		this.autocompletionTimer = null;
		this.state = {
			source: "",
			dirty: false,
		};
	}

	editor() {
		return this.editorRef.current;
	}

	currentState() {
		if (this.editorView && this.editorView.viewState) {
			return this.editorView.viewState.state;
		}
	}

	sourceChanged = (source) => {
		this.setState(
			{
				source: source,
				dirty: true,
			},
			this.triggerOnChange
		);
	};

	triggerOnChange = () => {
		if (this.props.onChange) {
			clearTimeout(this.typingTimer);
			this.typingTimer = setTimeout(() => {
				this.props.onChange(this.state.source);
			}, 500);
		}
	};

	theme() {
		const appearance = ide.settings.section("appearance");
		const mode = appearance.section(appearance.get("mode"));
		const colors = mode.section("colors");
		const code = mode.section("code");
		const background = colors.get("background");
		const tokens = PromptParser.tokenTable;
		return createTheme({
			theme: appearance.get("mode"),
			settings: {
				fontSize: this.props.fontSize,
				fontFamily: appearance.get("fontFamily"),
				background: background,
				foreground: "#75baff",
				caret:
					mode.name === "light"
						? "black"
						: colors.get("primaryColor"),
				selection: code.get("selection"),
				selectionMatch: "#cccccc50",
				lineHighlight: "#8a91991a",
				gutterBackground: background,
				gutterBorder: background,
				gutterForeground: "#8a919966",
			},
			styles: [
				{
					tag: tokens.selector,
					color: code.get("selector"),
				},
				{
					tag: tokens.class,
					color: code.get("global"),
				},
				{
					tag: tokens.at,
					color: code.get("number"),
				},
				{
					tag: tokens.text,
					color: code.get("selector"),
				},
			],
		});
	}

	completionSource = async (context) => {
		const prefix = context.matchBefore(/[^\s]*/);
		if (!prefix || prefix.from === prefix.to) return null;
		const text = prefix.text.trim();
		if (text.length < 1) return null;
		if (text[0] !== "@") return null;
		let options, target, start;
		const index = text.indexOf("#");
		if (index > 0) {
			const classname = text.substring(1, index);
			let species;
			try {
				species = await ide.backend.classNamed(classname);
				if (!species) return null;
				target = text.substring(index + 1, text.length);
				if (target.length <= 0) return null;
				const selectors = await ide.backend.selectors(
					classname,
					target
				);
				options = selectors
					.filter((s) => s.startsWith(target))
					.map((s) => {
						return { label: s, detail: "", type: "selector" };
					});
				start = prefix.from + index + 1;
			} catch (ignored) {
				return null;
			}
		} else {
			try {
				target = text.substring(1, text.length);
				if (target.length <= 0) return null;
				const classnames = await ide.backend.searchClassNames(target);
				options = classnames.map((n) => {
					return { label: n, detail: "", type: "class" };
				});
				start = prefix.from + 1;
			} catch (ignored) {
				return null;
			}
		}
		options = options.filter((o) => o.label !== target);
		if (options.length <= 0) return null;
		return {
			from: start,
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

	extraKeys() {
		return [
			{
				key: "Tab",
				run: acceptCompletion,
			},
		];
	}

	render() {
		console.log("rendering prompt editor");
		const { source } = this.state;
		return (
			<Box style={{ width: "100%", height: "100%" }}>
				<Scrollable>
					<CodeMirror
						ref={this.editorRef}
						width="100%"
						height="100%"
						extensions={[
							StreamLanguage.define(PromptParser),
							EditorView.lineWrapping,
							Prec.highest(keymap.of(this.extraKeys())),
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
						// onContextMenu={(event) => {
						// 	this.openMenu(event);
						// }}
						onCreateEditor={(view, state) => {
							this.editorView = view;
						}}
						//onUpdate={(update) => this.editorUpdated(update)}
						basicSetup={{
							lineNumbers: false,
							closeBrackets: false,
							bracketMatching: false,
							highlightActiveLine: false,
							drawSelection: false,
							//autocompletion: true,
						}}
					/>
				</Scrollable>
			</Box>
		);
	}
}

export default PromptEditor;
