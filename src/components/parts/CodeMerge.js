import React, { Component } from "react";
import Scrollable from "../controls/Scrollable.js";
import PopupMenu from "../controls/PopupMenu";
import { ide } from "../IDE.js";
import CodeMirrorMerge from "react-codemirror-merge";
import { StreamLanguage } from "@codemirror/language";
import { smalltalk } from "@codemirror/legacy-modes/mode/smalltalk";
import { material } from "@uiw/codemirror-theme-material";
import { EditorView } from "@codemirror/view";

const Original = CodeMirrorMerge.Original;
const Modified = CodeMirrorMerge.Modified;

class CodeMerge extends Component {
	constructor(props) {
		super(props);
		this.ref = React.createRef();
		this.state = {
			leftCode: "",
			rightCode: "",
			menuOpen: false,
			menuPosition: { x: null, y: null },
		};
	}

	static getDerivedStateFromProps(props, state) {
		return {
			leftCode: props.leftCode || "",
			rightCode: props.rightCode || "",
		};
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

	menuOptions() {
		const shortcuts = ide.settings.section("shortcuts");
		return [
			{ label: "Copy (Ctrl+c)", action: this.copyToClipboard },
			{ label: "Paste (Ctrl+v)", action: this.pasteFromClipboard },
			null,
			{
				label: "Do it (" + shortcuts.get("evaluateExpression") + ")",
				action: this.evaluateExpression,
			},
			{
				label: "Print it (" + shortcuts.get("showEvaluation") + ")",
				action: this.showEvaluation,
			},
			{
				label:
					"Inspect it (" + shortcuts.get("inspectEvaluation") + ")",
				action: this.inspectEvaluation,
			},
			{
				label: "Debug it (" + shortcuts.get("debugExpression") + ")",
				action: this.debugExpression,
			},
			{ label: "Profile it", action: this.profileExpression },
			{ label: "Google it", action: this.searchInGoogle },
			null,
			{
				label: "Browse class (" + shortcuts.get("browseClass") + ")",
				action: this.browseClass,
			},
			{
				label:
					"Browse senders (" + shortcuts.get("browseSenders") + ")",
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
		];
	}

	render() {
		const { leftCode, rightCode, menuOpen, menuPosition } = this.state;
		return (
			<Scrollable>
				<CodeMirrorMerge
					width="100%"
					height="100%"
					onContextMenu={(event) => {
						this.openMenu(event);
					}}
				>
					<Original
						value={leftCode}
						extensions={[
							StreamLanguage.define(smalltalk),
							EditorView.lineWrapping,
							material,
						]}
					/>
					<Modified
						value={rightCode}
						extensions={[
							StreamLanguage.define(smalltalk),
							EditorView.lineWrapping,
							material,
						]}
					/>
				</CodeMirrorMerge>
				<PopupMenu
					options={this.menuOptions()}
					open={menuOpen}
					position={menuPosition}
					onClose={this.closeMenu}
				/>
			</Scrollable>
		);
	}
}

export default CodeMerge;
