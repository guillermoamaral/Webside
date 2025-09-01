import React from "react";
import Scrollable from "../controls/Scrollable.js";
import PopupMenu from "../controls/PopupMenu";
//import { ide } from "../IDE.js";
import CodeMirrorMerge from "react-codemirror-merge";
import { EditorView, keymap } from "@codemirror/view";
import { CodeMirrorEditor } from "./CodeMirrorEditor";
import { lintGutter } from "@codemirror/lint";
import { Box } from "@mui/material";
import { Prec } from "@codemirror/state";

const Original = CodeMirrorMerge.Original;
const Modified = CodeMirrorMerge.Modified;

class CodeMirrorDiffEditor extends CodeMirrorEditor {
	constructor(props) {
		super(props);
		this.ref = React.createRef();
		this.state = {
			leftSource: "",
			rightSource: "",
			menuOpen: false,
			menuPosition: { x: null, y: null },
		};
	}

	static getDerivedStateFromProps(props, state) {
		if (
			props.leftSource !== state.leftSource ||
			props.rightSource !== state.rightSource
		) {
			return {
				leftSource: props.leftSource || "",
				rightSource: props.rightSource || "",
			};
		}
		return null;
	}

	shouldComponentUpdate(nextProps, nextState) {
		return (
			nextProps.leftSource !== this.props.leftSource ||
			nextProps.rightSource !== this.props.rightSource ||
			nextProps.highlightChanges !== this.state.highlightChanges
		);
	}

	render() {
		console.log("rendering code merge");
		const { leftSource, rightSource, menuOpen, menuPosition } = this.state;
		const highlightChanges = this.props.highlightChanges;
		const theme = this.theme();
		const menuOptions = this.menuOptions();
		return (
			<Box
				display="flex"
				flexDirection="column"
				style={{ height: "100%" }}
			>
				<Scrollable>
					<CodeMirrorMerge
						width="100%"
						height="100%"
						orientation="a-b"
						gutter={false}
						highlightChanges={highlightChanges}
						onContextMenu={(event) => {
							this.openMenu(event);
						}}
					>
						<Original
							value={leftSource}
							extensions={[
								this.lexer(),
								EditorView.lineWrapping,
								lintGutter(),
								theme,
								Prec.highest(keymap.of(this.extraKeys())),
							]}
							basicSetup={{
								lineNumbers: false,
							}}
						/>
						<Modified
							value={rightSource}
							extensions={[
								this.lexer(),
								EditorView.lineWrapping,
								lintGutter(),
								theme,
								Prec.highest(keymap.of(this.extraKeys())),
							]}
							basicSetup={{
								lineNumbers: false,
							}}
						/>
					</CodeMirrorMerge>
					{menuOptions && menuOptions.length > 0 && (
						<PopupMenu
							options={menuOptions}
							open={menuOpen}
							position={menuPosition}
							onClose={this.closeMenu}
							onOptionClick={this.menuOptionClicked}
						/>
					)}
				</Scrollable>
			</Box>
		);
	}
}

export default CodeMirrorDiffEditor;
