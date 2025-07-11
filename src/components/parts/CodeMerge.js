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

class CodeMerge extends CodeMirrorEditor {
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
		if (
			props.leftCode !== state.leftCode ||
			props.rightCode !== state.rightCode
		) {
			return {
				leftCode: props.leftCode || "",
				rightCode: props.rightCode || "",
			};
		}
		return null;
	}

	shouldComponentUpdate(nextProps, nextState) {
		return (
			nextProps.leftCode !== this.props.leftCode ||
			nextProps.rightCode !== this.props.rightCode ||
			nextProps.highlightChanges !== this.state.highlightChanges
		);
	}

	openMenu = (event) => {
		event.preventDefault();
		event.stopPropagation();
		this.setState({
			menuOpen: true,
			menuPosition: { x: event.clientX - 2, y: event.clientY - 4 },
		});
	};

	closeMenu = () => {
		this.setState({ menuOpen: false });
	};

	render() {
		console.log("rendering code merge");
		const { leftCode, rightCode, menuOpen, menuPosition } = this.state;
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
							value={leftCode}
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
							value={rightCode}
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
						/>
					)}
				</Scrollable>
			</Box>
		);
	}
}

export default CodeMerge;
