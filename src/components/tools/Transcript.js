import React from "react";
import Tool from "./Tool";
import CodeMirrorEditor from "../parts/CodeMirrorEditor";
import { Box } from "@mui/material";
import { ide } from "../IDE";

class Transcript extends Tool {
	aboutToSelect() {
		super.aboutToSelect();
		ide.resetUnredErrorCount();
		this.forceUpdate();
	}

	render() {
		const background = ide.colorSetting("transcriptColor");
		return (
			<Box
				sx={{
					minHeight: 100,
					height: "100%",
					padding: 1,
					background: background,
				}}
			>
				<CodeMirrorEditor
					source={ide.transcriptText()}
					onChange={this.props.onChange}
				/>
			</Box>
		);
	}
}

export default Transcript;
