import React from "react";
import Tool from "./Tool";
import CodeEditor from "../parts/CodeEditor";
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
				<CodeEditor
					source={ide.transcriptText()}
					onChange={this.props.onChange}
				/>
			</Box>
		);
	}
}

export default Transcript;
