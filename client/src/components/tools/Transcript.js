import React, { Component } from "react";
import CodeEditor from "../parts/CodeEditor";
import { Box } from "@material-ui/core";
import { ide } from "../IDE";

class Transcript extends Component {
	render() {
		return (
			<Box style={{ minHeight: 100, height: 300 }}>
				<CodeEditor
					styles={this.props.styles}
					source={ide.transcriptText()}
					onChange={this.props.onChange}
					showAccept={false}
				/>
			</Box>
		);
	}
}

export default Transcript;
