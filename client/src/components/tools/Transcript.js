import React, { Component } from "react";
import CodeEditor from "../parts/CodeEditor";
//import { Paper } from "@material-ui/core";
import { ide } from "../IDE";

class Transcript extends Component {
	render() {
		return (
			//<Paper variant="outlined" style={{ minHeight: 100 }}>
			<CodeEditor
				styles={this.props.styles}
				source={ide.transcriptText()}
				onChange={this.props.onChange}
				showAccept={false}
			/>
			//</Paper>
		);
	}
}

export default Transcript;
