import React, { Component } from "react";
import CodeEditor from "../parts/CodeEditor";
import { ide } from "../IDE";

class Transcript extends Component {
	render() {
		return (
			<CodeEditor
				styles={this.props.styles}
				source={ide.transcriptText()}
				onChange={this.props.onChange}
				showAccept={false}
			/>
		);
	}
}

export default Transcript;
