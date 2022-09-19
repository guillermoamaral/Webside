import React, { Component } from "react";
import CodeEditor from "../parts/CodeEditor";

class Transcript extends Component {
	render() {
		return (
			<CodeEditor
				styles={this.props.styles}
				source={this.context.transcriptText}
				onChange={this.props.onChange}
				showAccept={false}
			/>
		);
	}
}

export default Transcript;
