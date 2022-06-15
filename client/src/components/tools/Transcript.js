import React, { Component } from "react";
import CodeEditor from "../parts/CodeEditor";
import { IDEContext } from "../IDEContext";

class Transcript extends Component {
	static contextType = IDEContext;
	
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
