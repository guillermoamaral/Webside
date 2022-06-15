import React, { Component } from "react";
import CodeEditor from "../parts/CodeEditor";

class Transcript extends Component {
	constructor(props) {
		super(props);
		this.state = {
			text: props.text,
		};
	}

	textChanged = (text) => {
		const handler = this.props.onChange;
		if (handler) {
			handler(text);
		}
		this.setState({ text: text });
	};

	render() {
		return (
			<CodeEditor
				styles={this.props.styles}
				source={this.state.text}
				onChange={this.textChanged}
				showAccept={false}
			/>
		);
	}
}

export default Transcript;
