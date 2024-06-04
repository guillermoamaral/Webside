import React, { Component } from "react";
import { ide } from "../IDE";
import MarkdownPreview from "@uiw/react-markdown-preview";

class MarkdownView extends Component {
	componentDidMount() {
		ide.onColorModeChange(this.colorModeChanged);
	}

	componentWillUnmount() {
		ide.removeColorModeChangeHandler(this.colorModeChanged);
	}

	colorModeChanged = () => {
		this.forceUpdate();
	};

	render() {
		const source = this.props.source;
		const appearance = ide.settings.section("appearance");
		const mode = appearance.get("mode");
		const background = appearance
			.section(mode)
			.section("colors")
			.get("background");
		return (
			<MarkdownPreview
				source={source}
				style={{
					padding: 16,
					backgroundColor: background,
				}}
				wrapperElement={{
					"data-color-mode": mode,
				}}
			/>
		);
	}
}

export default MarkdownView;
