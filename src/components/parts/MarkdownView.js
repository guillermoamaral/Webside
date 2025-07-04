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

	linkClicked = (event) => {
		event.stopPropagation();
		if (this.props.onLinkClick) {
			event.preventDefault();
			this.props.onLinkClick(event.target.href);
		}
	};

	render() {
		const source = this.props.source;
		const appearance = ide.settings.section("appearance");
		const mode = appearance.get("mode");
		const background = appearance.section(mode).get("background");
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
				components={{
					a: ({ href, children }) => (
						<a
							href={href}
							target="_blank"
							rel="noopener noreferrer"
							onClick={this.linkClicked}
						>
							{children}
						</a>
					),
				}}
			/>
		);
	}
}

export default MarkdownView;
