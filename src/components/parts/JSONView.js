import React, { Component } from "react";
import { ide } from "../IDE";
import JsonView from "@uiw/react-json-view";
import { lightTheme } from "@uiw/react-json-view/light";
import { darkTheme } from "@uiw/react-json-view/dark";

class JSONView extends Component {
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
		const background = appearance.section(mode).get("background");
		const theme = mode === "dark" ? darkTheme : lightTheme;
		return (
			<JsonView
				value={source}
				displayDataTypes={false}
				displayObjectSize={false}
				style={{ ...theme, backgroundColor: background }}
				//collapsed
			/>
		);
	}
}

export default JSONView;
