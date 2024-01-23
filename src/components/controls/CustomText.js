import React, { PureComponent } from "react";
import { Box, Typography } from "@mui/material";

class CustomText extends PureComponent {
	constructor(props) {
		super(props);
		this.state = {
			parts: [],
		};
	}

	componentDidMount() {
		this.setState({ parts: this.textParts(this.props.text) });
	}

	textParts(rawText) {
		let bold = false;
		let italic = false;
		let colored = false;
		let output = [];
		let text = rawText.split("").reduce((a, b) => {
			if (b === "*") {
				if (a !== "")
					output.push({
						text: a,
						bold: bold,
						italic: italic,
						colored: colored,
					});
				bold = !bold;
				return "";
			} else if (b === "_") {
				if (a !== "")
					output.push({
						text: a,
						bold: bold,
						italic: italic,
						colored: colored,
					});
				italic = !italic;
				return "";
			} else if (b === "<") {
				if (a !== "")
					output.push({
						text: a,
						bold: bold,
						italic: italic,
						colored: colored,
					});
				colored = true;
				return "";
			} else if (b === ">") {
				if (a !== "")
					output.push({
						text: a,
						bold: bold,
						italic: italic,
						colored: colored,
					});
				colored = false;
				return "";
			} else {
				return a + b;
			}
		}, "");
		if (text !== "")
			output.push({
				text: text,
				bold: bold,
				italic: italic,
				colored: colored,
			});
		return output;
	}

	render() {
		let parts = this.state.parts;
		return (
			<Box display="flex" flexDirection="row">
				{parts.map((part, i) => (
					<Typography
						{...this.props}
						mr={1}
						key={"part" + i}
						color={part.colored ? "primary" : "default"}
						fontWeight={
							part.bold ? "fontWeightBold" : "fontWeightRegular"
						}
						fontStyle={part.italic ? "italic" : "normal"}
					>
						{part.text}
					</Typography>
				))}
			</Box>
		);
	}
}

export default CustomText;
