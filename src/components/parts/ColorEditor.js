import React, { Component } from "react";
import { Box, TextField, Typography, Slider } from "@mui/material";

class ColorEditor extends Component {
	constructor(props) {
		super(props);
		const rgba = this.props.value || "#ffffff00";
		this.state = {
			rgb: this.rgbFrom(rgba),
			transparency: this.transparencyFrom(rgba),
		};
	}

	rgbChanged(rgb) {
		this.setState({ rgb: rgb });
		if (this.props.onChange) this.props.onChange(this.rgba());
	}

	transparencyChanged(transparency) {
		this.setState({ transparency: transparency });
		if (this.props.onChange) this.props.onChange(this.rgba());
	}

	rgba() {
		const percent = Math.round((this.state.transparency * 255) / 100);
		const a = percent.toString(16).padStart(2, 0);
		return this.state.rgb + a;
	}

	rgbFrom(rgba) {
		return rgba.substring(0, 7);
	}

	transparencyFrom(rgba) {
		return rgba.length > 7
			? (Number("0x" + rgba.substring(7, 9)) / 255) * 100.0
			: 0;
	}

	render() {
		const { rgb, transparency } = this.state;
		const { name, editable } = this.props;
		return (
			<Box display="flex" flexDirection="row" alignItems="center">
				<Typography mr={2} sx={{ minWidth: 100 }}>
					{this.rgba()}
				</Typography>
				<TextField
					sx={{ minWidth: 50 }}
					size="small"
					id={name}
					type="color"
					margin="dense"
					name={name}
					variant="outlined"
					value={rgb}
					onChange={(event) => {
						this.rgbChanged(event.target.value);
					}}
					required
					disabled={!editable}
				/>
				<Box m={2}>
					<Typography variant="body2">Transparency</Typography>
				</Box>
				<Box display="flex" sx={{ minWidth: 100, marginRight: 2 }}>
					<Slider
						value={transparency}
						onChange={(event, value) => {
							this.transparencyChanged(value);
						}}
						valueLabelDisplay="auto"
						size="small"
					/>
				</Box>
			</Box>
		);
	}
}

export default ColorEditor;
