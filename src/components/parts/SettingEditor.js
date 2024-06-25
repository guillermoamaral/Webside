import React, { Component } from "react";
import {
	Box,
	Typography,
	TextField,
	Checkbox,
	Select,
	MenuItem,
	OutlinedInput,
	FormGroup,
	FormControlLabel,
	Tooltip,
	ToggleButton,
	ToggleButtonGroup,
} from "@mui/material";
import ShortcutEditor from "./ShortcutEditor";
import InfoIcon from "@mui/icons-material/InfoOutlined";
import ColorEditor from "./ColorEditor";
import FormatBoldIcon from "@mui/icons-material/FormatBold";
import FormatItalicIcon from "@mui/icons-material/FormatItalic";

class SettingEditor extends Component {
	constructor(props) {
		super(props);
		let value = this.props.setting.value;
		if (value === null) {
			value = this.props.setting.default;
		}
		this.state = { value: value };
	}

	valueChanged = (value) => {
		this.props.setting.value = value;
		this.setState({ value: value });
	};

	render() {
		const value = this.state.value;
		const { setting, showLabel = true } = this.props;
		const { type, name, label, description, editable, options } = setting;
		const alignment = type === "number" ? "right" : "left";
		const textStyleBoldItalic = [];
		if (type === "textStyle") {
			if (setting.italic) textStyleBoldItalic.push("italic");
			if (setting.bold) textStyleBoldItalic.push("bold");
		}
		return (
			<Box display="flex" flexDirection="row" alignItems="center">
				{description && (
					<Tooltip key={label} title={description}>
						<InfoIcon />
					</Tooltip>
				)}
				{showLabel && type !== "boolean" && (
					<Typography variant="body2" ml={1} mr={2}>
						{label}
					</Typography>
				)}
				{["text", "paragraph", "number", "url"].includes(type) && (
					<TextField
						fullWidth={type === "paragraph"}
						multiline={type === "paragraph"}
						sx={{ minWidth: 50 }}
						size="small"
						id={name}
						type={type}
						placeholder={"Enter " + label}
						margin="dense"
						name={name}
						variant="outlined"
						value={value}
						inputProps={{
							id: name + "key",
							style: { textAlign: alignment },
						}}
						onChange={(event) => {
							this.valueChanged(event.target.value);
						}}
						required
						disabled={!editable}
					/>
				)}
				{type === "boolean" && (
					<FormGroup>
						<FormControlLabel
							control={
								<Checkbox
									checked={value}
									onChange={(event) =>
										this.valueChanged(event.target.checked)
									}
									disabled={!editable}
								/>
							}
							label={label}
							size="small"
						/>
					</FormGroup>
				)}
				{type === "options" && (
					<Select
						size="small"
						value={value}
						input={<OutlinedInput margin="dense" />}
						onChange={(event) => {
							this.valueChanged(event.target.value);
						}}
						disabled={!editable}
					>
						{options.map((option) => (
							<MenuItem value={option} key={option}>
								{option}
							</MenuItem>
						))}
					</Select>
				)}
				{type === "shortcut" && (
					<ShortcutEditor
						value={value}
						onChange={(value) => this.valueChanged(value)}
					/>
				)}
				{type === "color" && (
					<ColorEditor
						name={name}
						value={value}
						editable={editable}
						onChange={this.valueChanged}
					/>
				)}
				{type === "textStyle" && (
					<Box display="flex" flexDirection="row">
						<ColorEditor
							name={name}
							value={setting.color}
							editable={editable}
							onChange={(rgba) => {
								setting.color = rgba;
								this.forceUpdate();
							}}
						/>
						<ToggleButtonGroup
							size="small"
							value={textStyleBoldItalic}
							onChange={(a, s) => {
								setting.italic = s.includes("italic");
								setting.bold = s.includes("bold");
								this.forceUpdate();
							}}
						>
							<ToggleButton value="bold" size="small">
								<FormatBoldIcon fontSize="small" />
							</ToggleButton>
							<ToggleButton value="italic" size="small">
								<FormatItalicIcon fontSize="small" />
							</ToggleButton>
						</ToggleButtonGroup>
					</Box>
				)}
			</Box>
		);
	}
}

export default SettingEditor;
