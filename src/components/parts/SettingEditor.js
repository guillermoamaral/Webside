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
} from "@mui/material";
import ShortcutEditor from "./ShortcutEditor";
import InfoIcon from "@mui/icons-material/InfoOutlined";

class SettingEditor extends Component {
	constructor(props) {
		super(props);
		let value = this.props.setting.value;
		if (value === null) {
			value = this.props.setting.default;
		}
		this.state = { value: value };
	}

	valueChanged(value) {
		this.props.setting.value = value;
		this.setState({ value: value });
	}

	render() {
		const value = this.state.value;
		const { setting, showLabel = true } = this.props;
		const { type, name, label, description, editable, options } = setting;
		const alignment = type === "number" ? "right" : "left";
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
				{["text", "paragraph", "number", "color", "url"].includes(
					type
				) && (
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
			</Box>
		);
	}
}

export default SettingEditor;
