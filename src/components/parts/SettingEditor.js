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
} from "@mui/material";
import ShortcutEditor from "./ShortcutEditor";

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
		const { type, name, label, description, editable, options } =
			this.props.setting;
		return (
			<Box
				mt={1}
				mb={1}
				display="flex"
				flexDirection="row"
				alignItems="center"
			>
				<Typography mr={2}>{label}</Typography>
				{(type === "text" ||
					type === "paragraph" ||
					type === "number" ||
					type === "color" ||
					type === "url") && (
					<TextField
						fullWidth={type === "paragraph"}
						multiline={type === "paragraph"}
						sx={{ minWidth: 50 }}
						size="small"
						id={name}
						type={type}
						placeholder={description}
						margin="dense"
						name={name}
						variant="outlined"
						value={value}
						inputProps={{ id: name + "key" }}
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
							label={label !== description ? description : ""}
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
