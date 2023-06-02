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

class SettingEditor extends Component {
	constructor(props) {
		super(props);
		this.state = {
			value: this.props.setting.value || this.props.setting.default,
		};
	}

	valueChanged(value) {
		this.props.setting.value = value;
		this.setState({ value: value });
	}

	render() {
		const value = this.state.value;
		const setting = this.props.setting;
		const type = setting.type === "boolean" ? "checkbox" : setting.type;
		return (
			<Box
				mt={1}
				mb={1}
				display="flex"
				flexDirection="row"
				alignItems="center"
			>
				<Typography mr={2}>{setting.label}</Typography>
				{(setting.type === "text" ||
					setting.type === "number" ||
					setting.type === "color" ||
					setting.type === "url") && (
					<TextField
						sx={{ minWidth: 50 }}
						size="small"
						id={setting.name}
						type={type}
						placeholder={setting.description}
						margin="dense"
						name={setting.name}
						variant="outlined"
						value={value}
						onChange={(event) => {
							this.valueChanged(event.target.value);
						}}
						required
						disabled={!setting.editable}
					/>
				)}
				{setting.type === "boolean" && (
					<FormGroup>
						<FormControlLabel
							control={
								<Checkbox
									onChange={(event) =>
										this.valueChanged(event.target.value)
									}
									disabled={!setting.editable}
								/>
							}
							label={setting.label}
							size="small"
						/>
					</FormGroup>
				)}
				{setting.type === "options" && (
					<Select
						size="small"
						value={value}
						input={<OutlinedInput margin="dense" />}
						onChange={(event) => {
							this.valueChanged(event.target.value);
						}}
						disabled={!setting.editable}
					>
						{setting.options.map((option) => (
							<MenuItem value={option} key={option}>
								{option}
							</MenuItem>
						))}
					</Select>
				)}
			</Box>
		);
	}
}

export default SettingEditor;
