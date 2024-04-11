import React, { Component } from "react";
import {
	Box,
	Typography,
	TextField,
	Checkbox,
	FormGroup,
	FormControlLabel,
} from "@mui/material";
import CustomList from "./CustomList";

class CustomInput extends Component {
	constructor(props) {
		super(props);
		this.state = { value: this.props.defaultValue };
	}

	valueChanged = (value) => {
		this.setState({ value: value });
		if (this.props.onValueChange) this.props.onValueChange(value);
	};
	render() {
		const value = this.state.value;
		const { name, type, label, description, options, errorText } =
			this.props;
		return (
			<Box>
				{!options && (
					<Box
						mt={1}
						mb={1}
						display="flex"
						flexDirection="row"
						alignItems="center"
					>
						<Typography mr={2}>{label + ":"}</Typography>
						<Box flexGrow={1}>
							{(type === "text" || type === "number") && (
								<TextField
									sx={{ minWidth: 50 }}
									size="small"
									id={name}
									type={type}
									placeholder={description}
									margin="dense"
									name={name}
									variant="outlined"
									value={value || ""}
									inputProps={{ id: name + "key" }}
									onChange={(event) => {
										this.valueChanged(event.target.value);
									}}
									error={errorText.length > 0}
									helperText={errorText}
								/>
							)}
							{type === "boolean" && (
								<FormGroup>
									<FormControlLabel
										control={
											<Checkbox
												checked={
													value === undefined
														? false
														: value
												}
												onChange={(event) =>
													this.valueChanged(
														event.target.checked
													)
												}
											/>
										}
										label={
											label !== description
												? description
												: ""
										}
										size="small"
									/>
								</FormGroup>
							)}
						</Box>
					</Box>
				)}
				{options && (
					<Box mt={1} mb={1} display="flex" flexDirection="column">
						<Typography mr={2}>{label}</Typography>
						<Box
							style={{
								height: 200,
								border: errorText ? "1px solid red" : "none",
								borderRadius: 4,
							}}
						>
							<CustomList
								autoFocus
								items={options}
								selectedItem={value}
								onItemSelect={this.valueChanged}
								filterAlwaysPresent={true}
							/>
						</Box>
						{errorText.length > 0 && (
							<Typography variant="caption" color="error">
								{errorText}
							</Typography>
						)}
					</Box>
				)}
			</Box>
		);
	}
}

export default CustomInput;
