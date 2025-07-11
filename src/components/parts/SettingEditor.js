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
	IconButton,
	Avatar,
} from "@mui/material";
import ShortcutEditor from "./ShortcutEditor";
import InfoIcon from "@mui/icons-material/InfoOutlined";
import ColorEditor from "./ColorEditor";
import FormatBoldIcon from "@mui/icons-material/FormatBold";
import FormatItalicIcon from "@mui/icons-material/FormatItalic";
import RefreshIcon from "@mui/icons-material/Refresh";
import { ide } from "../IDE";

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
		const setting = this.props.setting;
		const actual = setting.type === "number" ? Number(value) : value;
		setting.value = actual;
		this.setState({ value: actual });
		if (this.props.onValueChange) this.props.onValueChange();
	};

	loadImage = (e) => {
		const file = e.target.files[0];
		if (!file) return;
		const maxSize = 1024 * 200;
		if (file.size > maxSize) {
			ide.inform(
				`File size exceeds ${
					maxSize / 1024
				} KB. Please select a smaller file.`
			);
			e.target.value = "";
			return;
		}
		const reader = new FileReader();
		reader.onload = () => {
			const result = reader.result;
			this.valueChanged(result);
		};
		reader.readAsDataURL(file);
	};

	settingOptions() {
		const setting = this.props.setting;
		let options = [];
		if (setting.type !== "options") return options;
		options = setting.getOptions();
		if (!options.includes(setting.value))
			options = [setting.value, ...options];
		return options;
	}

	refreshSettingOptions = async () => {
		const setting = this.props.setting;
		if (setting && setting.refreshHandler) {
			const options = await setting.refreshHandler();
			setting.setOptions(options);
		}
		this.forceUpdate();
	};

	render() {
		const value = this.state.value;
		const { setting, showLabel = true, minLabelWidth = 150 } = this.props;
		const { type, name, label, description, editable } = setting;
		const alignment = type === "number" ? "right" : "left";
		const textStyleBoldItalic = [];
		if (type === "textStyle") {
			if (setting.italic) textStyleBoldItalic.push("italic");
			if (setting.bold) textStyleBoldItalic.push("bold");
		}
		return (
			<Box display="flex" flexDirection="row" alignItems="center">
				<Box
					display="flex"
					flexDirection="row"
					mr={2}
					sx={{ minWidth: minLabelWidth }}
				>
					{showLabel && (
						<Typography
							variant="body2"
							color={editable ? "text.primary" : "grey.500"}
						>
							{label}
						</Typography>
					)}
				</Box>
				<Box>
					{["text", "paragraph", "number", "url"].includes(type) && (
						<TextField
							fullWidth={type === "paragraph"}
							multiline={type === "paragraph"}
							sx={{ minWidth: 250 }}
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
											this.valueChanged(
												event.target.checked
											)
										}
										disabled={!editable}
									/>
								}
								//label={label}
								size="small"
							/>
						</FormGroup>
					)}
					{type === "options" && (
						<Box display="flex" flexDirection="row">
							<Select
								size="small"
								value={value}
								input={<OutlinedInput margin="dense" />}
								onChange={(event) => {
									this.valueChanged(event.target.value);
								}}
								disabled={!editable}
								sx={{ minWidth: 250 }}
							>
								{this.settingOptions().map((option) => (
									<MenuItem value={option} key={option}>
										{option}
									</MenuItem>
								))}
							</Select>
							{setting.refreshHandler && (
								<IconButton
									onClick={this.refreshSettingOptions}
								>
									<RefreshIcon fontSize="small" />
								</IconButton>
							)}
						</Box>
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
									this.valueChanged(); // Trigger re-render
								}}
							/>
							<ToggleButtonGroup
								size="small"
								value={textStyleBoldItalic}
								onChange={(a, s) => {
									setting.italic = s.includes("italic");
									setting.bold = s.includes("bold");
									this.valueChanged(); // Trigger re-render
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
					{type === "image" && (
						<IconButton component="label">
							<Avatar
								alt={description}
								src={value}
								sx={{ width: 56, height: 56 }}
							/>
							<input
								type="file"
								accept="image/*"
								style={{ display: "none" }}
								onChange={this.loadImage}
							/>
						</IconButton>
					)}
				</Box>
				{description && (
					<Box ml={1} display="flex" alignItems="center">
						<Tooltip key={label} title={description}>
							<InfoIcon fontSize="small" />
						</Tooltip>
					</Box>
				)}
			</Box>
		);
	}
}

export default SettingEditor;
