import React, { useState } from "react";
import PropTypes from "prop-types";
import {
	Dialog,
	DialogActions,
	DialogContent,
	DialogContentText,
	DialogTitle,
	Button,
	TextField,
} from "@mui/material";

function PromptDialog(props, context) {
	const {
		open,
		onClose,
		title,
		message,
		placeholder,
		ok,
		cancel,
		required,
		defaultValue,
	} = props;
	const [value, setValue] = useState(defaultValue);
	return (
		<Dialog
			fullWidth
			open={open}
			onClose={() => onClose(null)}
			aria-labelledby="prompt-dialog-title"
			aria-describedby="prompt-dialog-message"
		>
			<DialogTitle id="prompt-dialog-title">{title}</DialogTitle>
			<DialogContent>
				{typeof message === `string` ? (
					<DialogContentText id="confirm-dialog-message">
						{message}
					</DialogContentText>
				) : (
					message
				)}
				<TextField
					id="prompt-dialog-text-field"
					onChange={(event) => {
						setValue(event.target.value);
					}}
					defaultValue={defaultValue}
					required
					placeholder={placeholder}
					margin="dense"
					fullWidth
					autoFocus
					variant="outlined"
					onKeyPress={(event) => {
						if (event.key === "Enter") {
							onClose(value);
						}
					}}
				/>
			</DialogContent>
			<DialogActions>
				<Button
					type="submit"
					onClick={() => onClose(value)}
					color={ok.color}
					variant={ok.variant}
					disabled={required && !value}
					startIcon={ok.startIcon}
					endIcon={ok.endIcon}
				>
					{ok.text}
				</Button>
				<Button
					onClick={() => onClose(null)}
					color={cancel.color}
					variant={cancel.variant}
					startIcon={cancel.startIcon}
					endIcon={cancel.endIcon}
				>
					{cancel.text}
				</Button>
			</DialogActions>
		</Dialog>
	);
}

PromptDialog.propTypes = {
	open: PropTypes.bool.isRequired,
	onClose: PropTypes.func.isRequired,
	title: PropTypes.string,
	message: PropTypes.node,
	placeholder: PropTypes.string,
	ok: PropTypes.shape({
		text: PropTypes.string,
		color: PropTypes.string,
		variant: PropTypes.string,
		startIcon: PropTypes.element,
		endIcon: PropTypes.element,
	}),
	cancel: PropTypes.shape({
		text: PropTypes.string,
		color: PropTypes.string,
		variant: PropTypes.string,
		startIcon: PropTypes.element,
		endIcon: PropTypes.element,
	}),
	required: PropTypes.bool,
	defaultValue: PropTypes.oneOfType([PropTypes.string, PropTypes.number]),
	value: PropTypes.string,
};

PromptDialog.defaultProps = {
	open: false,
	title: "Enter",
	placeholder: "",
	ok: {
		text: "OK",
		color: "primary",
		variant: "outlined",
	},
	cancel: {
		text: "Cancel",
		color: "inherit",
		variant: "outlined",
	},
	required: false,
};

export default PromptDialog;
