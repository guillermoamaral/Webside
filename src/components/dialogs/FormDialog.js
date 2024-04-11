import React, { useState, useEffect } from "react";
import PropTypes from "prop-types";
import {
	Dialog,
	DialogActions,
	DialogContent,
	DialogContentText,
	DialogTitle,
	Button,
} from "@mui/material";
import CustomInput from "../controls/CustomInput";

function FormDialog(props, context) {
	const { open, onClose, title, message, inputs, ok, cancel } = props;

	const initialValues = {};
	const initialErrors = {};
	inputs.forEach((i) => {
		let defaultValue =
			i.defaultValue === undefined && i.type === "boolean"
				? false
				: i.defaultValue;
		initialValues[i.name] = defaultValue;
		initialErrors[i.name] = "";
	});

	const [values, setValues] = useState(initialValues);
	useEffect(() => setValues(initialValues), [inputs]);
	const [errors, setErrors] = useState(initialErrors);
	useEffect(() => setErrors(initialErrors), [inputs]);

	const inputNamed = (name) => {
		return inputs.find((i) => i.name === name);
	};

	const isValidValue = (input, value) => {
		if (!input.required) return true;
		if (value === undefined) return false;
		if (input.type === "boolean") return true;
		return value.length > 0;
	};

	const validateInput = (input, value) => {
		return isValidValue(input, value)
			? ""
			: "A valid value for \"" + input.label + "\" must be provided";
	};

	const setValue = (name, value) => {
		let newValues = { ...values };
		newValues[name] = value;
		let input = inputNamed(name);
		let newErrors = { ...errors };
		newErrors[name] = validateInput(input, value);
		setErrors(newErrors);
		setValues(newValues);
	};

	const submit = () => {
		let valid = true;
		let newErrors = { ...errors };
		let error;
		inputs.forEach((i) => {
			error = validateInput(i, values[i.name]);
			if (error.length > 0) {
				newErrors[i.name] = error;
				valid = false;
			}
		});
		valid ? onClose(values) : setErrors(newErrors);
	};

	return (
		<Dialog
			fullWidth
			open={open}
			onClose={() => onClose(null)}
			aria-labelledby="alert-dialog-title"
			aria-describedby="alert-dialog-message"
		>
			<DialogTitle id="alert-dialog-title">{title}</DialogTitle>
			<DialogContent dividers={true}>
				{typeof message === `string` ? (
					<DialogContentText id="confirm-dialog-message">
						{message}
					</DialogContentText>
				) : (
					message
				)}
				{inputs.map((i) => {
					return (
						<CustomInput
							key={i.name}
							name={i.name}
							type={i.type}
							defaultValue={i.defaultValue}
							label={i.label || i.name}
							options={i.options}
							description={i.description}
							onValueChange={(v) => setValue(i.name, v)}
							errorText={errors[i.name]}
						/>
					);
				})}
			</DialogContent>
			<DialogActions>
				<Button
					type="submit"
					onClick={submit}
					color={ok.color}
					variant={ok.variant}
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

FormDialog.propTypes = {
	open: PropTypes.bool.isRequired,
	onClose: PropTypes.func.isRequired,
	title: PropTypes.string,
	message: PropTypes.node,
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
};

FormDialog.defaultProps = {
	open: false,
	title: "Form",
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
};

export default FormDialog;
