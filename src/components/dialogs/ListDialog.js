import React, { useEffect, useState } from "react";
import PropTypes from "prop-types";
import {
	Dialog,
	DialogActions,
	DialogContent,
	DialogContentText,
	DialogTitle,
	Button,
	Box,
} from "@mui/material";
import CustomList from "../controls/CustomList";

function ListDialog(props, context) {
	const {
		open = false,
		onClose,
		title = "Select an item",
		message,
		items,
		ok = {},
		cancel = {},
		required = false,
		defaultValue,
		filter,
	} = props;
	const [value, setValue] = useState(defaultValue);
	useEffect(() => {
		setValue(defaultValue);
	}, [defaultValue]);
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
				<Box style={{ height: 200 }}>
					<CustomList
						autoFocus
						items={items}
						selectedItem={value}
						onItemSelect={setValue}
						filterAlwaysPresent={filter}
						onItemDoubleClick={(item) => onClose(item)}
					/>
				</Box>
			</DialogContent>
			<DialogActions>
				<Button
					type="submit"
					onClick={() => onClose(value)}
					color={ok.color || "primary"}
					variant="outlined"
					disabled={required && !value}
					startIcon={ok.startIcon}
					endIcon={ok.endIcon}
					autoFocus
				>
					{ok.text || "Ok"}
				</Button>
				<Button
					onClick={() => onClose(null)}
					color={cancel.color || "inherit"}
					variant="outlined"
					startIcon={cancel.startIcon}
					endIcon={cancel.endIcon}
				>
					{cancel.text || "Cancel"}
				</Button>
			</DialogActions>
		</Dialog>
	);
}

ListDialog.propTypes = {
	open: PropTypes.bool.isRequired,
	onClose: PropTypes.func.isRequired,
	title: PropTypes.string,
	message: PropTypes.node,
	items: PropTypes.array.isRequired,
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
};

export default ListDialog;
