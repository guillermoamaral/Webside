import React from "react";
import PropTypes from "prop-types";
import withStateHandlers from "recompose/withStateHandlers";
import {
	Dialog,
	DialogActions,
	DialogContent,
	DialogContentText,
	DialogTitle,
	Button,
	Paper,
} from "@material-ui/core";
import CustomList from "../controls/CustomList";

function ListDialog(props, context) {
	const {
		open,
		onClose,
		onExited,
		title,
		message,
		items,
		ok,
		cancel,
		required,
		value,
		handleChange,
	} = props;
	return (
		<Dialog
			fullWidth
			open={open}
			onClose={() => onClose(null)}
			onExited={onExited}
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
				<Paper variant="outlined">
					<CustomList
						autoFocus
						items={items}
						selectedItem={value}
						onSelect={handleChange}
					/>
				</Paper>
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

ListDialog.propTypes = {
	open: PropTypes.bool.isRequired,
	onClose: PropTypes.func.isRequired,
	onExited: PropTypes.func.isRequired,
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
	value: PropTypes.string,
	handleChange: PropTypes.func.isRequired,
};

ListDialog.defaultProps = {
	open: false,
	title: "Select an item",
	ok: {
		text: "OK",
		color: "primary",
		variant: "outlined",
	},
	cancel: {
		text: "Cancel",
		color: "default",
		variant: "outlined",
	},
	required: false,
};

export default withStateHandlers(
	({ defaultValue }) => ({ value: defaultValue }),
	{ handleChange: (state) => (value) => ({ value: value }) }
)(ListDialog);
