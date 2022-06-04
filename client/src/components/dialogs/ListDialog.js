import React from "react";
import PropTypes from "prop-types";
import withStateHandlers from "recompose/withStateHandlers";
import Dialog from "@material-ui/core/Dialog";
import DialogActions from "@material-ui/core/DialogActions";
import DialogContent from "@material-ui/core/DialogContent";
import DialogContentText from "@material-ui/core/DialogContentText";
import DialogTitle from "@material-ui/core/DialogTitle";
import Button from "@material-ui/core/Button";
import { List, ListItem, ListItemText } from "@material-ui/core";

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
				<List
					//onKeyDown={this.keyPressed}
					style={{ paddingTop: 0, paddingBottom: 0 }}
				>
					{items.map((item, index) => {
						return (
							<ListItem
								style={{
									paddingTop: 0,
									paddingBottom: 0,
									paddingLeft: 0,
									paddingRight: 0,
								}}
								button
								key={"item" + index}
								selected={value === item}
								onClick={(event) => handleChange(item)}
							>
								<ListItemText primary={item} />
							</ListItem>
						);
					})}
				</List>
			</DialogContent>
			<DialogActions>
				<Button
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
	handleChange: PropTypes.func.isRequired,
};

ListDialog.defaultProps = {
	open: false,
	title: "Select an item",
	placeholder: "",
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
