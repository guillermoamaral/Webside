import React, { Component } from "react";
import { Snackbar, Alert, Button } from "@mui/material";

class CustomSnacks extends Component {
	constructor(props) {
		super(props);
		this.state = { open: props.open };
	}

	closeClicked = () => {
		this.setState({ open: false }, () => {
			if (this.props.onClose) {
				this.props.onClose();
			}
		});
	};

	actionClicked = () => {
		this.closeClicked();
		this.props.action.handler();
	};

	render() {
		const { text, severity, open, action } = this.props;
		return (
			<Snackbar
				open={open}
				autoHideDuration={5000}
				onClose={this.closeClicked}
			>
				<Alert
					onClose={this.closeClicked}
					action={
						action && (
							<Button
								size="small"
								color="inherit"
								onClick={this.actionClicked}
							>
								{action.label}
							</Button>
						)
					}
					variant="filled"
					elevation={6}
					severity={severity || "info"}
					sx={{ width: "100%" }}
				>
					{text}
				</Alert>
			</Snackbar>
		);
	}
}

export default CustomSnacks;
