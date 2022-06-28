import React, { Component } from "react";
import Snackbar from "@material-ui/core/Snackbar";
import Alert from "@material-ui/lab/Alert";

class CustomSnacks extends Component {
	constructor(props) {
		super(props);
		this.state = { open: props.open };
	}

	closeClicked = () => {
		const handler = this.props.onClose;
		if (handler) {
			handler();
		} else {
			this.setState({ open: false });
		}
	};

	render() {
		const { text, severity, open } = this.props;
		return (
			<Snackbar open={open} autoHideDuration={5000} onClose={this.closeClicked}>
				<Alert
					onClose={this.closeClicked}
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
