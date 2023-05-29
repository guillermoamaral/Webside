import React from "react";
import AlertDialog from "./AlertDialog";
import ConfirmDialog from "./ConfirmDialog";
import PromptDialog from "./PromptDialog";
import DialogContext from "./DialogContext";
import ListDialog from "./ListDialog";

class DialogProvider extends React.PureComponent {
	state = {
		dialog: { type: null },
	};

	handleClose = (value) => {
		const { dialog } = this.state;
		this.setState({ dialog: { ...dialog, open: false } });
		return dialog.type === "alert" || value
			? dialog.resolve(value)
			: dialog.reject();
	};

	alert = (options) => {
		return typeof options === "string"
			? new Promise((resolve, reject) => {
					this.setState({
						dialog: {
							message: options,
							type: "alert",
							resolve,
							reject,
							open: true,
						},
					});
			  })
			: new Promise((resolve, reject) => {
					this.setState({
						dialog: {
							...options,
							type: "alert",
							resolve,
							reject,
							open: true,
						},
					});
			  });
	};

	confirm = (options) => {
		return typeof options === "string"
			? new Promise((resolve, reject) => {
					this.setState({
						dialog: {
							message: options,
							type: "confirm",
							resolve,
							reject,
							open: true,
						},
					});
			  })
			: new Promise((resolve, reject) => {
					this.setState({
						dialog: {
							...options,
							type: "confirm",
							resolve,
							reject,
							open: true,
						},
					});
			  });
	};

	prompt = (options) => {
		return typeof options === "string"
			? new Promise((resolve, reject) => {
					this.setState({
						dialog: {
							message: options,
							type: "prompt",
							resolve,
							reject,
							open: true,
						},
					});
			  })
			: new Promise((resolve, reject) => {
					this.setState({
						dialog: {
							...options,
							type: "prompt",
							resolve,
							reject,
							open: true,
						},
					});
			  });
	};

	list = (options) => {
		return Array.isArray(options)
			? new Promise((resolve, reject) => {
					this.setState({
						dialog: {
							items: options,
							type: "list",
							resolve,
							reject,
							open: true,
						},
					});
			  })
			: new Promise((resolve, reject) => {
					this.setState({
						dialog: {
							...options,
							type: "list",
							resolve,
							reject,
							open: true,
						},
					});
			  });
	};

	dialog = {
		alert: this.alert,
		confirm: this.confirm,
		prompt: this.prompt,
		list: this.list,
	};

	render() {
		const { children } = this.props;
		const { dialog } = this.state;
		return (
			<DialogContext.Provider value={{ dialog: this.dialog }}>
				{children}
				{dialog.type === "alert" && (
					<AlertDialog
						{...dialog}
						open={dialog.open}
						onClose={this.handleClose}
					/>
				)}
				{dialog.type === "confirm" && (
					<ConfirmDialog
						{...dialog}
						open={dialog.open}
						onClose={this.handleClose}
					/>
				)}
				{dialog.type === "prompt" && (
					<PromptDialog
						{...dialog}
						open={dialog.open}
						onClose={this.handleClose}
					/>
				)}
				{dialog.type === "list" && (
					<ListDialog
						{...dialog}
						open={dialog.open}
						onClose={this.handleClose}
					/>
				)}
			</DialogContext.Provider>
		);
	}
}

export default DialogProvider;
