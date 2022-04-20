import React from "react";
import AlertDialog from "./AlertDialog";
import ConfirmDialog from "./ConfirmDialog";
import PromptDialog from "./PromptDialog";
import DialogContext from "./DialogContext";
import ListDialog from "./ListDialog";

class DialogProvider extends React.PureComponent {
	state = {
		alertDialog: null,
		confirmDialog: null,
		promptDialog: null,
		listDialog: null,
	};

	handleAlertDialogClose = (value) => {
		const { alertDialog } = this.state;
		this.setState({ alertDialog: { ...alertDialog, open: false } });
		return alertDialog.resolve(value);
	};

	handleConfirmDialogClose = (value) => {
		const { confirmDialog } = this.state;
		this.setState({ confirmDialog: { ...confirmDialog, open: false } });
		return value ? confirmDialog.resolve(value) : confirmDialog.reject();
	};

	handlePromptDialogClose = (value) => {
		const { promptDialog } = this.state;
		this.setState({ promptDialog: { ...promptDialog, open: false } });
		return value ? promptDialog.resolve(value) : promptDialog.reject();
	};

	handleListDialogClose = (value) => {
		const { listDialog } = this.state;
		this.setState({ listDialog: { ...listDialog, open: false } });
		return value ? listDialog.resolve(value) : listDialog.reject();
	};

	handleExited = () => {
		this.setState({
			alertDialog: null,
			confirmDialog: null,
			promptDialog: null,
			listDialog: null,
		});
	};

	alert = (options) => {
		return typeof options === "string"
			? new Promise((resolve, reject) => {
					this.setState({
						alertDialog: { message: options, resolve, reject, open: true },
					});
			  })
			: new Promise((resolve, reject) => {
					this.setState({
						alertDialog: { ...options, resolve, reject, open: true },
					});
			  });
	};

	confirm = (options) => {
		return typeof options === "string"
			? new Promise((resolve, reject) => {
					this.setState({
						confirmDialog: { message: options, resolve, reject, open: true },
					});
			  })
			: new Promise((resolve, reject) => {
					this.setState({
						confirmDialog: { ...options, resolve, reject, open: true },
					});
			  });
	};

	prompt = (options) => {
		return typeof options === "string"
			? new Promise((resolve, reject) => {
					this.setState({
						promptDialog: { message: options, resolve, reject, open: true },
					});
			  })
			: new Promise((resolve, reject) => {
					this.setState({
						promptDialog: { ...options, resolve, reject, open: true },
					});
			  });
	};

	list = (options) => {
		return Array.isArray(options)
			? new Promise((resolve, reject) => {
					this.setState({
						listDialog: { items: options, resolve, reject, open: true },
					});
			  })
			: new Promise((resolve, reject) => {
					this.setState({
						listDialog: { ...options, resolve, reject, open: true },
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
		const { alertDialog, confirmDialog, promptDialog, listDialog } = this.state;
		return (
			<DialogContext.Provider value={{ dialog: this.dialog }}>
				{children}
				{alertDialog && (
					<AlertDialog
						{...alertDialog}
						open={alertDialog.open}
						onClose={this.handleAlertDialogClose}
						onExited={this.handleExited}
					/>
				)}
				{confirmDialog && (
					<ConfirmDialog
						{...confirmDialog}
						open={confirmDialog.open}
						onClose={this.handleConfirmDialogClose}
						onExited={this.handleExited}
					/>
				)}
				{promptDialog && (
					<PromptDialog
						{...promptDialog}
						open={promptDialog.open}
						onClose={this.handlePromptDialogClose}
						onExited={this.handleExited}
					/>
				)}
				{listDialog && (
					<ListDialog
						{...listDialog}
						open={listDialog.open}
						onClose={this.handleListDialogClose}
						onExited={this.handleExited}
					/>
				)}
			</DialogContext.Provider>
		);
	}
}

export default DialogProvider;
