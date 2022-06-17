import React, { Component } from "react";
import FastCustomList from "../controls/FastCustomList";
import { IDEContext } from "../IDEContext";
import { withDialog } from "../dialogs/index";

class PackageList extends Component {
	static contextType = IDEContext;

	createPackage = async () => {
		try {
			const name = await this.props.dialog.prompt({
				title: "New package",
				required: true,
			});
			const pack = await this.context.api.createPackage(name);
			const handler = this.props.onCreate;
			if (handler) {
				handler(pack);
			}
		} catch (error) {
			this.context.reportError(error);
		}
	};

	removePackage = async (pack) => {
		if (!pack) {
			return;
		}
		const confirm = await this.props.dialog.confirm({
			title: "Delete " + pack.name + " package?",
			ok: { text: "Delete", color: "secondary", variant: "outlined" },
		});
		if (!confirm) {
			return;
		}
		try {
			await this.context.api.deletePackage(pack.name);
			const handler = this.props.onRemove;
			if (handler) {
				handler(pack);
			}
		} catch (error) {
			this.context.reportError(error);
		}
	};

	renamePackage = async (pack) => {
		if (!pack) {
			return;
		}
		try {
			const newName = await this.props.dialog.prompt({
				title: "Rename package",
				defaultValue: pack.name,
				required: true,
			});
			await this.context.api.renamePackage(pack.name, newName);
			pack.name = newName;
			const handler = this.props.onRename;
			if (handler) {
				handler(pack);
			}
		} catch (error) {
			this.context.reportError(error);
		}
	};

	runTests = (pack) => {
		if (pack) {
			this.context.runTestPackage(pack.name);
		}
	};

	menuOptions() {
		return [
			{ label: "New", action: this.createPackage },
			{ label: "Rename", action: this.renamePackage },
			{ label: "Remove", action: this.removePackage },
			null,
			{ label: "Test", action: this.runTests },
		];
	}

	render() {
		return (
			<FastCustomList
				items={this.props.packages}
				itemLabel="name"
				selectedItem={this.props.selected}
				onSelect={this.props.onSelect}
				menuOptions={this.menuOptions()}
			/>
		);
	}
}
export default withDialog()(PackageList);
