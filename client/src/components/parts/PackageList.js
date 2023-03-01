import React, { Component } from "react";
import FastCustomList from "../controls/FastCustomList";
import { ide } from "../IDE";
import { withDialog } from "../dialogs/index";

class PackageList extends Component {
	createPackage = async () => {
		try {
			const name = await this.props.dialog.prompt({
				title: "New package",
				required: true,
			});
			await ide.api.createPackage(name);
			const pack = await ide.api.packageNamed(name);
			if (this.props.onCreate) {
				this.props.onCreate(pack);
			}
		} catch (error) {
			ide.reportError(error);
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
			await ide.api.removePackage(pack.name);
			if (this.props.onRemove) {
				this.props.onRemove(pack);
			}
		} catch (error) {
			ide.reportError(error);
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
			await ide.api.renamePackage(pack.name, newName);
			pack.name = newName;
			if (this.props.onRename) {
				this.props.onRename(pack);
			}
		} catch (error) {
			ide.reportError(error);
		}
	};

	runTests = (pack) => {
		if (pack) {
			ide.runTestPackage(pack.name);
		}
	};

	migratePackage = (pack) => {
		if (pack) {
			ide.migratePackage(pack.name);
		}
	};

	menuOptions() {
		return [
			{ label: "New", action: this.createPackage },
			{ label: "Rename", action: this.renamePackage },
			{ label: "Remove", action: this.removePackage },
			null,
			{ label: "Test", action: this.runTests },
			null,
			{ label: "Migrate", action: this.migratePackage },
		];
	}

	render() {
		return (
			<FastCustomList
				enableFilter={false}
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
