import React, { Component } from "react";
import CustomList from "../controls/CustomList";
import { ide } from "../IDE";
import ToolContainerContext from "../ToolContainerContext";

class PackageList extends Component {
	static contextType = ToolContainerContext;

	createPackage = async () => {
		try {
			const name = await ide.prompt({
				title: "New package",
				required: true,
			});
			await ide.backend.createPackage(name);
			const pack = await ide.backend.packageNamed(name);
			if (this.props.onPackageCreate) {
				this.props.onPackageCreate(pack);
			}
		} catch (error) {
			ide.reportError(error);
		}
	};

	removePackage = async (pack) => {
		if (!pack) {
			return;
		}
		const confirm = await ide.confirm({
			title: "Delete " + pack.name + " package?",
			ok: { text: "Delete", color: "secondary", variant: "outlined" },
		});
		if (!confirm) {
			return;
		}
		try {
			await ide.backend.removePackage(pack.name);
			if (this.props.onPackageRemove) {
				this.props.onPackageRemove(pack);
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
			const newName = await ide.prompt({
				title: "Rename package",
				defaultValue: pack.name,
				required: true,
			});
			await ide.backend.renamePackage(pack.name, newName);
			pack.name = newName;
			if (this.props.onPackageRename) {
				this.props.onPackageRename(pack);
			}
		} catch (error) {
			ide.reportError(error);
		}
	};

	runTests = (pack) => {
		if (pack) {
			this.context.runTestPackage(pack.name);
		}
	};

	migratePackage = (pack) => {
		if (pack) {
			this.context.migratePackage(pack.name);
		}
	};

	menuOptions() {
		return [
			{ label: "New", action: this.createPackage },
			{ label: "Rename", action: this.renamePackage },
			{ label: "Remove", action: this.removePackage },
			null,
			{ label: "Run tests", action: this.runTests },
			null,
			{ label: "Migrate", action: this.migratePackage },
		];
	}

	render() {
		return (
			<CustomList
				enableFilter={false}
				items={this.props.packages}
				itemLabel="name"
				selectedItem={this.props.selectedPackage}
				onItemSelect={this.props.onPackageSelect}
				menuOptions={this.menuOptions()}
			/>
		);
	}
}
export default PackageList;
