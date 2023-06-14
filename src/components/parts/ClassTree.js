import React, { Component } from "react";
import CustomTree from "../controls/CustomTree";
import { ide } from "../IDE";
import { container } from "../ToolsContainer";
//import Scrollable from "../controls/Scrollable";

class ClassTree extends Component {
	newClass = async (superclass) => {
		if (!superclass) {
			return;
		}
		try {
			const name = await ide.prompt({
				title: "New " + superclass.name + " subclass",
				required: true,
			});
			const packagename = superclass.package;
			await ide.api.defineClass(name, superclass.name, packagename);
			const species = await ide.api.classNamed(name);
			if (this.props.onClassDefine) {
				this.props.onClassDefine(species);
			}
		} catch (error) {
			ide.reportError(error);
		}
	};

	removeClass = async (species) => {
		if (!species) {
			return;
		}
		try {
			const confirm = await ide.confirm({
				title: "Delete " + species.name + " class?",
				ok: { text: "Delete", color: "secondary", variant: "outlined" },
			});
			if (!confirm) {
				return;
			}
			await ide.api.removeClass(species.name);
			if (this.props.onClassRemove) {
				this.props.onClassRemove(species);
			}
		} catch (error) {
			ide.reportError(error);
		}
	};

	renameClass = async (species) => {
		if (!species) {
			return;
		}
		try {
			const newName = await ide.prompt({
				title: "Rename class",
				defaultValue: species.name,
				required: true,
			});
			await ide.api.renameClass(species.name, newName);
			species.name = newName;
			if (this.props.onClassRename) {
				this.props.onClassRename(species);
			}
		} catch (error) {
			ide.reportError(error);
		}
	};

	browseClass = (species) => {
		if (species) {
			container.browseClass(species.name);
		}
	};

	browsePackage = (species) => {
		if (species) {
			container.browsePackage(species.package);
		}
	};

	browseClassReferences = (species) => {
		if (species) {
			container.browseClassReferences(species.name);
		}
	};

	runTests = (species) => {
		if (species) {
			container.runTestClass(species.name);
		}
	};

	migrateClass = (species) => {
		if (species) {
			container.migrateClass(species.name);
		}
	};

	menuOptions() {
		return [
			{ label: "New", action: this.newClass },
			{ label: "Rename", action: this.renameClass },
			{ label: "Remove", action: this.removeClass },
			null,
			{ label: "Browse", action: this.browseClass },
			{ label: "Browse package", action: this.browsePackage },
			{ label: "Browse references", action: this.browseClassReferences },
			null,
			{ label: "Run tests", action: this.runTests },
			null,
			{ label: "Migrate", action: this.migrateClass },
		];
	}

	render() {
		return (
			<CustomTree
				items={this.props.roots || []}
				itemLabel="name"
				itemStyle={this.props.labelStyle}
				children={"subclasses"}
				onItemExpand={this.props.onClassExpand}
				onItemSelect={this.props.onClassSelect}
				selectedItem={this.props.selectedClass}
				menuOptions={this.menuOptions()}
			/>
		);
	}
}
export default ClassTree;
