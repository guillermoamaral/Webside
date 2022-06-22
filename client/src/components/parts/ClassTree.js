import React, { Component } from "react";
import CustomTree from "../controls/CustomTree";
import { IDEContext } from "../IDEContext";
import { withDialog } from "../dialogs/index";
//import Scrollable from "../controls/Scrollable";

class ClassTree extends Component {
	static contextType = IDEContext;

	newSubclass = async (superclass) => {
		if (!superclass) {
			return;
		}
		try {
			const name = await this.props.dialog.prompt({
				title: "New " + superclass.name + " subclass",
				required: true,
			});
			//This is not correct as the class might be indexed from its creation...
			const packagename = superclass.package;
			const definition =
				superclass.name +
				" subclass: #" +
				name +
				" instanceVariableNames: '' classVariableNames: '' poolDictionaries: ''";
			await this.context.api.defineClass(name, packagename, definition);
			const species = await this.context.api.getClass(name);
			const handler = this.props.onCreate;
			if (handler) {
				handler(species);
			}
		} catch (error) {
			this.context.reportError(error);
		}
	};

	removeClass = async (species) => {
		if (!species) {
			return;
		}
		try {
			const confirm = await this.props.dialog.confirm({
				title: "Delete " + species.name + " class?",
				ok: { text: "Delete", color: "secondary", variant: "outlined" },
			});
			if (!confirm) {
				return;
			}
			await this.context.api.removeClass(species.name);
			const handler = this.props.onRemove;
			if (handler) {
				handler(species);
			}
		} catch (error) {
			this.context.reportError(error);
		}
	};

	renameClass = async (species) => {
		if (!species) {
			return;
		}
		try {
			const newName = await this.props.dialog.prompt({
				title: "Rename class",
				defaultValue: species.name,
				required: true,
			});
			await this.context.api.renameClass(species.name, newName);
			species.name = newName;
			const handler = this.props.onRename;
			if (handler) {
				handler(species);
			}
		} catch (error) {
			this.context.reportError(error);
		}
	};

	browseClass = (species) => {
		if (species) {
			this.context.browseClass(species.name);
		}
	};

	browsePackage = (species) => {
		if (species) {
			this.context.browsePackage(species.package);
		}
	};

	browseClassReferences = (species) => {
		if (species) {
			this.context.browseClassReferences(species.name);
		}
	};

	runTests = (species) => {
		if (species) {
			this.context.runTestClass(species.name);
		}
	};

	menuOptions() {
		return [
			{ label: "New", action: this.newSubclass },
			{ label: "Rename", action: this.renameClass },
			{ label: "Remove", action: this.removeClass },
			null,
			{ label: "Browse", action: this.browseClass },
			{ label: "Browse package", action: this.browsePackage },
			{ label: "References", action: this.browseClassReferences },
			null,
			{ label: "Test", action: this.runTests },
		];
	}

	render() {
		return (
			<CustomTree
				items={this.props.roots || []}
				itemLabel="name"
				itemStyle={this.props.labelStyle}
				children={"subclasses"}
				onExpand={this.props.onExpand}
				onSelect={this.props.onSelect}
				selectedItem={this.props.selected}
				menuOptions={this.menuOptions()}
			/>
		);
	}
}
export default withDialog()(ClassTree);
