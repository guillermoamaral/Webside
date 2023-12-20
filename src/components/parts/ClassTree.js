import React, { Component } from "react";
import { ide } from "../IDE";
import ToolContainerContext from "../ToolContainerContext";
import FastTree from "../controls/FastTree";

class ClassTree extends Component {
	static contextType = ToolContainerContext;

	newClass = async (superclass) => {
		if (!superclass) {
			return;
		}
		try {
			const name = await ide.prompt({
				title: "New " + superclass.name + " subclass",
				required: true,
			});
			await ide.backend.defineClass(
				name,
				superclass.name,
				superclass.package
			);
			const species = await ide.backend.classNamed(name);
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
			await ide.backend.removeClass(species.name);
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
			await ide.waitFor(() => ide.backend.renameClass(species.name, newName));
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

	migrateClass = (species) => {
		if (species) {
			this.context.migrateClass(species.name);
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
		const { roots, labelStyle, selectedClass, onClassSelect, expandedClasses, onClassExpand, onClassCollapse } = this.props;
		return (
			<FastTree
				nodes={roots || []}
				nodeId="name"
				nodeLabel="name"
				nodeStyle={labelStyle}
				nodeChildren="subclasses"
				selectedNode={selectedClass}
				onNodeSelect={onClassSelect}
				expandedNodes={expandedClasses}
				onNodeExpand={onClassExpand}
				onNodeCollapse={onClassCollapse}
				menuOptions={this.menuOptions()}
			/>
		);
	}
}
export default ClassTree;
