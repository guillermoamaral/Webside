import React, { Component } from "react";
import CustomList from "../controls/CustomList";
import { IDEContext } from "../IDEContext";
import { withDialog } from "../dialogs/index";

class VariableList extends Component {
	static contextType = IDEContext;

	extendedVariables(variables) {
		let extended = [];
		if (variables) {
			const groups = {};
			variables.forEach((v) => {
				if (!groups[v.class]) {
					groups[v.class] = [];
				}
				groups[v.class].push(v);
			});
			Object.keys(groups).forEach((c) => {
				extended.push({ name: c, type: "separator" });
				groups[c].forEach((v) => extended.push(v));
			});
		}
		return extended;
	}

	variableSelected = (variable) => {
		const selected = variable.type === "separator" ? null : variable;
		if (this.props.onSelect) {
			this.props.onSelect(this.props.onSelect);
		}
	};

	addVariable = async () => {
		try {
			const name = await this.props.dialog.prompt({
				title: "New variable",
				required: true,
			});
			await this.context.api.addInstanceVariable(this.props.class.name, name);
			if (this.props.onAdd) {
				this.props.onAdd();
			}
		} catch (error) {
			this.context.reportError(error);
		}
	};

	renameVariable = async (variable) => {
		if (!variable) {
			return;
		}
		try {
			const newName = await this.props.dialog.prompt({
				title: "Rename variable",
				defaultValue: variable.name,
				required: true,
			});
			console.log(variable);
			if (variable.type === "instance") {
				await this.context.api.renameInstanceVariable(
					this.props.class.name,
					variable.name,
					newName
				);
			} else {
				await this.context.api.renameClassVariable(
					this.props.class.name,
					variable.name,
					newName
				);
			}
			variable.name = newName;
			if (this.props.onRename) {
				this.props.onRename(variable, newName);
			}
		} catch (error) {
			this.context.reportError(error);
		}
	};

	removeVariable = async (variable) => {
		if (!variable) {
			return;
		}
		try {
			if (variable.type === "instance") {
				await this.context.api.removeInstanceVariable(
					this.props.class.name,
					variable.name
				);
			} else {
				await this.context.api.removeClassVariable(
					this.props.class.name,
					variable.name
				);
			}
			if (this.props.onRemove) {
				this.props.onRemove();
			}
		} catch (error) {
			this.context.reportError(error);
		}
	};

	moveVariableUp = async (variable) => {
		if (!variable) {
			return;
		}
		try {
			await this.context.api.moveInstanceVariableUp(
				this.props.class.name,
				variable.name
			);
			if (this.props.onMoveUp) {
				this.props.onMoveUp(variable);
			}
		} catch (error) {
			this.context.reportError(error);
		}
	};

	moveVariableDown = async (variable, target) => {
		if (!variable) {
			return;
		}
		try {
			await this.context.api.moveInstanceVariableDown(
				this.props.class.name,
				variable.name,
				target
			);
			if (this.props.onRemove) {
				this.props.onRemove(variable);
			}
		} catch (error) {
			this.context.reportError(error);
		}
	};

	menuOptions() {
		const options = [
			{ label: "Add", action: this.addVariable },
			{ label: "Rename", action: this.renameVariable },
			{ label: "Remove", action: this.removeVariable },
			{ label: "Move to superclass", action: this.moveVariableUp },
		];
		if (this.props.class && this.props.class.subclasses) {
			options.push({
				label: "Move to subclass",
				suboptions: this.props.class.subclasses.map((c) => {
					return {
						label: c.name,
						action: (v) => this.moveVariableDown(v, c.name),
					};
				}),
			});
		}
		return options;
	}

	render() {
		const variables = this.extendedVariables(this.props.variables);
		return (
			<CustomList
				itemLabel="name"
				itemDivider={(item) => item.type === "separator"}
				labelStyle={(item) => (item.type === "separator" ? "italic" : "normal")}
				labelSize={(item) => (item.type === "separator" ? "small" : "normal")}
				items={variables}
				selectedItem={this.props.selected}
				onSelect={this.variableSelected}
				menuOptions={this.menuOptions()}
			/>
		);
	}
}

export default withDialog()(VariableList);
