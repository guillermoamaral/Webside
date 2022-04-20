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
		const handler = this.props.onSelect;
		if (handler) {
			handler(selected);
		}
	};

	addVariable = async () => {
		try {
			const name = await this.props.dialog.prompt({
				title: "New variable",
				required: true,
			});
			await this.context.api.addInstanceVariable(this.props.class.name, name);
			const handler = this.props.onAdd;
			if (handler) {
				handler();
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
			await this.context.api.renameInstanceVariable(
				this.props.class.name,
				variable.name,
				newName
			);
			variable.name = newName;
			const handler = this.props.onRename;
			if (handler) {
				handler(variable, newName);
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
			await this.context.api.deleteInstanceVariable(
				this.props.class.name,
				variable.name
			);
			const handler = this.props.onRemove;
			if (handler) {
				handler();
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
			const handler = this.props.onMoveUp;
			if (handler) {
				handler(variable);
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
			const handler = this.props.onRemove;
			if (handler) {
				handler(variable);
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
				items={variables}
				selectedItem={this.props.selectedVariable}
				onSelect={this.variableSelected}
				menuOptions={this.menuOptions()}
			/>
		);
	}
}

export default withDialog()(VariableList);
