import React, { Component } from "react";
import CustomList from "../controls/CustomList";
import CustomPaper from "../controls/CustomPaper";
import { ide } from "../IDE";

class UVariableList extends Component {

	constructor(props) {
		super(props);
		this.state = {
			class: null,
			variables: [],
			selectedVariable: null,
			loading: false,
		};
	}

	componentDidMount() {
		this.updateVariables();
	}

	componentDidUpdate(prevProps) {
		if (this.props.class !== prevProps.class) {
			this.updateVariables();
		}
	}

	async updateVariables() {
		this.setState({ loading: true });
		let variables = await this.fetchVariables();
		this.setState({
			loading: false,
			variables: variables,
			selectedVariable: null,
		});
	}

	async fetchVariables() {
		let variables = [];
		let species = this.props.class;
		if (!species) return variables;
		if (species.template) return variables;
		try {
			variables = await ide.backend.variables(species.name);;
		} catch (error) {
			ide.reportError(error);
		}
		return variables;
	}

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
		let selected = variable.type === "separator" ? null : variable;
		this.setState({ selectedVariable: selected })
		if (this.props.onVariableSelect) {
			this.props.onVariableSelect(selected);
		}
	};

	addVariable = async () => {
		try {
			const name = await ide.prompt({
				title: "New instance variable",
				required: true,
			});
			await ide.backend.addInstanceVariable(this.props.class.name, name);
			let variables = await this.fetchVariables();
			let variable = variables.find(v => v.name === name);
			this.setState({ variables: variables, selectedVariable: variable })
			if (this.props.onVariableAdd) {
				this.props.onVariableAdd(variable);
			}
		} catch (error) {
			ide.reportError(error);
		}
	};

	renameVariable = async (variable) => {
		if (!variable) {
			return;
		}
		try {
			const newName = await ide.prompt({
				title: "Rename variable",
				defaultValue: variable.name,
				required: true,
			});
			if (variable.type === "instance") {
				await ide.backend.renameInstanceVariable(
					this.props.class.name,
					variable.name,
					newName
				);
			} else {
				await ide.backend.renameClassVariable(
					this.props.class.name,
					variable.name,
					newName
				);
			}
			variable.name = newName;
			this.setState({ selectedVariable: variable })
			if (this.props.onVariableRename) {
				this.props.onVariableRename(variable);
			}
		} catch (error) {
			ide.reportError(error);
		}
	};

	removeVariable = async (variable) => {
		if (!variable) {
			return;
		}
		try {
			if (variable.type === "instance") {
				await ide.backend.removeInstanceVariable(
					variable.class,
					variable.name
				);
			} else {
				await ide.backend.removeClassVariable(
					variable.class,
					variable.name
				);
			}
			let variables = await this.fetchVariables();
			this.setState({ variables: variables, selectedVariable: null })
			if (this.props.onVariableRemove) {
				this.props.onVariableRemove(variable);
			}
		} catch (error) {
			ide.reportError(error);
		}
	};

	moveVariableUp = async (variable) => {
		if (!variable) {
			return;
		}
		try {
			await ide.backend.moveInstanceVariableUp(
				this.props.class.name,
				variable.name
			);
			let variables = await this.fetchVariables();
			let moved = variables.find(v => v.name === variable.name);
			this.setState({ variables: variables, selectedVariable: moved })
			if (this.props.onMoveUp) {
				this.props.onMoveUp(moved);
			}
		} catch (error) {
			ide.reportError(error);
		}
	};

	moveVariableDown = async (variable, target) => {
		if (!variable) {
			return;
		}
		try {
			await ide.backend.moveInstanceVariableDown(
				this.props.class.name,
				variable.name,
				target
			);
			let variables = await this.fetchVariables();
			let moved = variables.find(v => v.name === variable.name);
			this.setState({ variables: variables, selectedVariable: moved })
			if (this.props.onVariableRemove) {
				this.props.onVariableRemove(moved);
			}
		} catch (error) {
			ide.reportError(error);
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
		let { variables, selectedVariable, loading } = this.state;
		variables = this.extendedVariables(variables);
		return (
			<CustomPaper>
				<CustomList
					loading={loading}
					sx={{ height: "100%" }}
					itemLabel="name"
					itemDivider={(item) => item.type === "separator"}
					labelStyle={(item) =>
						item.type === "separator" ? "italic" : "normal"
					}
					labelSize={(item) =>
						item.type === "separator" ? "small" : "normal"
					}
					items={variables}
					selectedItem={selectedVariable}
					onItemSelect={this.variableSelected}
					menuOptions={this.menuOptions()}
				/>
			</CustomPaper>
		);
	}
}

export default UVariableList;
