import React, { Component } from "react";
import CustomTable from "../controls/CustomTable";
import { ide } from "../IDE";
import { container } from "../ToolsContainer";

class RegisterTable extends Component {
	registerSelected = (frame) => {
		if (this.props.onRegisterSelect) {
			this.props.onRegisterSelect(frame);
		}
	};

	menuOptions() {
		return [{ label: "Inspect", action: this.inspect }];
	}

	inspect = async (register) => {
		try {
			await ide.api.pinNativeDebuggerRegister(
				this.props.debugger,
				register.name
			);
			const object = await ide.api.objectWithId(register.name);
			container.openInspector(object);
		} catch (error) {
			ide.reportError(error);
		}
	};

	registerColums() {
		return [
			{ field: "name", label: "Register", align: "left" },
			{
				field: (r) => {
					return r.value.toString(16).toUpperCase();
				},
				label: "Native",
				align: "right",
			},
			{ field: "object", label: "Object", align: "left" },
		];
	}

	render() {
		return (
			<CustomTable
				columns={this.registerColumns()}
				rows={this.props.registers}
				onRowSelect={this.registerSelected}
				menuOptions={this.menuOptions()}
			/>
		);
	}
}

export default RegisterTable;
