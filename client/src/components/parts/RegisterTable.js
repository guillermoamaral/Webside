import React, { Component } from "react";
import CustomTable from "../controls/CustomTable";
import { IDEContext } from "../IDEContext";

class RegisterTable extends Component {
	static contextType = IDEContext;

	registerSelected = (frame) => {
		if (this.props.onSelect) {
			this.props.onSelect(frame);
		}
	};

	menuOptions() {
		return [{ label: "Inspect", action: this.inspect }];
	}

	inspect = async (register) => {
		try {
			await this.context.api.pinNativeDebuggerRegister(
				this.props.debugger,
				register.name
			);
			const object = await this.context.api.getObject(register.name);
			this.context.inspectObject(object);
		} catch (error) {
			this.context.reportError(error);
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
		const styles = this.props.styles;
		return (
			<CustomTable
				styles={styles}
				columns={this.registerColumns()}
				rows={this.props.registers}
				onSelect={this.registerSelected}
				menuOptions={this.menuOptions()}
			/>
		);
	}
}

export default RegisterTable;
