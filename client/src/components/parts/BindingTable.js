import React, { PureComponent } from "react";
import { Box, Paper } from "@material-ui/core";
import clsx from "clsx";
import { IDEContext } from "../IDEContext";
import CustomTable from "../controls/CustomTable";
import CodeEditor from "../parts/CodeEditor";
import Scrollable from "../controls/Scrollable";

class BindingTable extends PureComponent {
	static contextType = IDEContext;

	constructor(props) {
		super(props);
		this.state = {
			selectedBinding: null,
		};
	}

	// al actualizar deberia dejar seleccionado el binding con el mismo nombre que estaba seleccionado
	// const bindings = frame ? frame.bindings || [] : [];
	// const name = this.state.selectedBinding
	// 	? this.state.selectedBinding.name
	// 	: "self";

	bindingSelected = async (binding) => {
		this.setState({ selectedBinding: binding });
	};

	inspectBinding = async (binding) => {
		try {
			const context = {
				debugger: this.props.id,
				frame: this.props.frame ? this.props.frame.index : null,
			};
			const object = await this.context.evaluateExpression(
				binding.name,
				false,
				true,
				context
			);
			this.context.inspectObject(object);
		} catch (error) {
			this.context.reportError(error);
		}
	};

	evaluationContext() {
		const frame = this.state.selectedFrame;
		return frame
			? {
					debugger: this.props.id,
					frame: frame.index,
			  }
			: {};
	}

	bindingColor(binding) {
		let color;
		switch (binding.type) {
			case "argument":
				color = "#f06520";
				break;
			case "temporary":
				color = "#81c9f3";
				break;
			default:
				color = "#268bd2";
		}
		return color;
	}

	bindingColumns() {
		return [
			{
				field: "name",
				label: "Name",
				align: "left",
				color: (b) => {
					return this.bindingColor(b);
				},
			},
			{ field: "value", label: "Value", align: "left" },
		];
	}

	bindingOptions() {
		return [{ label: "Inspect", action: this.inspectBinding }];
	}

	render() {
		const { frame, styles } = this.props;
		const bindings = frame ? frame.bindings : [];
		const { selectedBinding } = this.state;
		const fixedHeightPaper = clsx(styles.paper, styles.fixedHeight);
		return (
			<Paper style={{ height: "100%" }} variant="outlined">
				<Box pb={1} flexGrow={1} height="80%">
					<CustomTable
						styles={styles}
						columns={this.bindingColumns()}
						rows={bindings}
						onSelect={this.bindingSelected}
						menuOptions={this.bindingOptions()}
					/>
				</Box>
				<Box>
					<CodeEditor
						styles={styles}
						lineNumbers={false}
						source={selectedBinding ? selectedBinding.value : ""}
						onAccept={this.saveBinding}
					/>
				</Box>
			</Paper>
		);
	}
}

export default BindingTable;
