import React, { PureComponent } from "react";
import { Box, Paper } from "@material-ui/core";
import { IDEContext } from "../IDEContext";
import CustomTable from "../controls/CustomTable";
import CodeEditor from "../parts/CodeEditor";
//import Scrollable from "../controls/Scrollable";

class BindingTable extends PureComponent {
	static contextType = IDEContext;

	constructor(props) {
		super(props);
		this.state = {
			selectedBinding: null,
		};
	}

	static getDerivedStateFromProps(props, state) {
		if (props.frame) {
			const name = state.selectedBinding ? state.selectedBinding.name : "self";
			const selected = (props.frame.bindings || []).find(
				(b) => b.name === name
			);
			return {
				selectedBinding: selected,
			};
		} else {
			return null;
		}
	}

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
		const frame = this.props.frame;
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

	bindingValue(binding) {
		const value = binding.value;
		const max = 100;
		return value.length > max ? value.substr(0, 99) + "…" : value;
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
			{
				field: (b) => {
					return this.bindingValue(b);
				},
				label: "Value",
				align: "left",
			},
		];
	}

	bindingOptions() {
		return [{ label: "Inspect", action: this.inspectBinding }];
	}

	render() {
		const { frame, styles } = this.props;
		const bindings = frame ? frame.bindings : [];
		const { selectedBinding } = this.state;
		const percent = selectedBinding ? 70 : 100;
		return (
			<Box style={{ height: "100%" }}>
				<Box pb={1} flexGrow={1} height={percent + "%"}>
					<Paper variant="outlined" style={{ height: "100%" }}>
						<CustomTable
							styles={styles}
							columns={this.bindingColumns()}
							rows={bindings}
							onSelect={this.bindingSelected}
							menuOptions={this.bindingOptions()}
						/>
					</Paper>
				</Box>
				{selectedBinding && (
					<Box pb={1} height={100 - percent + "%"}>
						<Paper variant="outlined" style={{ height: "100%" }}>
							<CodeEditor
								styles={styles}
								lineNumbers={false}
								source={selectedBinding ? selectedBinding.value : ""}
								//onAccept={this.saveBinding}
								context={this.evaluationContext()}
							/>
						</Paper>
					</Box>
				)}
			</Box>
		);
	}
}

export default BindingTable;
