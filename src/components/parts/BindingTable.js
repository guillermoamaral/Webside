import React, { PureComponent } from "react";
import { Box, Paper } from "@mui/material";
import CustomTable from "../controls/CustomTable";
import CodeEditor from "../parts/CodeEditor";
import { ide } from "../IDE";
import ToolContainerContext from "../ToolContainerContext";

class BindingTable extends PureComponent {
	static contextType = ToolContainerContext;

	constructor(props) {
		super(props);
		this.state = {
			selectedBinding: null,
		};
	}

	static getDerivedStateFromProps(props, state) {
		if (props.frame) {
			const name = state.selectedBinding
				? state.selectedBinding.name
				: "self";
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
			const object = await this.context.evaluateExpression(
				binding.name,
				false,
				true,
				this.evaluationContext()
			);
			this.context.openInspector(object);
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
		const appearance = ide.settings.section("appearance");
		const mode = appearance.section(appearance.get("mode"));
		const code = mode.section("code");
		return code.get(binding.type) || "#268bd2";
	}

	bindingValue(binding) {
		const value = binding.value || "error retrieving value";
		const max = 100;
		return value.length > max ? value.substr(0, 99) + "…" : value;
	}

	bindingColumns() {
		return [
			{
				field: "name",
				label: "Name",
				align: "left",
				link: (b) => {
					this.inspectBinding(b);
				},
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
		const { frame } = this.props;
		const bindings = frame ? frame.bindings : [];
		const { selectedBinding } = this.state;
		return (
			<Box
				display="flex"
				flexDirection="column"
				justifyContent="center"
				style={{ height: "100%" }}
			>
				<Box pb={1} height={"80%"}>
					<Paper variant="outlined" style={{ height: "100%" }}>
						<CustomTable
							noHeaders
							columns={this.bindingColumns()}
							rows={bindings}
							onRowSelect={this.bindingSelected}
							menuOptions={this.bindingOptions()}
						/>
					</Paper>
				</Box>
				<Box height={"20%"}>
					<Paper variant="outlined" style={{ height: "100%" }}>
						<CodeEditor
							source={
								selectedBinding ? selectedBinding.value : ""
							}
							//onAccept={this.saveBinding}
							context={this.evaluationContext()}
						/>
					</Paper>
				</Box>
			</Box>
		);
	}
}

export default BindingTable;
