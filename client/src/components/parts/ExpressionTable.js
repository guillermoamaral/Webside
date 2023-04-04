import React, { PureComponent } from "react";
import { Box, Paper, IconButton } from "@material-ui/core";
import { ide } from "../IDE";
import CustomTable from "../controls/CustomTable";
//import { DataGrid } from "@mui/x-data-grid";
import CodeEditor from "../parts/CodeEditor";
import AddIcon from "@material-ui/icons/AddCircle";
import DeleteIcon from "@material-ui/icons/Delete";
import { withDialog } from "../dialogs/index";

class ExpressionTable extends PureComponent {
	constructor(props) {
		super(props);
		this.state = {
			expressions: [],
			selectedExpression: null,
		};
	}

	expressionSelected = async (expression) => {
		this.setState({ selectedExpression: expression });
	};

	inspectExpression = async (expression) => {
		try {
			const object = await ide.evaluateExpression(
				expression.sourceCode,
				false,
				true,
				this.evaluationContext()
			);
			ide.openInspector(object);
		} catch (error) {
			ide.reportError(error);
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

	expressionValue(expression) {
		const value = expression.value;
		const max = 100;
		return value.length > max ? value.substr(0, 99) + "…" : value;
	}

	expressionColumns() {
		return [
			{
				field: "sourceCode",
				label: "Expression",
				align: "left",
				color: "#268bd2",
				editable: true,
			},
			{
				field: (e) => {
					return this.expressionValue(e);
				},
				link: (e) => {
					this.inspectExpression(e);
				},
				label: "Value",
				align: "left",
			},
		];
	}

	// expressionColumns() {
	// 	return [
	// 		{
	// 			field: "expression",
	// 			headerName: "Expression",
	// 			editable: true,
	// 		},
	// 		{
	// 			field: "value",
	// 			headerName: "Value",
	// 			editable: false,
	// 		},
	// 		{
	// 			field: "delete",
	// 			headerName: "delete",
	// 			editable: false,
	// 			sortable: false,
	// 			renderCell: (params) => {
	// 				return (
	// 					<IconButton
	// 						size="small"
	// 						onClick={(event) => {
	// 							event.stopPropagation();
	// 							const expression = this.state.expressions.find(
	// 								(e) => e.id == params.id
	// 							);
	// 							this.removeExpression(expression);
	// 						}}
	// 					></IconButton>
	// 				);
	// 			},
	// 		},
	// 	];
	// }

	expressionOptions() {
		return [
			{ label: "Inspect", action: this.inspectExpression },
			{ label: "Remove", action: this.removeExpression },
		];
	}

	expressionActions() {
		return [
			{
				label: "Remove",
				icon: <DeleteIcon fontSize="small" />,
				handler: this.removeExpression,
			},
		];
	}

	evaluationContext() {
		const frame = this.props.frame;
		return frame
			? {
					debugger: this.props.id,
					frame: frame.index,
			  }
			: {};
	}

	addExpression = async () => {
		try {
			const source = await this.props.dialog.prompt({
				title: "Expression",
			});
			if (source) {
				const expression = {
					id: this.state.expressions.length,
					sourceCode: source,
				};
				try {
					const result = await ide.api.evaluateExpression(
						source,
						true,
						false,
						this.evaluationContext()
					);
					expression.value = result.printString;
				} catch (error) {
					expression.value = error.data
						? error.data.description
						: "Error evaluating expression";
					expression.error = true;
				}
				this.setState({
					expressions: [...this.state.expressions, expression],
				});
			}
		} catch (error) {}
	};

	removeExpression = (expression) => {
		const expressions = this.state.expressions;
		const index = expressions.indexOf(expression);
		if (index >= 0) {
			expressions.splice(index, 1);
			this.setState({ expressions: expressions });
		}
	};

	render() {
		const { styles } = this.props;
		const { expressions, selectedExpression } = this.state;
		return (
			<Box
				display="flex"
				flexDirection="column"
				justifyContent="center"
				style={{ height: "100%" }}
			>
				<Box pb={1} height={"80%"}>
					<Paper variant="outlined" style={{ height: "100%" }}>
						<Box
							display="flex"
							flexDirection="column"
							height={"100%"}
						>
							<Box flexGrow={1}>
								<CustomTable
									//noHeaders
									styles={styles}
									columns={this.expressionColumns()}
									rows={expressions}
									onSelect={this.expressionSelected}
									menuOptions={this.expressionOptions()}
									rowActions={this.expressionActions()}
								/>
								{/* <DataGrid
							rows={expressions}
							columns={this.expressionColumns()}
							disableSelectionOnClick
							density="compact"
							disableColumnMenu
						/> */}
							</Box>
							<Box>
								<IconButton
									id="addExpression"
									color="primary"
									size="small"
									onClick={this.addExpression}
								>
									<AddIcon />
								</IconButton>
							</Box>
						</Box>
					</Paper>
				</Box>
				<Box height={"20%"}>
					<Paper variant="outlined" style={{ height: "100%" }}>
						<CodeEditor
							styles={styles}
							lineNumbers={false}
							source={
								selectedExpression
									? selectedExpression.value
									: ""
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

export default withDialog()(ExpressionTable);
