import React, { PureComponent } from "react";
import { Box, Paper, IconButton } from "@mui/material";
import { ide } from "../IDE";
import { container } from "../ToolsContainer";
import { DataGrid } from "@mui/x-data-grid";
import CodeEditor from "../parts/CodeEditor";
import AddIcon from "@mui/icons-material/AddCircle";
import DeleteIcon from "@mui/icons-material/Delete";

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

	newExpressionId() {
		const expressions = this.state.expressions;
		if (expressions.length === 0) {
			return 0;
		}
		const sorted = expressions.map((e) => e.id).sort();
		const maxId = sorted[sorted.length - 1];
		return maxId + 1;
	}

	addExpression = async () => {
		try {
			const source = await ide.prompt({
				title: "Expression",
			});
			if (source) {
				const expression = {
					id: this.newExpressionId(),
					sourceCode: source,
				};
				await this.evaluateExpression(expression);
				this.setState({
					expressions: [...this.state.expressions, expression],
				});
			}
		} catch (error) {}
	};

	expressionWithId(id) {
		return this.state.expressions.find((e) => e.id === id);
	}

	updateExpression = async (expression) => {
		if (expression) {
			await this.evaluateExpression(expression);
			this.setState({
				expressions: this.state.expressions,
				selectedExpression: null,
			});
		}
	};

	evaluateExpression = async (expression) => {
		try {
			const result = await ide.api.evaluateExpression(
				expression.sourceCode,
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
	};

	removeExpression = (expression) => {
		const expressions = this.state.expressions;
		const index = expressions.indexOf(expression);
		if (index >= 0) {
			expressions.splice(index, 1);
			this.setState({ expressions: expressions });
		}
	};

	inspectExpression = async (expression) => {
		try {
			const object = await container.evaluateExpression(
				expression.sourceCode,
				false,
				true,
				this.evaluationContext()
			);
			container.openInspector(object);
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
				headerName: "Expression",
				editable: true,
			},
			{
				field: "value",
				headerName: "Value",
				editable: false,
			},
			{
				field: "delete",
				headerName: "",
				editable: false,
				sortable: false,
				renderCell: (params) => {
					return (
						<IconButton
							size="small"
							onClick={(event) => {
								event.stopPropagation();
								this.removeExpression(
									this.expressionWithId(params.id)
								);
							}}
						></IconButton>
					);
				},
			},
		];
	}

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
								<DataGrid
									rows={expressions}
									columns={this.expressionColumns()}
									disableSelectionOnClick
									density="compact"
									disableColumnMenu
									hideFooter
									processRowUpdate={(newRow, oldRow) => {
										const expression =
											this.expressionWithId(newRow.id);
										expression.sourceCode =
											newRow.sourceCode;
										this.updateExpression(expression);
										return newRow;
									}}
								/>
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

export default ExpressionTable;
