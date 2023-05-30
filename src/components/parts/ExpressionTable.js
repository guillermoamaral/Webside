import React, { PureComponent } from "react";
import { Box, Paper, IconButton } from "@mui/material";
import { ide } from "../IDE";
import { container } from "../ToolsContainer";
import { DataGrid } from "@mui/x-data-grid";
import CodeEditor from "../parts/CodeEditor";
import {
	AddCircle as AddIcon,
	Delete as DeleteIcon,
	Refresh as RefreshIcon,
} from "@mui/icons-material";

class ExpressionTable extends PureComponent {
	constructor(props) {
		super(props);
		this.state = {
			expressions: [],
			selectedExpression: null,
		};
	}

	expressionSelected = (expression) => {
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
			this.forceUpdate();
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
		this.setState({
			expressions: this.state.expressions.filter(
				(e) => e.id !== expression.id
			),
		});
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
				minWidth: 120,
			},
			{
				field: "value",
				headerName: "Value",
				editable: false,
				minWidth: 120,
			},
			{
				field: "delete",
				headerName: "",
				editable: false,
				sortable: false,
				renderCell: (params) => {
					return (
						<Box
							display="flex"
							alignItems="center"
							key={"box" + params.id}
						>
							<Box key={"update" + params.id}>
								<IconButton
									color="inherit"
									size="small"
									onClick={(event) => {
										event.stopPropagation();
										this.updateExpression(
											this.expressionWithId(params.id)
										);
									}}
								>
									<RefreshIcon />
								</IconButton>
							</Box>
							<Box key={"delete" + params.id}>
								<IconButton
									color="inherit"
									size="small"
									onClick={(event) => {
										event.stopPropagation();
										this.removeExpression(
											this.expressionWithId(params.id)
										);
									}}
								>
									<DeleteIcon />
								</IconButton>
							</Box>
						</Box>
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

	processRowUpdate = async (newRow) => {
		const expression = this.expressionWithId(newRow.id);
		expression.sourceCode = newRow.sourceCode;
		await this.updateExpression(expression);
		return expression;
	};

	render() {
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
									onRowClick={(params) => {
										this.expressionSelected(
											this.expressionWithId(params.id)
										);
									}}
									processRowUpdate={this.processRowUpdate}
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
