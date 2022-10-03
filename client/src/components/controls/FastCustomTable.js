import React, { Component } from "react";
import AutoSizer from "react-virtualized-auto-sizer";
import { FixedSizeList as List } from "react-window";
import {
	Table,
	TableHead,
	TableBody,
	TableRow,
	TableCell,
	Box,
} from "@material-ui/core";

const ROW_HEIGHT = 30;

class FastCustomTable extends Component {
	constructor(props) {
		super(props);
		this.state = {
			menuOpen: false,
			menuPosition: { x: null, y: null },
			selectedRow: null,
		};
	}

	columns() {
		const columns = [...this.props.columns];
		const extra = {
			field: "actions",
			label: "",
			align: "center",
		};
		columns.push(extra);
		return columns;
	}

	rowSelected = (row) => {
		this.setState({ selectedRow: row });
		if (this.props.onSelect) {
			this.props.onSelect(row);
		}
	};

	getCellValue = (row, column) => {
		if (!row) {
			return "";
		}
		const field = column.field;
		const value = typeof field == "string" ? row[field] : field(row);
		return column.formatter ? column.formatter(value) : value;
	};

	rowActionButtons(row, index) {
		const actions = this.props.rowActions || [];
		return (
			<Box display="flex" alignItems="center">
				{actions.map((action) => {
					const visible =
						action.visible === undefined ||
						(typeof action.visible == "boolean" &&
							action.visible) ||
						(typeof action.visible == "function" &&
							action.visible(row));
					return (
						<Box style={{ minWidth: 10 }}>
							{visible && (
								<IconButton
									className="button"
									key={action.label + index}
									color="inherit"
									size="small"
									onClick={(event) => {
										action.handler(row);
									}}
								>
									{action.icon}
								</IconButton>
							)}
						</Box>
					);
				})}
			</Box>
		);
	}

	renderRow = ({ index, style }) => {
		const row = this.props.rows[index];
		const selected = this.props.selectedRow === row;
		return (
			<TableRow
				component="div"
				hover
				style={{ style }}
				selected={selected}
				onClick={(event) => this.rowSelected(row)}
			>
				{this.columns().map((column) => {
					return (
						<TableCell
							component="div"
							key={column.field + index}
							align={column.align}
							onClick={(event) => this.rowSelected(row)}
							style={{ height: ROW_HEIGHT }}
						>
							{column.field === "actions"
								? this.rowActionButtons(row, index)
								: this.getCellValue(row, column)}
						</TableCell>
					);
				})}
			</TableRow>
		);
	};

	render() {
		const { menuOpen, menuPosition } = this.state;
		const columns = this.columns();
		const rows = this.props.rows || [];
		return (
			<Box style={{ height: "100%" }}>
				<Table
					component="div"
					style={{ height: "100%", width: "100%" }}
				>
					<TableHead component="div">
						<TableRow component="div">
							{columns.map((column) => {
								return (
									<TableCell
										component="div"
										key={column.field}
										align={column.align}
										style={{ minWidth: column.minWith }}
									>
										{column.label}
									</TableCell>
								);
							})}
						</TableRow>
					</TableHead>
					<TableBody component="div">
						<AutoSizer>
							{({ height, width }) => (
								<List
									height={height}
									width={width}
									itemCount={rows.length}
									itemSize={ROW_HEIGHT}
								>
									{this.renderRow}
								</List>
							)}
						</AutoSizer>
					</TableBody>
				</Table>
			</Box>
		);
	}
}

export default FastCustomTable;
