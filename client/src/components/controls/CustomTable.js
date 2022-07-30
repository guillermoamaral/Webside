import React, { Component } from "react";
import {
	TableContainer,
	Table,
	TableHead,
	TableBody,
	TableRow,
	TableCell,
	Box,
	IconButton,
} from "@material-ui/core";
import { withStyles } from "@material-ui/core/styles";
import PopupMenu from "./PopupMenu";
import Scrollable from "./Scrollable";

const styles = () => ({
	row: {
		"& .button": {
			display: "none",
		},
		"&:hover .button": {
			display: "block",
		},
	},
});

class CustomTable extends Component {
	constructor(props) {
		super(props);
		this.state = {
			menuOpen: false,
			menuPosition: { x: null, y: null },
			selectedRow: null,
		};
	}

	rowSelected = (row) => {
		this.setState({ selectedRow: row });
		if (this.props.onSelect) {
			this.props.onSelect(row);
		}
	};

	openMenu = (event) => {
		event.preventDefault();
		this.setState({
			menuOpen: true,
			menuPosition: { x: event.clientX - 2, y: event.clientY - 4 },
		});
	};

	closeMenu = () => {
		this.setState({ menuOpen: false });
	};

	menuOptionClicked = (option) => {
		const selected = this.state.selectedRow;
		if (option.action) {
			option.action(selected);
		}
	};

	getMenuOptionEnabled = (option) => {
		const selected = this.state.selectedRow;
		if (option.enabled) {
			return option.enabled(selected);
		}
		return true;
	};

	getCellValue = (row, column) => {
		const getter = column.field;
		const value = typeof getter == "string" ? row[getter] : getter(row);
		return column.formatter ? column.formatter(value) : value;
	};

	keyDown = (event) => {
		event.preventDefault();
		const key = event.key;
		if (key === "ArrowUp") {
			this.moveUp();
		}
		if (key === "ArrowDown") {
			this.moveDown();
		}
		return true;
	};

	moveUp = () => {
		const rows = this.props.rows;
		const index = rows.indexOf(this.state.selectedRow);
		if (index > 0) {
			this.rowSelected(rows[index - 1]);
		}
	};

	moveDown = () => {
		const rows = this.props.rows;
		const index = rows.indexOf(this.state.selectedRow);
		if (index < rows.length - 1) {
			this.rowSelected(rows[index + 1]);
		}
	};

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

	rowActionButtons(row, index) {
		const actions = this.props.rowActions || [];
		return (
			<Box display="flex" alignItems="center">
				{actions.map((action) => {
					const visible =
						(typeof action.visible == "boolean" && action.visible) ||
						(typeof action.visible == "function" && action.visible(row));
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

	render() {
		const { selectedRow, menuOpen, menuPosition } = this.state;
		const columns = this.columns();
		const rows = this.props.rows || [];
		return (
			<Scrollable>
				<TableContainer className={this.props.styles.container}>
					<Table stickyHeader size="small" onKeyDown={this.keyDown}>
						{!this.props.noHeaders && (
							<TableHead>
								<TableRow>
									{columns.map((column) => (
										<TableCell
											key={column.field}
											align={column.align}
											style={{ minWidth: column.minWith }}
										>
											{column.label}
										</TableCell>
									))}
								</TableRow>
							</TableHead>
						)}
						<TableBody>
							{rows.map((row, index) => {
								return (
									<TableRow
										hover
										className={this.props.classes.row}
										tabIndex={-1}
										key={index}
										selected={row === selectedRow}
										onClick={(event) => this.rowSelected(row)}
										onContextMenu={this.openMenu}
									>
										{columns.map((column) => {
											return (
												<TableCell
													key={column.field}
													align={column.align}
													style={{ color: row.color || "default" }}
												>
													{column.field === "actions"
														? this.rowActionButtons(row, index)
														: this.getCellValue(row, column)}
												</TableCell>
											);
										})}
									</TableRow>
								);
							})}
						</TableBody>
					</Table>
				</TableContainer>
				<PopupMenu
					options={this.props.menuOptions}
					open={menuOpen}
					position={menuPosition}
					onOptionClick={this.menuOptionClicked}
					onOptionEnable={this.getMenuOptionEnabled}
					onClose={this.closeMenu}
				/>
			</Scrollable>
		);
	}
}

export default withStyles(styles)(CustomTable);
