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
	TablePagination,
	TableSortLabel,
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
			usePagination: props.usePagination,
			currentPage: 0,
			rowsPerPage: props.rowsPerPage || 10,
			order: {
				column: null,
				direction: "asc",
			},
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
		const field = column.field;
		const value = typeof field == "string" ? row[field] : field(row);
		return column.formatter ? column.formatter(value) : value;
	};

	getCellColor = (row, column) => {
		const color = column.color;
		if (typeof color == "function") {
			return color(row);
		}
		if (typeof color == "string") {
			return color;
		}
		return row.color ? row.color : "default";
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
		const rows = this.pageRows();
		const index = rows.indexOf(this.state.selectedRow);
		if (index > 0) {
			this.rowSelected(rows[index - 1]);
		}
	};

	moveDown = () => {
		const rows = this.pageRows();
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

	pageRows() {
		const { currentPage, rowsPerPage } = this.state;
		const begin = currentPage * rowsPerPage;
		const end = begin + rowsPerPage;
		return (this.props.rows || []).slice(begin, end);
	}

	rowsPerPageChanged(amount) {
		this.setState({
			currentPage: 0,
			rowsPerPage: amount,
		});
	}

	pageChanged(page) {
		this.setState({ currentPage: page });
	}

	sortComparator(a, b, column, direction) {
		const av = this.getCellValue(a, column);
		const bv = this.getCellValue(b, column);
		if (av < bv) {
			return direction === "asc" ? 1 : -1;
		}
		if (av > bv) {
			return direction === "asc" ? -1 : 1;
		}
		return 0;
	}

	sortByColumn(column) {
		const rows = this.props.rows;
		const order = this.state.order;
		const direction =
			order.column === column && order.direction === "asc"
				? "desc"
				: "asc";
		rows.forEach((r, i) => (r._index = i));
		rows.sort((a, b) => {
			const o = this.sortComparator(a, b, column, direction);
			return o !== 0 ? o : a._index - b._index;
		});
		this.setState({
			order: { column: column, direction: direction },
		});
	}

	render() {
		const {
			selectedRow,
			menuOpen,
			menuPosition,
			usePagination,
			currentPage,
			rowsPerPage,
			order,
		} = this.state;
		const columns = this.columns();
		const rows = this.props.rows;
		const border = this.props.hideRowBorder ? "none" : "";
		//To force a diff
		return (
			<Box
				p={1}
				display="flex"
				flexDirection="column"
				style={{ height: "100%" }}
			>
				<Box flexGrow={1}>
					<TableContainer style={{ height: "100%" }}>
						<Scrollable>
							<Table
								stickyHeader
								size="small"
								onKeyDown={this.keyDown}
							>
								{!this.props.noHeaders && (
									<TableHead>
										<TableRow key="header">
											{columns.map((column) => (
												<TableCell
													key={column.field}
													align={column.align}
													style={{
														minWidth:
															column.minWidth,
													}}
												>
													<TableSortLabel
														active={
															order.column &&
															order.column
																.field ===
																column.field
														}
														direction={
															order.direction
														}
														onClick={(event) => {
															this.sortByColumn(
																column
															);
														}}
													>
														{column.label}
													</TableSortLabel>
												</TableCell>
											))}
										</TableRow>
									</TableHead>
								)}
								<TableBody>
									{this.pageRows().map((row, index) => {
										return (
											<TableRow
												hover
												className={
													this.props.classes.row
												}
												tabIndex={-1}
												key={index}
												selected={row === selectedRow}
												onClick={(event) =>
													this.rowSelected(row)
												}
												onContextMenu={this.openMenu}
											>
												{columns.map((column) => {
													const color =
														this.getCellColor(
															row,
															column
														);
													return (
														<TableCell
															key={
																column.field +
																index
															}
															align={column.align}
															style={{
																color: color,
																borderBottom:
																	border,
															}}
														>
															{column.field ===
															"actions"
																? this.rowActionButtons(
																		row,
																		index
																  )
																: this.getCellValue(
																		row,
																		column
																  )}
														</TableCell>
													);
												})}
											</TableRow>
										);
									})}
								</TableBody>
							</Table>
						</Scrollable>
					</TableContainer>
					<PopupMenu
						options={this.props.menuOptions}
						open={menuOpen}
						position={menuPosition}
						onOptionClick={this.menuOptionClicked}
						onOptionEnable={this.getMenuOptionEnabled}
						onClose={this.closeMenu}
					/>
				</Box>
				{usePagination && (
					<Box>
						<TablePagination
							component="div"
							count={rows.length}
							size="small"
							page={currentPage}
							variant="text"
							onChangePage={(event, page) =>
								this.pageChanged(page)
							}
							rowsPerPage={rowsPerPage}
							onChangeRowsPerPage={(event) => {
								this.rowsPerPageChanged(
									parseInt(event.target.value, 10)
								);
							}}
						/>
					</Box>
				)}
			</Box>
		);
	}
}

export default withStyles(styles)(CustomTable);
