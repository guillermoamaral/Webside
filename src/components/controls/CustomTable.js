import { Component } from "react";
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
	Link,
	InputBase,
	Tooltip,
} from "@mui/material";
import { styled } from "@mui/material/styles";
import PopupMenu from "./PopupMenu";
import Scrollable from "./Scrollable";
import SearchIcon from "@mui/icons-material/Search";

const StyledTableRow = styled(TableRow)(({ theme }) => ({
	"& .actionButton": {
		display: "none",
	},
	"&:hover .actionButton": {
		display: "flex",
	},
}));

class CustomTable extends Component {
	constructor(props) {
		super(props);
		const rows = props.rows || [];
		this.state = {
			menuOpen: false,
			menuPosition: { x: null, y: null },
			selectedRow: null,
			usePagination: props.usePagination,
			currentPage: 0,
			rowsPerPage: props.rowsPerPage || 10,
			filterText: "",
			rows: rows,
			filteredRows: rows,
			order: {
				column: null,
				direction: "asc",
			},
			editingCell: null,
		};
	}

	static getDerivedStateFromProps(props, state) {
		if (state.rows !== props.rows) {
			return {
				menuOpen: false,
				rows: props.rows,
				selectedRow: props.selectedRow,
				filterText: "",
				filteredRows: props.rows,
			};
		}
		if (state.selectedRow !== props.selectedRow) {
			return {
				menuOpen: false,
				selectedRow: props.selectedRow,
			};
		}
		return null;
	}

	rowSelected = (row) => {
		this.setState({ selectedRow: row });
		if (this.props.onRowSelect) {
			this.props.onRowSelect(row);
		}
	};

	rowDoubleClicked = (row) => {
		if (this.props.onRowDoubleClick) {
			this.props.onRowDoubleClick(row);
		}
	};

	menuOptions = () => {
		const options = this.props.menuOptions;
		return typeof options === "function"
			? options(this.state.selectedRow)
			: options;
	};

	openMenu = (event) => {
		event.preventDefault();
		event.stopPropagation();
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
		if (option.action) option.action(selected);
	};

	getMenuOptionEnabled = (option) => {
		const selected = this.state.selectedRow;
		if (option.enabled) return option.enabled(selected);
		return true;
	};

	getCellText = (row, column) => {
		const field = column.field;
		var text = typeof field == "string" ? row[field] : field(row);
		text = column.formatter ? column.formatter(text) : text;
		return text || "";
	};

	setCellText = (row, column, value) => {
		console.log(row, column, value);
	};

	renderCell = (row, i, column, j) => {
		if (column.field === "actions") return this.renderActionButtons(row, i);
		const text = this.getCellText(row, column);
		if (column.link) {
			const color = this.getCellColor(row, column);
			return (
				<Link
					key={{ text }}
					href="#"
					onClick={(event) => {
						event.preventDefault();
						column.link(row);
					}}
					color="textPrimary"
					style={{ color: color }}
				>
					{text}
				</Link>
			);
		}
		if (
			column.editable &&
			this.state.editingCell &&
			this.state.editingCell[0] === i &&
			this.state.editingCell[1] === j
		) {
			return (
				<InputBase
					size="small"
					disabled={false}
					readOnly={false}
					//Review this fix size. It was fixed to avoid dynamic resizing
					//style={{ height: 22 }}
					placeholder={text}
					value={text}
					inputProps={{ "aria-label": "expression", size: "small" }}
					onChange={(event) => {
						this.setCellText(row, column, event.target.value);
					}}
					onBlur={(event) => {
						this.setState({ editingCell: null });
					}}
					// onKeyDown={(event) => {
					// 	console.log(event.key);
					// }}
				/>
			);
		}
		return text;
	};

	getCellColor = (row, column) => {
		const color = this.props.rowColor || column.color || row.color;
		if (typeof color == "function") return color(row);
		if (typeof color == "string") return color;
		return "default";
	};

	keyDown = (event) => {
		event.preventDefault();
		const key = event.key;
		if (key === "ArrowUp") this.moveUp();
		if (key === "ArrowDown") this.moveDown();
		return true;
	};

	moveUp = () => {
		const rows = this.pageRows();
		const index = rows.indexOf(this.state.selectedRow);
		if (index > 0) this.rowSelected(rows[index - 1]);
	};

	moveDown = () => {
		const rows = this.pageRows();
		const index = rows.indexOf(this.state.selectedRow);
		if (index < rows.length - 1) this.rowSelected(rows[index + 1]);
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

	getRowActions = (row) => {
		const actions = this.props.rowActions;
		if (typeof actions === "function") return actions(row);
		return actions || [];
	};

	renderActionButtons(row, index) {
		const actions = this.getRowActions(row);
		return (
			<Box display="flex" alignItems="center" key={"box" + index}>
				{actions.map((action, j) => {
					const visible =
						action.visible === undefined ||
						(typeof action.visible == "boolean" &&
							action.visible) ||
						(typeof action.visible == "function" &&
							action.visible(row));
					return (
						<Box
							//Review these fixed sizes. They were fixed to avoid dynamic
							//resizing when hovering
							style={{ width: 22, height: 22 }}
							key={"box" + index + "action" + j}
						>
							{visible && (
								<Tooltip title={action.label} placement="top">
									<IconButton
										className="actionButton"
										style={{ width: 22, height: 22 }}
										key={"button" + index + "action" + j}
										color="inherit"
										size="small"
										onClick={(event) => {
											event.stopPropagation();
											action.handler(row);
										}}
									>
										{action.icon}
									</IconButton>
								</Tooltip>
							)}
						</Box>
					);
				})}
			</Box>
		);
	}

	filterRows(text) {
		var filtered = this.state.rows;
		if (text !== "") {
			const target = text.toLowerCase();
			const columns = this.props.columns;
			filtered = filtered.filter((row) => {
				return (
					columns.find((column) => {
						const text = this.getCellText(row, column) || "";
						return text.toString().toLowerCase().includes(target);
					}) !== undefined
				);
			});
		}
		const { currentPage, rowsPerPage } = this.state;
		const page =
			currentPage * rowsPerPage > filtered.length
				? Math.floor(filtered.length / rowsPerPage)
				: currentPage;
		this.setState({
			filteredRows: filtered,
			filterText: text,
			currentPage: page,
		});
	}

	pageRows() {
		const { usePagination, currentPage, rowsPerPage, filteredRows } =
			this.state;
		if (!usePagination) return filteredRows;
		const begin = currentPage * rowsPerPage;
		const end = begin + rowsPerPage;
		return filteredRows.slice(begin, end);
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
		const av = this.getCellText(a, column);
		const bv = this.getCellText(b, column);
		if (av < bv) return direction === "asc" ? 1 : -1;
		if (av > bv) return direction === "asc" ? -1 : 1;
		return 0;
	}

	sortByColumn(column) {
		const { filteredRows, order } = this.state;
		const direction =
			order.column === column && order.direction === "asc"
				? "desc"
				: "asc";
		filteredRows.forEach((r, i) => (r._index = i));
		filteredRows.sort((a, b) => {
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
			filteredRows,
			filterText,
		} = this.state;
		const useFilter = this.props.useFilter;
		const columns = this.columns();
		const rows = this.pageRows() || [];
		const border = this.props.hideRowBorder ? "none" : "";
		const menuOptions = this.menuOptions();
		return (
			<Box
				p={0}
				m={0}
				display="flex"
				flexDirection="column"
				style={{ height: "100%" }}
			>
				<Box display="flex" flexGrow={1}>
					<TableContainer style={{ height: "100%" }}>
						<Scrollable>
							<Table
								stickyHeader
								size="small"
								onKeyDown={this.keyDown}
							>
								{!this.props.noHeaders && (
									<TableHead>
										<TableRow key="headers">
											{columns.map((column, index) => (
												<TableCell
													key={"header" + index}
													align={column.align}
													style={{
														minWidth:
															column.minWidth,
													}}
												>
													<TableSortLabel
														key={"label" + index}
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
									{rows.map((row, i) => {
										return (
											<StyledTableRow
												hover
												sx={{ cursor: "pointer" }}
												tabIndex={-1}
												key={"row" + i}
												selected={row === selectedRow}
												onClick={(event) =>
													this.rowSelected(row)
												}
												onContextMenu={this.openMenu}
												onDoubleClick={(event) => {
													event.preventDefault();
													this.rowDoubleClicked(row);
												}}
											>
												{columns.map((column, j) => {
													const color =
														this.getCellColor(
															row,
															column
														);
													return (
														<TableCell
															key={
																"cell" +
																i +
																"-" +
																j
															}
															align={column.align}
															style={{
																color: color,
																borderBottom:
																	border,
															}}
															onDoubleClick={(
																event
															) => {
																if (
																	column.editable
																) {
																	this.setState(
																		{
																			editingCell:
																				[
																					i,
																					j,
																				],
																		}
																	);
																}
															}}
														>
															{this.renderCell(
																row,
																i,
																column,
																j
															)}
														</TableCell>
													);
												})}
											</StyledTableRow>
										);
									})}
								</TableBody>
							</Table>
						</Scrollable>
					</TableContainer>
					{menuOptions && menuOptions.length > 0 && (
						<PopupMenu
							options={menuOptions}
							open={menuOpen}
							position={menuPosition}
							onOptionClick={this.menuOptionClicked}
							onOptionEnable={this.getMenuOptionEnabled}
							onClose={this.closeMenu}
						/>
					)}
				</Box>
				{(usePagination || useFilter) && (
					<Box display="flex" flexDirection="row">
						{(usePagination || useFilter) && (
							<Box
								display="flex"
								flexDirection="row"
								alignItems="center"
							>
								<Box ml={2} mr={1}>
									<SearchIcon size="small" />
								</Box>
								<Box>
									<InputBase
										value={filterText}
										placeholder="Search…"
										inputProps={{ "aria-label": "search" }}
										onKeyDown={(event) => {
											if (event.key === "Escape") {
												this.filterRows("");
											}
										}}
										onChange={(event) =>
											this.filterRows(event.target.value)
										}
									/>
								</Box>
							</Box>
						)}
						{usePagination && (
							<Box flexGrow={1}>
								<TablePagination
									component="div"
									count={filteredRows.length}
									size="small"
									page={currentPage}
									variant="text"
									onPageChange={(event, page) =>
										this.pageChanged(page)
									}
									rowsPerPage={rowsPerPage}
									onRowsPerPageChange={(event) => {
										this.rowsPerPageChanged(
											parseInt(event.target.value, 10)
										);
									}}
								/>
							</Box>
						)}
					</Box>
				)}
			</Box>
		);
	}
}

export default CustomTable;
