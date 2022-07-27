import React, { Component } from "react";
import { Box } from "@material-ui/core";
import { Pagination } from "@material-ui/lab";
import CustomTable from "./CustomTable";

class PaginatedTable extends Component {
	constructor(props) {
		super(props);
		this.state = {
			currentPage: 1,
		};
	}

	currentRows() {
		const begin = (this.state.currentPage - 1) * this.props.rowsPerPage;
		const end = begin + this.props.rowsPerPage;
		return this.props.rows.slice(begin, end);
	}

	render() {
		const { rowsPerPage, ...other } = this.props;
		const pages = Math.ceil(this.props.rows.length / rowsPerPage);
		return (
			<Box
				p={1}
				display="flex"
				flexDirection="column"
				style={{ height: "100%" }}
			>
				<Box flexGrow={1}>
					<CustomTable {...other} rows={this.currentRows()} />
				</Box>
				<Box>
					<Pagination
						count={pages}
						size="medium"
						page={this.state.currentPage}
						variant="text"
						onChange={(event, page) => this.setState({ currentPage: page })}
					/>
				</Box>
			</Box>
		);
	}
}

export default PaginatedTable;
