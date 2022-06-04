import React, { Component } from "react";
import { Box } from "@material-ui/core";
import { Pagination } from "@material-ui/lab";
import CustomList from "./CustomList";

class PaginatedList extends Component {
	constructor(props) {
		super(props);
		this.state = {
			currentPage: 1,
		};
	}

	currentItems() {
		const begin = (this.state.currentPage - 1) * this.props.itemsPerPage;
		const end = begin + this.props.itemsPerPage;
		return this.props.items.slice(begin, end);
	}

	render() {
		const { itemsPerPage, ...other } = this.props;
		const pages = Math.ceil(this.props.items.length / itemsPerPage);
		return (
			<Box p="5">
				<CustomList {...other} items={this.currentItems()} />
				<Pagination
					count={pages}
					size="medium"
					page={this.state.currentPage}
					variant="text"
					onChange={(event, page) => this.setState({ currentPage: page })}
				/>
			</Box>
		);
	}
}

export default PaginatedList;
