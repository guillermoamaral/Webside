import React, { Component } from "react";
import CustomTable from "../controls/CustomTable";
import ToolContainerContext from "../ToolContainerContext";
import ApplyIcon from "@mui/icons-material/CheckCircle";
import { Box } from "@mui/material";

class MethodVersionTable extends Component {
	static contextType = ToolContainerContext;

	changeSelected = (change) => {
		if (this.props.onChangeSelect) {
			this.props.onChangeSelect(change);
		}
	};

	browseClass = (change) => {
		if (change) {
			this.context.browseClass(change.className);
		}
	};

	browseImplementors = (change) => {
		if (change) {
			this.context.browseImplementors(change.selector);
		}
	};

	browseSenders = (change) => {
		if (change) {
			this.context.browseSenders(change.selector);
		}
	};

	applyChange = (change) => {
		if (this.props.onChangeApply) {
			this.props.onChangeApply(change);
		}
	};

	menuOptions() {
		return [
			{ label: "Install this version", action: this.applyChange },
			{ label: "Browse class", action: this.browseClass },
			{ label: "Browse implementors", action: this.browseImplementors },
			{ label: "Browse senders", action: this.browseSenders },
		];
	}

	rowActions() {
		return [
			{
				label: "Install",
				icon: <ApplyIcon fontSize="small" />,
				handler: this.applyChange,
			},
		];
	}

	colums() {
		return [
			{
				field: "timestamp",
				label: "Timestamp",
				minWidth: 250,
				align: "left",
				formatter: (ts) => {
					return ts ? new Date(ts).toLocaleString() : "";
				},
			},
			{ field: "author", label: "Author", minWidth: 250, align: "left" },
		];
	}

	render() {
		console.log("rendering history table");
		const { changes, selectedChange } = this.props;
		return (
			<Box
				display="flex"
				flexDirection="column"
				style={{ height: "100%" }}
			>
				<CustomTable
					style={{ height: "100%" }}
					columns={this.colums()}
					rows={changes}
					selectedRow={selectedChange}
					onRowSelect={this.changeSelected}
					menuOptions={this.menuOptions()}
					rowActions={this.rowActions()}
				/>
			</Box>
		);
	}
}

export default MethodVersionTable;
