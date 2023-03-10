import React, { Component } from "react";
import CustomTable from "../controls/CustomTable";
import { ide } from "../IDE";

class ChangesTable extends Component {
	changeSelected = (change) => {
		if (this.props.onSelect) {
			this.props.onSelect(change);
		}
	};

	browseClass = (change) => {
		if (change) {
			ide.browseClass(change.className);
		}
	};

	browseImplementors = (change) => {
		if (change && change.isMethodChange()) {
			ide.browseImplementors(change.selector);
		}
	};

	applyChange = async (change) => {
		if (change) {
			try {
				await ide.api.postChange(change.asJson());
				await change.updateCurrentSourceCode();
				change.color = this.colorFor(change);
			} catch (error) {
				this.reportError(error);
			}
		}
	};

	menuOptions() {
		return [
			{ label: "Browse class", action: this.browseClass },
			{ label: "Browse implementors", action: this.browseImplementors },
			{ label: "Apply", action: this.applyChange },
		];
	}

	changeColums() {
		return [
			{
				field: (ch) => {
					return ch.type();
				},
				label: "Type",
				minWidth: 150,
				align: "left",
			},
			{ field: "label", label: "Target", minWidth: 250, align: "left" },
			{
				field: "package",
				label: "Package",
				minWidth: 150,
				align: "left",
			},
			{
				field: "author",
				label: "Author",
				minWidth: 150,
				align: "center",
			},
			{
				field: "timestamp",
				label: "Timestamp",
				minWidth: 200,
				align: "left",
				formatter: (ts) => {
					return ts ? ts.toLocaleString("en-US") : "";
				},
			},
		];
	}

	colorFor(change) {
		return change.isUpToDate() ? "green" : "default";
	}

	render() {
		const styles = this.props.styles;
		const rows = this.props.changes;
		rows.forEach((ch) => (ch.color = this.colorFor(ch)));
		return (
			<CustomTable
				style={{ height: "100%" }}
				styles={styles}
				columns={this.changeColums()}
				rows={rows}
				onSelect={this.changeSelected}
				menuOptions={this.menuOptions()}
			/>
		);
	}
}

export default ChangesTable;
