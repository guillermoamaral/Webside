import React, { Component } from "react";
import CustomTable from "../controls/CustomTable";
import { ide } from "../IDE";
import { container } from "../ToolsContainer";
import ApplyIcon from "@material-ui/icons/CheckCircle";

class ChangesTable extends Component {
	changeSelected = (change) => {
		if (this.props.onSelect) {
			this.props.onSelect(change);
		}
	};

	browseClass = (change) => {
		if (change) {
			container.browseClass(change.className);
		}
	};

	browseImplementors = (change) => {
		if (change && change.isMethodChange()) {
			container.browseImplementors(change.selector);
		}
	};

	applyChange = async (change) => {
		if (change) {
			try {
				await ide.api.postChange(change.asJson());
				const handler = this.props.changeApplied;
				await change.updateCurrentSourceCode();
				if (handler) {
					handler(change);
				} else {
					change.color = this.colorFor(change);
					this.setState({});
				}
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

	changeActions() {
		return [
			{
				label: "Apply",
				icon: <ApplyIcon fontSize="small" />,
				handler: this.applyChange,
				//visible: this.canApplyChange,
			},
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
				minWidth: 300,
				align: "left",
				formatter: (ts) => {
					return ts ? ts.toLocaleString("en-US") : "";
				},
			},
		];
	}

	colorFor(change) {
		if (change.color) {
			return change.color;
		}
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
				rowActions={this.changeActions()}
			/>
		);
	}
}

export default ChangesTable;
