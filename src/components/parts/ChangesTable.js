import React, { Component } from "react";
import CustomTable from "../controls/CustomTable";
import { ide } from "../IDE";
import ToolContainerContext from "../ToolContainerContext";
import ApplyIcon from "@mui/icons-material/CheckCircle";
import RejectIcon from "@mui/icons-material/Delete";
import { Box, Stack, Chip } from "@mui/material";

class ChangesTable extends Component {
	static contextType = ToolContainerContext;

	constructor(props) {
		super(props);
		this.state = {
			filters: [],
		};
	}

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
		if (change && change.isMethodChange()) {
			this.context.browseImplementors(change.selector);
		}
	};

	browseSenders = (change) => {
		if (change && change.isMethodChange()) {
			this.context.browseSenders(change.selector);
		}
	};

	applyChange = (change) => {
		if (this.props.onChangeApply) {
			this.props.onChangeApply(change);
		}
	};

	addFilter(label, fx) {
		const filters = this.state.filters;
		const filter = { label: label, function: fx };
		filters.push(filter);
		this.filtersChanged(filters);
		this.setState({ filters: filters });
	}

	removeFilter(filter) {
		const filters = this.state.filters.filter((f) => {
			return f !== filter;
		});
		this.filtersChanged(filters);
		this.setState({
			filters: filters,
		});
	}

	filtersChanged(filters) {
		if (this.props.onFiltersChange) {
			this.props.onFiltersChange(filters);
		}
	}

	rejectChange = (change) => {
		if (this.props.onChangeReject) {
			this.props.onChangeReject(change);
		}
	};

	selectChangesWithTheSame(change, property) {
		const target = property === "type" ? change.type() : change[property];
		this.addFilter(property + " = " + target, (ch) => {
			const source = property === "type" ? ch.type() : ch[property];
			return source === target;
		});
	}

	selectChangesWith = async (change, property) => {
		const target = await ide.prompt({
			title: "Enter " + property,
		});
		this.addFilter(property + " = " + target, (ch) => {
			const source = property === "type" ? ch.type() : ch[property];
			return source === target;
		});
	};

	rejectChangesWithTheSame(change, property) {
		const target = property === "type" ? change.type() : change[property];
		this.addFilter(property + " != " + target, (ch) => {
			const source = property === "type" ? ch.type() : ch[property];
			return source !== target;
		});
	}

	rejectChangesWith = async (property) => {
		const target = await ide.prompt({
			title: "Enter " + property,
		});
		this.addFilter(property + " != " + target, (ch) => {
			const source = property === "type" ? ch.type() : ch[property];
			return source !== target;
		});
	};

	menuOptions() {
		const properties = [
			"type",
			"className",
			"selector",
			"package",
			"author",
			"timestamp",
		];
		const selectWithTheSameOptions = properties.map((p) => {
			return {
				label: p,
				action: (ch) => {
					this.selectChangesWithTheSame(ch, p);
				},
			};
		});
		const selectWithOptions = properties.map((p) => {
			return {
				label: p,
				action: (ch) => {
					this.selectChangesWith(ch, p);
				},
			};
		});
		const rejectWithTheSameOptions = properties.map((p) => {
			return {
				label: p,
				action: (ch) => {
					this.rejectChangesWithTheSame(p);
				},
			};
		});
		const rejectWithOptions = properties.map((p) => {
			return {
				label: p,
				action: (ch) => {
					this.rejectChangesWith(ch, p);
				},
			};
		});
		return [
			{ label: "Browse class", action: this.browseClass },
			{ label: "Browse implementors", action: this.browseImplementors },
			{ label: "Browse senders", action: this.browseSenders },
			{ label: "Apply", action: this.applyChange },
			{ label: "Reject", action: this.rejectChange },
			null,
			{
				label: "Select...",
				suboptions: [
					{
						label: "With the same...",
						suboptions: selectWithTheSameOptions,
					},
					{
						label: "With...",
						suboptions: selectWithOptions,
					},
				],
			},
			{
				label: "Reject...",
				suboptions: [
					{
						label: "With the same...",
						suboptions: rejectWithTheSameOptions,
					},
					{
						label: "With...",
						suboptions: rejectWithOptions,
					},
				],
			},
		];
	}

	rowActions() {
		return [
			{
				label: "Apply",
				icon: <ApplyIcon fontSize="small" />,
				handler: this.applyChange,
				//visible: this.canApplyChange,
			},
			{
				label: "Reject",
				icon: <RejectIcon fontSize="small" />,
				handler: this.rejectChange,
			},
		];
	}

	colums() {
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
					return ts ? new Date(ts).toLocaleString() : "";
				},
			},
		];
	}

	changeColor(change) {
		if (change.color) return change.color;
		const appearance = ide.settings.section("appearance");
		const mode = appearance.section(appearance.get("mode"));
		const colors = mode.section("colors");
		return change.isUpToDate()
			? colors.get("appliedChange")
			: colors.get("unappliedChange");
	}

	render() {
		console.log("rendering changes table");
		const filters = this.state.filters;
		const { changes, selectedChange } = this.props;
		return (
			<Box
				display="flex"
				flexDirection="column"
				style={{ height: "100%" }}
			>
				<Box flexGrow={1} style={{ height: "100%" }}>
					<CustomTable
						style={{ height: "100%" }}
						columns={this.colums()}
						rows={changes}
						rowColor={this.changeColor}
						selectedRow={selectedChange}
						onRowSelect={this.changeSelected}
						menuOptions={this.menuOptions()}
						rowActions={this.rowActions()}
						useFilter
					/>
				</Box>
				<Box ml={1} mt={1} mb={1}>
					<Stack direction="row" spacing={1}>
						{filters.map((f, i) => (
							<Chip
								label={f.label}
								onDelete={(e) => {
									this.removeFilter(f);
								}}
								key={"chip" + i}
							/>
						))}
					</Stack>
				</Box>
			</Box>
		);
	}
}

export default ChangesTable;
