import React, { Component } from "react";
import CustomTable from "../controls/CustomTable";
import { ide } from "../IDE";
import { container } from "../ToolsContainer";
import ApplyIcon from "@mui/icons-material/CheckCircle";
import { Box, Stack, Chip } from "@mui/material";

class ChangesTable extends Component {
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
				await change.updateCurrentSourceCode();
				change.color = null;
				change.color = this.colorFor(change);
				const handler = this.props.changeApplied;
				if (handler) {
					handler(change);
				} else {
					this.setState({});
				}
			} catch (error) {
				this.reportError(error);
			}
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

	rejectChangesWith = async (change, property) => {
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
					this.rejectChangesWithTheSame(ch, p);
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
			{ label: "Apply", action: this.applyChange },
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
					return ts ? ts.toLocaleString("en-US") : "";
				},
			},
		];
	}

	colorFor(change) {
		if (change.color) {
			return change.color;
		}
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
		changes.forEach((ch) => (ch.color = this.colorFor(ch)));
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
