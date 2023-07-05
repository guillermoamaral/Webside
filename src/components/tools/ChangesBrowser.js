import React, { Component } from "react";
import {
	Grid,
	Paper,
	Box,
	IconButton,
	Tooltip,
	Tab,
	ToggleButton,
} from "@mui/material";
import CodeMerge from "../parts/CodeMerge";
import { ide } from "../IDE";
import ChangesTable from "../parts/ChangesTable";
import DownloadIcon from "@mui/icons-material/GetApp";
import RejectOlderIcon from "@mui/icons-material/Compress";
import RejectUpToDateIcon from "@mui/icons-material/RemoveDone";
import ApplyIcon from "@mui/icons-material/Done";
import ApplyAllIcon from "@mui/icons-material/DoneAll";
import ShowOriginalIcon from "@mui/icons-material/Refresh";
import ToolContainerContext from "../ToolContainerContext";
import HighlightIcon from "@mui/icons-material/Highlight";

class ChangesBrowser extends Component {
	static contextType = ToolContainerContext;

	constructor(props) {
		super(props);
		this.state = {
			changes: props.changeset.changes,
			selectedChange: props.selectedChange,
			highlightChanges: true,
		};
	}

	changeSelected = async (change) => {
		await change.updateCurrentSourceCode();
		this.setState({ selectedChange: change });
	};

	evaluationContext() {
		const change = this.state.selectedChange;
		return change && change.className ? { class: change.className } : {};
	}

	updateChanges = (changes, selecting) => {
		var selected = selecting || this.state.selectedChange;
		selected = changes.includes(selected) ? selected : null;
		this.setState({ changes: changes, selectedChange: selected }, () =>
			this.updateLabel()
		);
	};

	updateLabel() {
		const label =
			(this.props.title || "Changes") +
			" (" +
			this.props.changeset.size() +
			")";
		this.context.updatePageLabel(this.props.id, label);
	}

	download = async (event) => {
		event.preventDefault();
		try {
			const ch = await ide.api.downloadChangeset(
				this.props.changeset.changes.map((ch) => {
					return ch.asJson();
				})
			);
			const blob = new Blob([ch]);
			const url = window.URL.createObjectURL(blob);
			const link = document.createElement("a");
			link.href = url;
			link.setAttribute("download", "changes.ch");
			document.body.appendChild(link);
			link.click();
			document.body.removeChild(link);
		} catch (error) {
			ide.reportError();
		}
	};

	nextChange(change) {
		const changes = this.props.changeset.changes;
		const index = changes.indexOf(change);
		return index >= 0
			? index < changes.length - 1
				? changes[index + 1]
				: null
			: null;
	}

	rejectChange = (change) => {
		const changeset = this.props.changeset;
		const next = this.nextChange(change);
		changeset.rejectChange(change);
		this.updateChanges(changeset.changes, next);
	};

	rejectOlderChanges = () => {
		const changeset = this.props.changeset;
		changeset.compress();
		this.updateChanges(changeset.changes);
	};

	rejectChangesUpToDate = async () => {
		const changeset = this.props.changeset;
		ide.waitFor(async () => await changeset.rejectUpToDate());
		console.log(changeset.changes);
		this.updateChanges(changeset.changes);
	};

	applySelectedChange = () => {
		const selectedChange = this.state.selectedChange;
		if (selectedChange) {
			this.applyChange(selectedChange);
		}
	};

	applyChange = async (change) => {
		await this.applyChanges([change]);
		this.updateChanges(
			this.props.changeset.changes,
			this.nextChange(change)
		);
	};

	applyAllChanges = async () => {
		const changes = this.props.changeset.changes;
		if (changes.length === 0) {
			return;
		}
		await this.applyChanges(changes);
		this.updateChanges(
			this.props.changeset.changes,
			changes[changes.length - 1]
		);
	};

	applyChanges = async (changes) => {
		ide.waitFor(
			async () =>
				await Promise.all(
					changes.map(async (ch) => {
						try {
							await ide.api.postChange(ch.asJson());
							await ch.updateCurrentSourceCode();
						} catch (error) {
							ide.reportError(error);
						}
					})
				)
		);
	};

	filtersChanged = (filters) => {
		const changeset = this.props.changeset;
		changeset.filterChanges(filters);
		this.updateChanges(changeset.changes);
	};

	showOriginalChanges = async () => {
		const changeset = this.props.changeset;
		changeset.changes = changeset.originalChanges || changeset.changes;
		ide.waitFor(async () => await changeset.updateCurrentSourceCode());
		this.updateChanges(changeset.changes);
	};

	render() {
		const { changes, selectedChange, highlightChanges } = this.state;
		console.log("rendering changes browser");
		return (
			<Grid container spacing={1}>
				<Grid item xs={12} md={12} lg={12}>
					<Box display="flex" justifyContent="flex-end">
						<Tooltip title="Apply change" placement="top">
							<span>
								<IconButton
									color="inherit"
									onClick={this.applySelectedChange}
									disabled={!selectedChange}
								>
									<ApplyIcon fontSize="small" />
								</IconButton>
							</span>
						</Tooltip>
						<Tooltip title="Apply all changes" placement="top">
							<IconButton
								color="inherit"
								onClick={this.applyAllChanges}
							>
								<ApplyAllIcon fontSize="small" />
							</IconButton>
						</Tooltip>
						<Tooltip title="Reject older changes" placement="top">
							<IconButton
								color="inherit"
								onClick={this.rejectOlderChanges}
							>
								<RejectOlderIcon fontSize="small" />
							</IconButton>
						</Tooltip>
						<Tooltip
							title="Reject changes up to date"
							placement="top"
						>
							<IconButton
								color="inherit"
								onClick={this.rejectChangesUpToDate}
							>
								<RejectUpToDateIcon fontSize="small" />
							</IconButton>
						</Tooltip>
						<Tooltip title="Show original changes" placement="top">
							<IconButton
								color="inherit"
								onClick={this.showOriginalChanges}
							>
								<ShowOriginalIcon fontSize="small" />
							</IconButton>
						</Tooltip>
						<Tooltip title="Download" placement="top">
							<IconButton color="inherit" onClick={this.download}>
								<DownloadIcon fontSize="small" />
							</IconButton>
						</Tooltip>
					</Box>
				</Grid>
				<Grid item xs={12} md={12} lg={12}>
					<Paper variant="outlined" style={{ height: 350 }}>
						<ChangesTable
							changes={changes}
							selectedChange={selectedChange}
							onChangeSelect={this.changeSelected}
							onChangeApply={this.applyChange}
							onChangeReject={this.rejectChange}
							onFiltersChange={this.filtersChanged}
						/>
					</Paper>
				</Grid>
				<Grid item xs={6} md={6} lg={6}>
					<Box
						display="flex"
						alignContent="center"
						justifyContent="flex-start"
					>
						<Tab label="New source" />
					</Box>
				</Grid>
				<Grid item xs={3} md={3} lg={3}>
					<Box
						display="flex"
						alignContent="center"
						justifyContent="flex-start"
					>
						<Tab label="Current source" />
					</Box>
				</Grid>
				<Grid item xs={3} md={3} lg={3}>
					<Box
						display="flex"
						alignContent="center"
						justifyContent="flex-end"
					>
						<Tooltip title="Highlight changes" placement="top">
							<ToggleButton
								value="check"
								size="small"
								selected={highlightChanges}
								onChange={() =>
									this.setState({
										highlightChanges: !highlightChanges,
									})
								}
							>
								<HighlightIcon fontSize="small" />
							</ToggleButton>
						</Tooltip>
					</Box>
				</Grid>
				<Grid item xs={12} md={12} lg={12}>
					<Paper
						variant="outlined"
						style={{ height: "100%", minHeight: 400 }}
					>
						<CodeMerge
							highlightChanges={highlightChanges}
							style={{ height: "100%" }}
							context={this.evaluationContext()}
							leftCode={
								selectedChange
									? selectedChange.sourceCode()
									: ""
							}
							rightCode={
								selectedChange
									? selectedChange.currentSourceCode()
									: ""
							}
						/>
					</Paper>
				</Grid>
			</Grid>
		);
	}
}

export default ChangesBrowser;
