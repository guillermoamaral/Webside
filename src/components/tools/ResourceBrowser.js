import React from "react";
import Tool from "./Tool";
import {
	Paper,
	List,
	ListItemButton,
	ListItemText,
	Box,
	IconButton,
	Tooltip,
	FormGroup,
	FormControlLabel,
	Checkbox,
	Typography,
	TextField,
} from "@mui/material";
import CustomTable from "../controls/CustomTable";
import { ide } from "../IDE";
import InspectorIcon from "../icons/InspectorIcon";
import ProcessIcon from "../icons/ProcessIcon";
import ObjectsIcon from "../icons/ObjectsIcon";
import WorkspaceIcon from "../icons/WorkspaceIcon";
import DebuggerIcon from "../icons/DebuggerIcon";
import TestRunnerIcon from "../icons/TestRunnerIcon";
import StatsIcon from "@mui/icons-material/Troubleshoot";
import SystemStats from "./SystemStats";
import RefreshIcon from "@mui/icons-material/Refresh";
import DeleteIcon from "@mui/icons-material/Delete";
import StopIcon from "@mui/icons-material/Stop";
import PauseIcon from "@mui/icons-material/Pause";
import EditWorkspace from "@mui/icons-material/Edit";
import CustomSplit from "../controls/CustomSplit";

class ResourceBrowser extends Tool {
	constructor(props) {
		super(props);
		this.types = [
			"Objects",
			"Evaluations",
			"Workspaces",
			"Debuggers",
			"Test Runs",
			"Stats",
		];
		this.state = {
			selectedType: null,
			resources: [],
			selectedResource: null,
			automaticRefresh: false,
			refreshFrequency: 1000,
			stats: [],
		};
	}

	aboutToSelect() {
		super.aboutToSelect();
		if (this.state.automaticRefresh) this.startRefreshInterval();
	}

	aboutToClose() {
		this.stopRefreshInterval();
	}

	aboutToDeselect() {
		this.stopRefreshInterval();
	}

	componentDidMount() {
		this.typeSelected("Objects");
	}

	startRefreshInterval() {
		this.stopRefreshInterval();
		this.refreshInterval = setInterval(async () => {
			try {
				this.refreshResources();
			} catch (ignored) {}
		}, this.state.refreshFrequency);
	}

	stopRefreshInterval() {
		if (!this.refreshInterval) return;
		clearInterval(this.refreshInterval);
		this.refreshInterval = null;
	}

	automaticRefreshClicked(boolean) {
		boolean ? this.startRefreshInterval() : this.stopRefreshInterval();
		this.setState({ automaticRefresh: boolean });
	}

	refreshFrequencyChanged(frequency) {
		if (this.state.automaticRefresh) this.startRefreshInterval();
		this.setState({ refreshFrequency: frequency });
	}

	// Objects...

	objectColumns() {
		return [
			{
				field: "id",
				link: this.openInspector,
				label: "ID",
				align: "left",
			},
			{ field: "class", label: "Class", align: "left", minWidth: 200 },
			{
				field: "printString",
				label: "Print String",
				minWidth: 200,
				align: "left",
			},
		];
	}

	objectOptions() {
		return [
			{ label: "Inspect", action: this.openInspector },
			{ label: "Unpin", action: this.unpinObject },
		];
	}

	objectActions() {
		return [
			{
				label: "Inspect",
				icon: <InspectorIcon fontSize="small" />,
				handler: this.openInspector,
			},
			{
				label: "Unpin",
				icon: <DeleteIcon fontSize="small" />,
				handler: this.unpinObject,
			},
		];
	}

	openInspector = (object) => {
		if (object) {
			this.context.openInspector(object);
		}
	};

	unpinObject = async (object) => {
		try {
			await ide.backend.unpinObject(object.id);
			this.setState({
				resources: this.state.resources.filter(
					(r) => r.id !== object.id
				),
				selectedResource: null,
			});
		} catch (error) {
			ide.reportError(error);
		}
	};

	unpinAllObjects = async () => {
		try {
			await ide.backend.unpinAllObjects();
			this.setState({
				resources: [],
				selectedResource: null,
			});
		} catch (error) {
			ide.reportError(error);
		}
	};

	// Evaluations...

	evaluationColumns() {
		return [
			{ field: "id", label: "ID", align: "left" },
			{
				field: "expression",
				label: "Expression",
				align: "left",
				minWidth: 200,
			},
			{
				field: "state",
				label: "State",
				minWidth: 200,
				align: "left",
			},
		];
	}

	evaluationOptions() {
		return [
			{ label: "Pause", action: this.pauseEvaluation },
			{ label: "Stop", action: this.cancelEvaluation },
		];
	}

	evaluationActions() {
		return [
			{
				label: "Pause",
				icon: <PauseIcon fontSize="small" />,
				handler: this.pauseEvaluation,
				visible: this.canPauseEvaluation,
			},
			{
				label: "Stop",
				icon: <StopIcon fontSize="small" />,
				handler: this.cancelEvaluation,
				visible: this.canCancelEvaluation,
			},
		];
	}

	canPauseEvaluation = async (evaluation) => {
		return evaluation && evaluation.state === "evaluating";
	};

	pauseEvaluation = async (evaluation) => {
		try {
			await ide.backend.pauseEvaluation(evaluation.id);
			const d = await ide.backend.createDebugger(evaluation.id);
			this.context.openDebugger(d.id, d.description);
		} catch (error) {
			ide.reportError(error);
		}
	};

	canCancelEvaluation = async (evaluation) => {
		return (
			evaluation &&
			(evaluation.state === "evaluating" || evaluation.state === "paused")
		);
	};

	cancelEvaluation = async (evaluation) => {
		try {
			await ide.backend.cancelEvaluation(evaluation.id);
			this.setState({
				resources: this.state.resources.filter(
					(r) => r.id !== evaluation.id
				),
			});
		} catch (error) {
			ide.reportError(error);
		}
	};

	// Workspaces...

	workspaceColumns() {
		return [
			{
				field: "id",
				link: this.openWorkspace,
				label: "ID",
				align: "left",
			},
			{
				field: (w) => {
					const source = w.source.trim();
					return source.length > 100
						? source.substr(0, 99) + "…"
						: source;
				},
				label: "Contents",
				align: "left",
			},
			{ field: "owner", label: "Owner", align: "center" },
		];
	}

	workspaceOptions() {
		return [
			{ label: "Open", action: this.openWorkspace },
			{ label: "Delete", action: this.deleteWorkspace },
		];
	}

	workspaceActions() {
		return [
			{
				label: "Open",
				icon: <EditWorkspace fontSize="small" />,
				handler: this.openWorkspace,
			},
			{
				label: "Delete",
				icon: <DeleteIcon fontSize="small" />,
				handler: this.deleteWorkspace,
			},
		];
	}

	openWorkspace = (workspace) => {
		if (workspace) {
			this.context.openWorkspace(workspace.id);
		}
	};

	deleteWorkspace = async (w) => {
		if (w) {
			try {
				await ide.backend.deleteWorkspace(w.id);
				this.setState({
					resources: this.state.resources.filter(
						(r) => r.id !== w.id
					),
					selectedResource: null,
				});
			} catch (error) {
				ide.reportError(error);
			}
		}
	};

	// Debuggers...

	debuggerColumns() {
		return [
			{
				field: "id",
				link: this.openDebugger,
				label: "ID",
				align: "left",
			},
			{
				field: "description",
				label: "Description",
				align: "left",
				minWidth: 200,
			},
			{ field: "creator", label: "Creator", align: "center" },
		];
	}

	debuggerOptions() {
		return [
			{ label: "Open", action: this.openDebugger },
			{ label: "Terminate", action: this.terminateDebugger },
		];
	}

	debuggerActions() {
		return [
			{
				label: "Terminate",
				icon: <StopIcon fontSize="small" />,
				handler: this.terminateDebugger,
			},
		];
	}

	openDebugger = (d) => {
		if (d) {
			this.context.openDebugger(d.id, d.description);
		}
	};

	terminateDebugger = async (d) => {
		if (d) {
			try {
				await ide.backend.terminateDebugger(d.id);
				this.setState({
					resources: this.state.resources.filter(
						(r) => r.id !== d.id
					),
					selectedResource: null,
				});
			} catch (error) {
				ide.reportError(error);
			}
		}
	};

	// Test runs...

	testRunColumns() {
		return [
			{ field: "id", link: this.openTestRun, label: "ID", align: "left" },
			{ field: "name", label: "Name", align: "left" },
			{ field: "total", label: "Tests", align: "right" },
			{ field: "running", label: "Running", align: "center" },
		];
	}

	testRunOptions() {
		return [
			{ label: "Open", action: this.openTestRun },
			{ label: "Delete", action: this.openTestRun },
		];
	}

	testRunActions() {
		return [
			{
				label: "Delete",
				icon: <DeleteIcon fontSize="small" />,
				handler: this.deleteTestRun,
			},
		];
	}

	openTestRun = (run) => {
		if (run) {
			this.context.openTestRunner(run.id, run.name);
		}
	};

	deleteTestRun = async (run) => {
		if (run) {
			try {
				await ide.backend.deleteTestRun(run.id);
				this.setState({
					resources: this.state.resources.filter(
						(r) => r.id !== run.id
					),
					selectedResource: null,
				});
			} catch (error) {
				ide.reportError(error);
			}
		}
	};

	// Stats...

	statsColumns() {
		return [
			{
				field: "label",
				label: "Label",
				align: "left",
			},
			{
				field: (s) => parseFloat(s.value.toFixed(2)),
				label: "Value",
				align: "right",
			},
			{ field: "unit", label: "Unit", align: "left" },
		];
	}

	// Common...

	updateResources = async (type) => {
		var resources;
		try {
			switch (type) {
				case "Objects":
					resources = await ide.backend.objects();
					break;
				case "Evaluations":
					resources = await ide.backend.evaluations();
					break;
				case "Workspaces":
					resources = await ide.backend.workspaces();
					break;
				case "Debuggers":
					resources = await ide.backend.debuggers();
					break;
				case "Test Runs":
					resources = await ide.backend.testRuns();
					break;
				case "Stats":
					resources = await ide.backend.systemStats();
					break;
				default:
			}
		} catch (error) {
			ide.reportError(error);
		}
		const stats = this.state.stats;
		if (type === "Stats") stats.push(...resources);
		this.setState({
			selectedType: type,
			resources: resources,
			stats: stats,
		});
	};

	resourceIcon(type, color) {
		var icon;
		switch (type) {
			case "Objects":
				icon = <ObjectsIcon color={color} />;
				break;
			case "Evaluations":
				icon = <ProcessIcon color={color} />;
				break;
			case "Workspaces":
				icon = <WorkspaceIcon color={color} />;
				break;
			case "Debuggers":
				icon = <DebuggerIcon color={color} />;
				break;
			case "Test Runs":
				icon = <TestRunnerIcon color={color} />;
				break;
			case "Stats":
				icon = <StatsIcon color={color} />;
				break;
			default:
				icon = <InspectorIcon color={color} />;
		}
		return icon;
	}

	typeSelected = (type) => {
		this.updateResources(type);
	};

	refreshResources = () => {
		this.updateResources(this.state.selectedType);
	};

	resourceSelected = (resource) => {
		this.setState({ selectedResource: resource });
	};

	columns() {
		var columns;
		switch (this.state.selectedType) {
			case "Objects":
				columns = this.objectColumns();
				break;
			case "Evaluations":
				columns = this.evaluationColumns();
				break;
			case "Workspaces":
				columns = this.workspaceColumns();
				break;
			case "Debuggers":
				columns = this.debuggerColumns();
				break;
			case "Test Runs":
				columns = this.testRunColumns();
				break;
			case "Stats":
				columns = this.statsColumns();
				break;
			default:
		}
		return columns;
	}

	menuOptions() {
		var options;
		switch (this.state.selectedType) {
			case "Objects":
				options = this.objectOptions();
				break;
			case "Evaluations":
				options = this.evaluationOptions();
				break;
			case "Workspaces":
				options = this.workspaceOptions();
				break;
			case "Debuggers":
				options = this.debuggerOptions();
				break;
			case "Test Runs":
				options = this.testRunOptions();
				break;
			default:
		}
		return options;
	}

	rowActions() {
		var options;
		switch (this.state.selectedType) {
			case "Objects":
				options = this.objectActions();
				break;
			case "Evaluations":
				options = this.evaluationActions();
				break;
			case "Workspaces":
				options = this.workspaceActions();
				break;
			case "Debuggers":
				options = this.debuggerActions();
				break;
			case "Test Runs":
				options = this.testRunActions();
				break;
			default:
		}
		return options;
	}

	render() {
		console.log("rendering resource browser");
		const {
			selectedType,
			resources,
			selectedResource,
			automaticRefresh,
			refreshFrequency,
			stats,
		} = this.state;
		return (
			<Box display="flex" flexDirection="column" sx={{ height: "100%" }}>
				<Box flexGrow={1}>
					<CustomSplit>
						<Box sx={{ width: "20%", minWidth: "15%" }}>
							{" "}
							<List>
								{this.types.map((type) => {
									const color =
										type === selectedType
											? "primary"
											: "inherit";
									return (
										<ListItemButton
											key={type}
											selected={type === selectedType}
											onClick={(event) =>
												this.typeSelected(type)
											}
										>
											<Box pt={0.5}>
												{this.resourceIcon(type, color)}
											</Box>
											<ListItemText
												primaryTypographyProps={{
													color: color,
												}}
												primary={
													<Box pl={1}>{type}</Box>
												}
											/>
										</ListItemButton>
									);
								})}
							</List>
						</Box>
						<Box
							display="flex"
							flexDirection="column"
							sx={{ width: "80%" }}
						>
							<Box>
								<Box display="flex" justifyContent="flex-end">
									{selectedType === "Objects" &&
										resources &&
										resources.length > 0 && (
											<Tooltip
												title="Unpin all"
												placement="top"
											>
												<span>
													<IconButton
														color="inherit"
														onClick={
															this.unpinAllObjects
														}
													>
														<DeleteIcon fontSize="small" />
													</IconButton>
												</span>
											</Tooltip>
										)}
									{!automaticRefresh && (
										<Tooltip
											title="Refresh"
											placement="top"
										>
											<span>
												<IconButton
													color="inherit"
													onClick={
														this.refreshResources
													}
												>
													<RefreshIcon fontSize="small" />
												</IconButton>
											</span>
										</Tooltip>
									)}
								</Box>
							</Box>
							<Box
								flexGrow={1}
								//style={{ minHeight: 500 }}
							>
								{selectedType &&
									(selectedType !== "Stats" ||
										!automaticRefresh) && (
										<Paper
											variant="outlined"
											style={{ height: "100%" }}
										>
											<CustomTable
												columns={this.columns()}
												rows={resources}
												onRowSelect={
													this.resourceSelected
												}
												menuOptions={this.menuOptions()}
												rowActions={this.rowActions()}
												selectedRow={selectedResource}
											/>
										</Paper>
									)}
								{selectedType === "Stats" &&
									automaticRefresh && (
										<SystemStats stats={stats} />
									)}
							</Box>
						</Box>
					</CustomSplit>
				</Box>
				<Box display="flex" flexDirection="row" alignItems="center">
					<FormGroup>
						<FormControlLabel
							control={
								<Checkbox
									checked={automaticRefresh}
									onChange={(event) =>
										this.automaticRefreshClicked(
											event.target.checked
										)
									}
								/>
							}
							label="Automatic refresh"
							size="small"
						/>
					</FormGroup>
					{automaticRefresh && (
						<Typography ml={1} mr={2}>
							Frequency (ms):
						</Typography>
					)}
					{automaticRefresh && (
						<TextField
							sx={{ minWidth: 50 }}
							size="small"
							type="number"
							margin="dense"
							name="refreshFrequency"
							variant="outlined"
							InputProps={{
								inputProps: { min: 300, max: 10000 },
							}}
							value={refreshFrequency}
							inputProps={{
								style: { textAlign: "right" },
							}}
							onChange={(event) => {
								this.refreshFrequencyChanged(
									parseInt(event.target.value)
								);
							}}
							required
						/>
					)}
				</Box>
			</Box>
		);
	}
}

export default ResourceBrowser;
