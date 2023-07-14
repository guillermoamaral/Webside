import React, { Component } from "react";
import {
	Grid,
	Paper,
	List,
	ListItem,
	ListItemText,
	Box,
	IconButton,
	Tooltip,
} from "@mui/material";
import CustomTable from "../controls/CustomTable";
import { ide } from "../IDE";
import ToolContainerContext from "../ToolContainerContext";
import InspectorIcon from "../icons/InspectorIcon";
import ProcessIcon from "../icons/ProcessIcon";
import ObjectsIcon from "../icons/ObjectsIcon";
import WorkspaceIcon from "../icons/WorkspaceIcon";
import DebuggerIcon from "../icons/DebuggerIcon";
import TestRunnerIcon from "../icons/TestRunnerIcon";
import MemoryIcon from "../icons/MemoryIcon";
import MemoryStats from "./MemoryStats";
import RefreshIcon from "@mui/icons-material/Refresh";
import DeleteIcon from "@mui/icons-material/Delete";
import StopIcon from "@mui/icons-material/Stop";
import EditWorkspace from "@mui/icons-material/Edit"

class ResourceBrowser extends Component {
	static contextType = ToolContainerContext;

	constructor(props) {
		super(props);
		this.types = [
			"Objects",
			"Evaluations",
			"Workspaces",
			"Debuggers",
			"Test Runs",
			"Memory",
		];
		this.state = {
			selectedType: null,
			resources: [],
			selectedResource: null,
		};
	}

	componentDidMount() {
		this.typeSelected("Objects");
	}

	typeSelected = async (type) => {
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
				default:
			}
		} catch (error) {
			ide.reportError(error);
		}
		this.setState({ selectedType: type, resources: resources });
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
			case "Memory":
				icon = <MemoryIcon color={color} />;
				break;
			default:
				icon = <InspectorIcon color={color} />;
		}
		return icon;
	}

	resourceSelected = (resource) => {
		this.setState({ selectedResource: resource });
	};

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

	openTestRun = (run) => {
		if (run) {
			this.context.openTestRunner(run.id, run.name);
		}
	};

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
		return [{ label: "Stop", action: this.cancelEvaluation }];
	}

	evaluationActions() {
		return [
			{
				label: "Stop",
				icon: <StopIcon fontSize="small" />,
				handler: this.cancelEvaluation,
			},
		];
	}

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

	testRunColumns() {
		return [
			{ field: "id", link: this.openTestRun, label: "ID", align: "left" },
			{ field: "name", label: "Name", align: "left" },
			{ field: "total", label: "Tests", align: "right" },
			{ field: "running", label: "Running", align: "center" },
		];
	}

	testRunOptions() {
		return [{ label: "Open", action: this.openTestRun }];
	}

	testRunActions() {
		return [];
	}

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
		const { selectedType, resources } = this.state;
		return (
			<Grid container spacing={1} style={{ height: "100%" }}>
				<Grid item xs={2} md={2} lg={2}>
					<List>
						{this.types.map((type) => {
							const color =
								type === selectedType ? "primary" : "inherit";
							return (
								<ListItem
									button
									key={type}
									selected={type === selectedType}
									onClick={(event) => this.typeSelected(type)}
								>
									<Box pt={0.5}>
										{this.resourceIcon(type, color)}
									</Box>
									<ListItemText
										primaryTypographyProps={{
											color: color,
										}}
										primary={<Box pl={1}>{type}</Box>}
									/>
								</ListItem>
							);
						})}
					</List>
				</Grid>
				<Grid item xs={10} md={10} lg={10}>
					<Grid container spacing={1}>
						<Grid item xs={12} md={12} lg={12}>
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
								<Tooltip title="Refresh" placement="top">
									<span>
										<IconButton
											color="inherit"
											onClick={() =>
												this.typeSelected(selectedType)
											}
										>
											<RefreshIcon fontSize="small" />
										</IconButton>
									</span>
								</Tooltip>
							</Box>
						</Grid>
						<Grid
							item
							xs={12}
							md={12}
							lg={12}
							style={{ minHeight: 500 }}
						>
							{selectedType && selectedType !== "Memory" && (
								<Paper
									variant="outlined"
									style={{ height: "100%" }}
								>
									<CustomTable
										columns={this.columns()}
										rows={resources}
										onRowSelect={this.resourceSelected}
										menuOptions={this.menuOptions()}
										rowActions={this.rowActions()}
									/>
								</Paper>
							)}
							{selectedType === "Memory" && <MemoryStats />}
						</Grid>
					</Grid>
				</Grid>
			</Grid>
		);
	}
}

export default ResourceBrowser;
