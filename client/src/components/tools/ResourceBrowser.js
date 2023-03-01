import React, { Component } from "react";
import {
	Grid,
	Paper,
	List,
	ListItem,
	ListItemText,
	Box,
	Button,
	IconButton,
	Typography,
} from "@material-ui/core";
import CustomTable from "../controls/CustomTable";
import { ide } from "../IDE";
import InspectorIcon from "../icons/InspectorIcon";
import WorkspaceIcon from "../icons/WorkspaceIcon";
import DebuggerIcon from "../icons/DebuggerIcon";
import TestRunnerIcon from "../icons/TestRunnerIcon";
import MemoryIcon from "../icons/MemoryIcon";
import MemoryStats from "./MemoryStats";
import RefreshIcon from "@material-ui/icons/Refresh";
import DeleteIcon from "@material-ui/icons/Delete";
import StopIcon from "@material-ui/icons/Stop";

class ResourceBrowser extends Component {
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
					resources = await ide.api.objects();
					break;
				case "Evaluations":
					resources = await ide.api.evaluations();
					break;
				case "Workspaces":
					resources = await ide.api.workspaces();
					break;
				case "Debuggers":
					resources = await ide.api.debuggers();
					break;
				case "Test Runs":
					resources = await ide.api.testRuns();
					break;
				default:
			}
		} catch (error) {
			ide.reportError(error);
		}
		this.setState({ selectedType: type, resources: resources });
	};

	resourceIcon(type) {
		var icon;
		switch (type) {
			case "Objects":
				icon = <InspectorIcon />;
				break;
			case "Evaluations":
				icon = <InspectorIcon />;
				break;
			case "Workspaces":
				icon = <WorkspaceIcon />;
				break;
			case "Debuggers":
				icon = <DebuggerIcon />;
				break;
			case "Test Runs":
				icon = <TestRunnerIcon />;
				break;
			case "Memory":
				icon = <MemoryIcon />;
				break;
			default:
				icon = <InspectorIcon />;
		}
		return icon;
	}

	resourceSelected = (resource) => {
		this.setState({ selectedResource: resource });
	};

	openInspector = (object) => {
		if (object) {
			ide.openInspector(object);
		}
	};

	unpinObject = async (object) => {
		try {
			await ide.api.unpinObject(object.id);
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
			await ide.api.unpinAllObjects();
			this.setState({
				resources: [],
				selectedResource: null,
			});
		} catch (error) {
			ide.reportError(error);
		}
	};

	objectOptions() {
		return [
			{ label: "Inspect", action: this.openInspector },
			{ label: "Unpin", action: this.unpinObject },
		];
	}

	cancelEvaluation = async (evaluation) => {
		try {
			await ide.api.cancelEvaluation(evaluation.id);
			this.setState({
				resources: this.state.resources.filter(
					(r) => r.id !== evaluation.id
				),
			});
		} catch (error) {
			ide.reportError(error);
		}
	};

	evaluationOptions() {
		return [{ label: "Stop", action: this.cancelEvaluation }];
	}

	openWorkspace = (workspace) => {
		if (workspace) {
			ide.openWorkspace(workspace.id);
		}
	};

	deleteWorkspace = async (w) => {
		if (w) {
			try {
				await ide.api.deleteWorkspace(w.id);
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

	workspaceOptions() {
		return [
			{ label: "Open", action: this.openWorkspace },
			{ label: "Delete", action: this.deleteWorkspace },
		];
	}

	openDebugger = (d) => {
		if (d) {
			ide.openDebugger(d.id, d.description);
		}
	};

	terminateDebugger = async (d) => {
		if (d) {
			try {
				await ide.api.terminateDebugger(d.id);
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

	openTestRun = (t) => {
		if (t) {
			ide.openTestRunner(t.id, t.name);
		}
	};

	debuggerOptions() {
		return [
			{ label: "Open", action: this.openDebugger },
			{ label: "Terminate", action: this.terminateDebugger },
		];
	}

	testRunOptions() {
		return [{ label: "Open", action: this.openTestRun }];
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

	evaluationActions() {
		return [
			{
				label: "Stop",
				icon: <StopIcon fontSize="small" />,
				handler: this.cancelEvaluation,
			},
		];
	}

	workspaceActions() {
		return [
			{
				label: "Open",
				icon: <InspectorIcon fontSize="small" />,
				handler: this.openWorkspace,
			},
			{
				label: "Delete",
				icon: <DeleteIcon fontSize="small" />,
				handler: this.deleteWorkspace,
			},
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

	testRunActions() {
		return [];
	}

	objectColumns() {
		return [
			{ field: "id", label: "ID", align: "left" },
			{ field: "class", label: "Class", align: "left", minWidth: 200 },
			{
				field: "printString",
				label: "Print String",
				minWidth: 200,
				align: "left",
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

	workspaceColumns() {
		return [
			{ field: "id", label: "ID", align: "left" },
			{ field: "owner", label: "Owner", align: "center" },
		];
	}

	debuggerColumns() {
		return [
			{ field: "id", label: "ID", align: "left" },
			{ field: "creator", label: "Creator", align: "center" },
			{
				field: "description",
				label: "Description",
				align: "left",
				minWidth: 200,
			},
		];
	}

	testRunColumns() {
		return [
			{ field: "id", label: "ID", align: "left" },
			{ field: "name", label: "Name", align: "left" },
			{ field: "total", label: "Tests", align: "right" },
			{ field: "running", label: "Running", align: "center" },
		];
	}

	resourceColumns() {
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

	render() {
		const { selectedType, resources } = this.state;
		const styles = this.props.styles;
		return (
			<Grid container spacing={1} style={{ height: "100%" }}>
				<Grid item xs={2} md={2} lg={2}>
					<List>
						{this.types.map((type) => (
							<ListItem
								button
								key={type}
								selected={type === selectedType}
								onClick={(event) => this.typeSelected(type)}
							>
								{this.resourceIcon(type)}
								<ListItemText
									primary={<Box pl={1}>{type}</Box>}
								/>
							</ListItem>
						))}
					</List>
				</Grid>
				<Grid item xs={10} md={10} lg={10}>
					<Grid container spacing={1}>
						<Grid item xs={11} md={11} lg={11}>
							<Typography variant="h6" align="center">
								{selectedType}
							</Typography>
						</Grid>
						<Grid item xs={1} md={1} lg={1}>
							{selectedType === "Objects" &&
								resources &&
								resources.length > 0 && (
									<Button
										variant="text"
										startIcon={<DeleteIcon />}
										onClick={this.unpinAllObjects}
									>
										Unpin All
									</Button>
								)}
							<IconButton
								color="inherit"
								onClick={() => this.typeSelected(selectedType)}
							>
								<RefreshIcon fontSize="small" />
							</IconButton>
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
										styles={styles}
										columns={this.resourceColumns()}
										rows={resources}
										onSelect={this.resourceSelected}
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
