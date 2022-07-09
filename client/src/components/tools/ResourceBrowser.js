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
} from "@material-ui/core";
import CustomTable from "../controls/CustomTable";
import { IDEContext } from "../IDEContext";
import InspectorIcon from "../icons/InspectorIcon";
import WorkspaceIcon from "../icons/WorkspaceIcon";
import DebuggerIcon from "../icons/DebuggerIcon";
import TestRunnerIcon from "../icons/TestRunnerIcon";
import MemoryIcon from "../icons/MemoryIcon";
import MemoryStats from "./MemoryStats";
import RefreshIcon from "@material-ui/icons/Refresh";
import DeleteIcon from "@material-ui/icons/Delete";

class ResourceBrowser extends Component {
	static contextType = IDEContext;

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
					resources = await this.context.api.getObjects();
					break;
				case "Evaluations":
					resources = await this.context.api.getEvaluations();
					break;
				case "Workspaces":
					resources = await this.context.api.getWorkspaces();
					break;
				case "Debuggers":
					resources = await this.context.api.getDebuggers();
					break;
				case "Test Runs":
					resources = await this.context.api.getTestRuns();
					break;
				default:
			}
		} catch (error) {
			this.context.reportError(error);
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

	inspectObject = (object) => {
		if (object) {
			this.context.inspectObject(object);
		}
	};

	unpinObject = async (object) => {
		try {
			await this.context.api.unpinObject(object.id);
			this.setState({
				resources: this.state.resources.filter((r) => r.id !== object.id),
				selectedResource: null,
			});
		} catch (error) {
			this.context.reportError(error);
		}
	};

	unpinAllObjects = async () => {
		try {
			await this.context.api.unpinAllObjects();
			this.setState({
				resources: [],
				selectedResource: null,
			});
		} catch (error) {
			this.context.reportError(error);
		}
	};

	objectOptions() {
		return [
			{ label: "Inspect", action: this.inspectObject },
			{ label: "Unpin", action: this.unpinObject },
		];
	}

	cancelEvaluation = async (evaluation) => {
		try {
			await this.context.api.cancelEvaluation(evaluation.id);
			this.setState({
				resources: this.state.resources.filter((r) => r.id !== evaluation.id),
			});
		} catch (error) {
			this.context.reportError(error);
		}
	};

	evaluationOptions() {
		return [{ label: "Stop", action: this.cancelEvaluation }];
	}

	openWorkspace = (workspace) => {
		if (workspace) {
			this.context.openWorkspace(workspace.id);
		}
	};

	workspaceOptions() {
		return [{ label: "Open", action: this.openWorkspace }];
	}

	openDebugger = (d) => {
		if (d) {
			this.context.openDebugger(d.id, d.description);
		}
	};

	openTestRun = (t) => {
		if (t) {
			this.context.openTestRunner(t.id, t.name);
		}
	};

	debuggerOptions() {
		return [{ label: "Open", action: this.openDebugger }];
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

	resourceColumns(type) {
		var columns;
		switch (type) {
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
		const { selectedType, resources, selectedResource } = this.state;
		const columns = this.resourceColumns(selectedType);
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
								<ListItemText primary={<Box pl={1}>{type}</Box>} />
							</ListItem>
						))}
					</List>
				</Grid>
				<Grid item xs={10} md={10} lg={10}>
					<Grid container spacing={1}>
						<Grid item xs={12} md={12} lg={12}>
							<Box display="flex" justifyContent="flex-end">
								{selectedType === "Objects" && resources.length > 0 && (
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
							</Box>
						</Grid>
						<Grid item xs={12} md={12} lg={12} style={{ minHeight: 500 }}>
							{selectedType && selectedType !== "Memory" && (
								<Paper variant="outlined" style={{ height: "100%" }}>
									<CustomTable
										styles={styles}
										columns={columns}
										rows={resources}
										onSelect={this.resourceSelected}
										menuOptions={this.menuOptions()}
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
