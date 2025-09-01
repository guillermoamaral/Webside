import React from "react";
import Tool from "./Tool";
import {
	Paper,
	Accordion,
	AccordionSummary,
	Typography,
	IconButton,
	Box,
	Link,
	TextField,
} from "@mui/material";
import CloseIcon from "@mui/icons-material/Close";
import ExpandMoreIcon from "@mui/icons-material/ExpandMore";
import Inspector from "./Inspector";
import { ide } from "../IDE";
import CustomSplit from "../controls/CustomSplit";
import NotebookView from "./NotebookView";
import Notebook from "../../model/Notebook";
import Scrollable from "../controls/Scrollable";
import ShareIcon from "@mui/icons-material/Share";
import CodeEditorBackend from "../parts/CodeEditorBackend";

class Workspace extends Tool {
	constructor(props) {
		super(props);
		this.autoSaveFrequency = 5000;
		this.state = {
			source: props.source || "",
			opensInspector: true,
			inspectors: [],
			name: props.name || "",
			editingName: false,
			notebook: props.useNotebookView
				? Notebook.fromSource(props.source || "")
				: null,
			useNotebookView:
				typeof props.useNotebookView === "boolean"
					? props.useNotebookView
					: false,
		};
		this.editorRef = React.createRef();
		this.pendingSave = false;
		this.saveTimer = null;
	}

	componentDidMount() {
		this.autoSaveInterval = setInterval(
			this.autoSave,
			this.autoSaveFrequency
		);
	}

	componentWillUnmount() {
		clearInterval(this.autoSaveInterval);
	}

	autoSave = async () => {
		if (!this.pendingSave) return;
		this.pendingSave = false;
		this.save();
	};

	save = async () => {
		const { source, name } = this.state;
		try {
			await ide.backend.saveWorkspace({
				id: this.props.id,
				source: source,
				name: name,
			});
			this.updateLabel(name);
		} catch (error) {
			ide.reportError(error);
		}
	};

	aboutToSelect() {
		super.aboutToSelect();
		if (this.editorRef && this.editorRef.current)
			this.editorRef.current.updatePlay();
	}

	async aboutToClose() {
		try {
			await ide.backend.deleteWorkspace(this.props.id);
		} catch (error) {
			ide.reportError(error);
		}
	}

	openInspector(object) {
		const inspector = (
			<Inspector
				key={object.id}
				root={object}
				showWorkspace={false}
				embedded={true}
			/>
		);
		const inspectors = this.state.inspectors;
		inspectors.unshift(inspector);
		this.setState({ inspectors: inspectors });
	}

	closeInspector = async (event, id) => {
		event.stopPropagation();
		try {
			await ide.backend.unpinObject(id);
			this.setState({
				inspectors: this.state.inspectors.filter((i) => i.key !== id),
			});
		} catch (error) {
			ide.reportError(error);
		}
	};

	sourceChanged = (source) => {
		if (source === this.state.source) return;
		this.setState({ source });
		this.pendingSave = true;
	};

	notebookChanged = (notebook) => {
		this.sourceChanged(notebook.toSource());
	};

	expressionEvaluated = (object) => {
		if (this.state.opensInspector) this.openInspector(object);
	};

	evaluationContext() {
		return {
			workspace: this.props.id,
		};
	}

	tooltipForBinding = async (name) => {
		let binding;
		try {
			const bindings = await ide.backend.workspaceBindings(this.props.id);
			binding = bindings.find((b) => b.name === name);
		} catch (ignored) {
			console.log(ignored);
		}
		if (binding) return binding.value;
	};

	inspectBinding = async (name) => {
		try {
			const object = await this.context.evaluateExpression(
				name,
				false,
				true,
				this.evaluationContext()
			);
			this.context.openInspector(object);
		} catch (error) {
			ide.reportError(error);
		}
	};

	toggleNotebookView = (event) => {
		event.preventDefault();
		let { useNotebookView, notebook, source } = this.state;
		if (!useNotebookView) notebook = Notebook.fromSource(source);
		this.setState({
			useNotebookView: !useNotebookView,
			notebook: notebook,
		});
	};

	nameChanged = (event) => {
		this.setState({ name: event.target.value });
	};

	copyUrlToClipboard = () => {
		const url = new URL(window.location.href);
		url.hash = url.hash + `&workspace=${this.props.id}`;
		navigator.clipboard.writeText(url.toString()).then(
			() => ide.inform("Workspace link copied to clipboard"),
			(err) => ide.reportError("Failed to copy: " + err)
		);
	};

	render() {
		const {
			name,
			source,
			inspectors,
			notebook,
			useNotebookView,
			editingName,
		} = this.state;
		const background = ide.colorSetting("workspaceColor");
		return (
			<Box
				display="flex"
				sx={{ height: "100%", padding: 1, background: background }}
				flexDirection="column"
			>
				<Box
					sx={{
						display: "flex",
						justifyContent: "space-between",
						alignItems: "center",
						mb: 1,
						ml: 1,
					}}
				>
					<Box
						sx={{
							display: "flex",
							alignItems: "center",
							gap: 1,
							minWidth: 150,
						}}
					>
						<IconButton
							size="small"
							onClick={this.copyUrlToClipboard}
						>
							<ShareIcon fontSize="small" />
						</IconButton>
						<Box
							onDoubleClick={() =>
								this.setState({ editingName: true })
							}
							sx={{ minWidth: 100 }}
						>
							{editingName ? (
								<TextField
									variant="standard"
									autoFocus
									placeholder="[Unnamed]"
									value={name}
									onChange={this.nameChanged}
									onBlur={() =>
										this.setState(
											{ editingName: false },
											this.save
										)
									}
									onKeyDown={(e) => {
										if (e.key === "Enter") {
											this.setState(
												{ editingName: false },
												this.save
											);
										}
									}}
									InputProps={{ disableUnderline: true }}
								/>
							) : (
								<Typography
									variant="normal"
									fontWeight="fontWeightBold"
									color="text.secondary"
								>
									{name || "[Unnamed]"}
								</Typography>
							)}
						</Box>
					</Box>
					<Link
						href="#"
						onClick={this.toggleNotebookView}
						variant="caption"
						sx={{ ml: 2 }}
					>
						{useNotebookView
							? "Switch to standard Workspace"
							: "Switch to Notebook View"}
					</Link>
				</Box>
				{!useNotebookView && (
					<CustomSplit>
						<Box flexGrow={1} sx={{ width: "50%" }}>
							<Paper
								variant="outlined"
								style={{ minHeight: 300, height: "100%" }}
							>
								<CodeEditorBackend
									ref={this.editorRef}
									context={this.evaluationContext()}
									source={source}
									originalSource={""}
									showPlay={true}
									onChange={this.sourceChanged}
									onEvaluate={this.expressionEvaluated}
									onTooltipShow={this.tooltipForBinding}
									onTooltipClick={this.inspectBinding}
								/>
							</Paper>
						</Box>
						{inspectors.length > 0 && (
							<Box sx={{ width: "50%" }}>
								{inspectors.map((inspector, index) => {
									return (
										<Accordion
											key={inspector.key}
											defaultExpanded
										>
											<AccordionSummary
												expandIcon={
													<ExpandMoreIcon fontSize="small" />
												}
												sx={{
													minHeight: 28,
													paddingY: 0.25,
													"&.MuiAccordionSummary-root":
														{
															minHeight: 28,
														},
													"& .MuiAccordionSummary-content":
														{
															marginY: 0.25,
														},
													"& .MuiButtonBase-root": {
														paddingY: 0.25,
														minHeight: 28,
													},
												}}
											>
												<Box
													display="flex"
													flexWrap="nowrap"
													alignItems="center"
													justifyContent="center"
													mt={0}
												>
													<Box>
														<IconButton
															onClick={(
																event
															) => {
																this.closeInspector(
																	event,
																	inspector.key
																);
															}}
															size="small"
														>
															<CloseIcon fontSize="small" />
														</IconButton>
													</Box>
													<Box>
														<Typography variant="body2">
															{"Inspecting: " +
																inspector.props
																	.root.class}
														</Typography>
													</Box>
												</Box>
											</AccordionSummary>
											{inspector}
										</Accordion>
									);
								})}
							</Box>
						)}
					</CustomSplit>
				)}
				{useNotebookView && (
					<Scrollable sx={{ flexGrow: 1 }}>
						<NotebookView
							notebook={notebook}
							onChange={this.notebookChanged}
							evaluationContext={this.evaluationContext()}
						/>
					</Scrollable>
				)}
			</Box>
		);
	}
}

export default Workspace;
