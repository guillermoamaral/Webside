import React from "react";
import Tool from "./Tool";
import {
	Paper,
	Accordion,
	AccordionSummary,
	Typography,
	IconButton,
	Box,
} from "@mui/material";
import CloseIcon from "@mui/icons-material/Close";
import ExpandMoreIcon from "@mui/icons-material/ExpandMore";
import CodeEditor from "../parts/CodeEditor";
import Inspector from "./Inspector";
import { ide } from "../IDE";
import CustomSplit from "../controls/CustomSplit";

class Workspace extends Tool {
	constructor(props) {
		super(props);
		this.state = {
			source: this.props.source || "",
			opensInspector: true,
			inspectors: [],
		};
		this.editorRef = React.createRef();
	}

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
		this.setState({ source: source });
		ide.backend.saveWorkspace({ id: this.props.id, source: source });
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
		} catch (error) {}
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
			this.context.reportError(error);
		}
	};

	render() {
		const { source, inspectors } = this.state;
		const background = ide.colorSetting("workspaceColor");
		return (
			<Box
				display="flex"
				sx={{ height: "100%", padding: 1, background: background }}
			>
				<CustomSplit>
					<Box flexGrow={1} sx={{ width: "50%" }}>
						<Paper
							variant="outlined"
							style={{ minHeight: 300, height: "100%" }}
						>
							<CodeEditor
								ref={this.editorRef}
								context={this.evaluationContext()}
								source={source}
								showAccept={false}
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
											expandIcon={<ExpandMoreIcon />}
											iconbuttonprops={{ size: "small" }}
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
														onClick={(event) => {
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
													<Typography>
														{"Inspecting: " +
															inspector.props.root
																.class}
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
			</Box>
		);
	}
}

export default Workspace;
