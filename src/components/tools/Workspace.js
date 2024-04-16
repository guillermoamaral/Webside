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
import PlayIcon from "@mui/icons-material/PlayArrow";
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
			evaluating: false,
		};
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

	evaluateClicked = async () => {
		try {
			this.setState({ evaluating: true });
			const object = await this.context.evaluateExpression(
				this.state.source,
				false,
				true,
				{ workspace: this.props.id }
			);
			if (this.state.opensInspector) {
				this.setState({ evaluating: false });
				this.openInspector(object);
			} else {
				this.setState({
					source: this.state.source + " -> " + object.printString,
					evaluating: false,
				});
			}
		} catch (error) {
			this.setState({ evaluating: false });
		}
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
		const { source, inspectors, evaluating } = this.state;
		return (
			<Box display="flex" sx={{ height: "100%" }}>
				<CustomSplit>
					<Box flexGrow={1} sx={{ width: "50%" }}>
						<Paper
							variant="outlined"
							style={{ minHeight: 300, height: "100%" }}
						>
							<CodeEditor
								context={this.evaluationContext()}
								source={source}
								showAccept
								acceptIcon={
									<PlayIcon style={{ color: "#3bba5d" }} />
								}
								onAccept={this.evaluateClicked}
								onChange={this.sourceChanged}
								evaluating={evaluating}
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
