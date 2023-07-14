import React, { Component } from "react";
import {
	Grid,
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
import ToolContainerContext from "../ToolContainerContext";

class Workspace extends Component {
	static contextType = ToolContainerContext;

	constructor(props) {
		super(props);
		this.state = {
			source: this.props.source || "",
			opensInspector: true,
			inspectors: [],
			evaluating: false,
		};
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

	saveSource = (source) => {
		ide.backend.saveWorkspace({ id: this.props.id, source: source });
		this.setState({ source: source });
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

	render() {
		const { source, inspectors, evaluating } = this.state;
		const editorWidth = inspectors.length > 0 ? 8 : 12;
		return (
			<Grid container spacing={1}>
				<Grid item xs={12} md={editorWidth} lg={editorWidth}>
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
							onChange={this.saveSource}
							evaluating={evaluating}
						/>
					</Paper>
				</Grid>
				{inspectors.length > 0 && (
					<Grid item xs={12} md={4} lg={4}>
						{inspectors.map((inspector, index) => {
							return (
								<Accordion key={inspector.key} defaultExpanded>
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
					</Grid>
				)}
			</Grid>
		);
	}
}

export default Workspace;
