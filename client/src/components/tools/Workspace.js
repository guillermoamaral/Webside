import React, { Component } from "react";
import {
	Grid,
	Paper,
	Accordion,
	AccordionSummary,
	Typography,
	IconButton,
	Box,
} from "@material-ui/core";
import CloseIcon from "@material-ui/icons/Close";
import PlayIcon from "@material-ui/icons/PlayArrow";
import ExpandMoreIcon from "@material-ui/icons/ExpandMore";
import InspectorIcon from "../icons/InspectorIcon";
import CodeEditor from "../parts/CodeEditor";
import Inspector from "./Inspector";
import { IDEContext } from "../IDEContext";

class Workspace extends Component {
	static contextType = IDEContext;

	constructor(props) {
		super(props);
		this.state = {
			expression: "",
			opensInspector: true,
			inspectors: [],
			evaluating: false,
		};
	}

	openInspector(object) {
		const inspector = (
			<Inspector
				key={object.id}
				styles={this.props.styles}
				root={object}
				showWorkspace={false}
			/>
		);
		const inspectors = this.state.inspectors;
		inspectors.unshift(inspector);
		this.setState({ inspectors: inspectors });
	}

	closeInspector = async (event, id) => {
		event.stopPropagation();
		try {
			await this.context.api.unpinObject(id);
			this.setState({
				inspectors: this.state.inspectors.filter((i) => i.key !== id),
			});
		} catch (error) {
			this.context.reportError(error);
		}
	};

	expressionChanged = (text) => {
		this.setState({ expression: text });
	};

	evaluateClicked = async () => {
		try {
			this.setState({ evaluating: true });
			const object = await this.context.evaluateExpression(
				this.state.expression,
				false,
				true,
				{ workspace: this.props.id }
			);
			if (this.state.opensInspector) {
				this.setState({ evaluating: false });
				this.openInspector(object);
			} else {
				this.setState({
					expression: this.state.expression + " -> " + object.printString,
					evaluating: false,
				});
			}
		} catch (error) {
			this.setState({ evaluating: false });
		}
	};

	render() {
		const { expression, inspectors, evaluating } = this.state;
		return (
			<Grid container spacing={1}>
				<Grid item xs={12} md={8} lg={8}>
					<Grid item xs={12} md={12} lg={12}>
						<Paper variant="outlined">
							<CodeEditor
								context={{ workspace: this.props.id }}
								styles={this.props.styles}
								lineNumbers={false}
								source={expression}
								showAccept={true}
								acceptIcon={<PlayIcon />}
								onAccept={this.evaluateClicked}
								onChange={this.expressionChanged}
								evaluating={evaluating}
							/>
						</Paper>
					</Grid>
				</Grid>
				<Grid item xs={12} md={4} lg={4}>
					{inspectors.map((inspector, index) => {
						return (
							<Accordion key={inspector.key} defaultExpanded>
								<AccordionSummary
									expandIcon={<ExpandMoreIcon />}
									style={{ maxHeight: 15 }}
								>
									<Box
										display="flex"
										flexWrap="nowrap"
										alignItems="center"
										justifyContent="center"
									></Box>
									<Box>
										<IconButton
											onClick={(event) => {
												this.closeInspector(event, inspector.key);
											}}
											size="small"
										>
											<CloseIcon fontSize="small" />
										</IconButton>
									</Box>
									<Box pt={0.5} pr={1}>
										<InspectorIcon />
									</Box>
									<Box>
										<Typography>
											{"Inspecting: " + inspector.props.root.class}
										</Typography>
									</Box>
								</AccordionSummary>
								{inspector}
							</Accordion>
						);
					})}
				</Grid>
			</Grid>
		);
	}
}

export default Workspace;
