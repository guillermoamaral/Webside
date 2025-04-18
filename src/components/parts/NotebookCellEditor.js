import React, { Component } from "react";
import {
	Paper,
	Box,
	IconButton,
	Tooltip,
	Button,
	CircularProgress,
} from "@mui/material";
import EvaluateIcon from "@mui/icons-material/PlayCircle";
import DeleteIcon from "@mui/icons-material/Delete";
import { ide } from "../IDE";
import CodeEditor from "./CodeEditor";
import MarkdownView from "./MarkdownView";
import ObjectPresenter from "./ObjectPresenter";
import ToolContainerContext from "../ToolContainerContext";
import MoveUpIcon from "@mui/icons-material/ArrowUpward";
import MoveDownIcon from "@mui/icons-material/ArrowDownward";
import InspectorIcon from "../icons/InspectorIcon";
import { darken } from "@mui/system";
import CollapsedIcon from "@mui/icons-material/ArrowRight";
import ExpandedIcon from "@mui/icons-material/ArrowDropDown";

class NotebookCell extends Component {
	static contextType = ToolContainerContext;

	constructor(props) {
		super(props);
		this.state = {
			evaluating: false,
			currentEvaluation: null,
			resultExpanded: true,
		};
		this.editorRef = null;
	}

	shouldComponentUpdate(nextProps, nextState) {
		return (
			nextProps.editable !== this.props.editable ||
			nextProps.cell.source !== this.props.cell.source ||
			nextProps.cell.result !== this.props.cell.result ||
			nextProps.hovered !== this.props.hovered ||
			nextState.evaluating !== this.state.evaluating ||
			nextState.currentEvaluation !== this.state.currentEvaluation ||
			nextState.resultExpanded !== this.state.resultExpanded
		);
	}

	sourceChanged = (source) => {
		const { cell, onChange } = this.props;
		cell.setSource(source);
		if (onChange) onChange(cell);
	};

	evaluateClicked = async (event) => {
		event.stopPropagation();
		const { cell, evaluationContext, onEvaluate } = this.props;
		if (!cell.isCode()) {
			if (onEvaluate) onEvaluate(cell);
			return;
		}
		this.setState({ evaluating: true });
		if (!cell || !cell.source || cell.source.length === 0) return;
		let object, error;
		try {
			const evaluation = {
				expression: cell.source,
				sync: false,
				pin: false,
				context: evaluationContext,
			};
			let issued = await ide.backend.issueEvaluation(evaluation);
			evaluation.id = issued.id;
			evaluation.state = issued.state;
			this.setState({ currentEvaluation: evaluation });
			object = await this.context.waitForEvaluationResult(evaluation);
			const views = await ide.backend.objectViews(object.id, []);
			object.views = views;
			this.setState({ evaluating: false, currentEvaluation: null });
		} catch (exception) {
			object = null;
			error = exception;
			this.setState({
				evaluating: false,
				currentEvaluation: null,
			});
		}
		cell.result = object;
		cell.error = error;
		if (onEvaluate) onEvaluate(cell);
	};

	inspectClicked = (event) => {
		event.stopPropagation();
		const { cell } = this.props;
		if (cell.result) this.context.openInspector(cell.result, true);
	};

	deleteClicked = (event) => {
		event.stopPropagation();
		const { onDelete } = this.props;
		if (onDelete) onDelete();
	};

	keyDownPressed = (event) => {
		if (this.props.editable && event.ctrlKey && event.key === "Enter") {
			event.stopPropagation();
			this.evaluateClicked(event);
		}
	};

	moveUpClicked = (event) => {
		event.stopPropagation();
		const { onMoveUp } = this.props;
		if (onMoveUp) onMoveUp();
	};

	moveDownClicked = (event) => {
		event.stopPropagation();
		const { onMoveDown } = this.props;
		if (onMoveDown) onMoveDown();
	};

	toggleCellType = () => {
		const { cell, onChange } = this.props;
		cell.toggleType();
		this.forceUpdate();
		if (onChange) onChange(cell);
	};

	focusEditor = () => {
		if (this.editorRef && this.editorRef.focusEditor)
			this.editorRef.focusEditor();
	};

	toggleResultExpanded = () => {
		this.setState({
			resultExpanded: !this.state.resultExpanded,
		});
	};

	render() {
		const {
			cell,
			editable,
			evaluationContext,
			enableUp,
			enableDown,
			hovered,
		} = this.props;
		const { resultExpanded, evaluating } = this.state;
		return (
			<Box sx={{ width: "100%" }} onKeyDown={this.keyDownPressed}>
				<Paper
					variant={editable || hovered ? "outlined" : "elevation"}
					elevation={0}
					sx={(theme) => ({
						width: "100%",
						position: "relative",
						display: "flex",
						flexDirection: "column",
						borderLeft: "10px solid",
						borderColor: editable
							? "primary.main"
							: hovered
							? "grey.500"
							: "transparent",
						pl: 1,
						mb: 1,
						pr: 1,
						backgroundColor:
							cell.isCode() && !editable
								? darken(theme.palette.background.paper, 0.05)
								: "inherit",
					})}
				>
					{cell.isText() && !editable && (
						<Box sx={{ ml: 1, width: "95%" }}>
							<MarkdownView source={cell.source} />
						</Box>
					)}
					{editable && (
						<Box
							sx={{
								position: "absolute",
								top: 0,
								right: 5,
								display: "flex",
								gap: 0,
								zIndex: 1,
							}}
						>
							<Tooltip title="Move Up">
								<span>
									<IconButton
										size="small"
										onClick={this.moveUpClicked}
										disabled={!enableUp}
									>
										<MoveUpIcon fontSize="small" />
									</IconButton>
								</span>
							</Tooltip>
							<Tooltip title="Move Down">
								<span>
									<IconButton
										size="small"
										onClick={this.moveDownClicked}
										disabled={!enableDown}
									>
										<MoveDownIcon fontSize="small" />
									</IconButton>
								</span>
							</Tooltip>
							<Tooltip title="Delete cell">
								<IconButton
									size="small"
									onClick={this.deleteClicked}
								>
									<DeleteIcon fontSize="small" />
								</IconButton>
							</Tooltip>
						</Box>
					)}
					{(hovered || editable) && cell.isCode() && (
						<Box
							sx={{
								position: "absolute",
								top: 0,
								left: 2,
								width: 32,
								height: 32,
								display: "flex",
								alignItems: "center",
								justifyContent: "center",
							}}
						>
							<Tooltip title="Evaluate code">
								<IconButton
									size="small"
									onClick={this.evaluateClicked}
									disabled={evaluating}
									sx={{
										width: 32,
										height: 32,
										padding: 0,
										position: "relative", // necesario para que el spinner se posicione encima
									}}
								>
									<EvaluateIcon fontSize="small" />
									{evaluating && (
										<CircularProgress
											size={20}
											sx={{
												position: "absolute",
												zIndex: 1,
											}}
										/>
									)}
								</IconButton>
							</Tooltip>
						</Box>
					)}
					{(cell.isCode() || editable) && (
						<Box sx={{ ml: 3, width: "95%" }}>
							<CodeEditor
								ref={(ref) => (this.editorRef = ref)}
								noScroll
								showLineNumbers={false}
								source={cell.source}
								readOnly={!editable}
								onChange={this.sourceChanged}
								context={evaluationContext}
								changeEventLaps={300}
								enableAutocompletion={false}
							/>
						</Box>
					)}
					{editable && (
						<Box
							sx={{
								display: "flex",
								justifyContent: "flex-end",
								alignItems: "center",
							}}
						>
							<Button
								variant="text"
								size="small"
								onClick={this.toggleCellType}
								sx={{
									textTransform: "none",
									fontSize: "0.75rem",
									minWidth: 0,
									px: 1,
								}}
							>
								{cell.typeLabel()}
							</Button>
						</Box>
					)}
				</Paper>
				{cell.isCode() && cell.result && !evaluating && (
					<Paper
						variant="elevation"
						elevation={0}
						sx={{
							width: "100%",
							display: "flex",
							flexDirection: "column",
							flexGrow: 1,
							minHeight: 30,
							maxHeight: resultExpanded ? 400 : 30,
							height: resultExpanded ? 400 : 30,
							overflow: "hidden",
							borderLeft: "10px solid",
							borderColor: editable
								? "primary.main"
								: "transparent",
							position: "relative",
						}}
					>
						<Box
							sx={{
								position: "absolute",
								top: 0,
								left: 2,
								zIndex: 1,
							}}
						>
							<Tooltip
								title={
									resultExpanded
										? "Hide result"
										: "Show result"
								}
							>
								<IconButton
									size="small"
									onClick={this.toggleResultExpanded}
								>
									{resultExpanded ? (
										<ExpandedIcon fontSize="small" />
									) : (
										<CollapsedIcon fontSize="small" />
									)}
								</IconButton>
							</Tooltip>
						</Box>
						{hovered && (
							<Box
								sx={{
									position: "absolute",
									top: 0,
									right: 5,
									display: "flex",
									gap: 0,
									zIndex: 1,
								}}
							>
								<Tooltip title="Inspect result">
									<IconButton
										size="small"
										onClick={this.inspectClicked}
									>
										<InspectorIcon fontSize="small" />
									</IconButton>
								</Tooltip>
							</Box>
						)}
						<Box
							sx={{ ml: 3, pr: 3, height: "100%", width: "100%" }}
						>
							{resultExpanded && (
								<ObjectPresenter
									context={evaluationContext}
									object={cell.result}
								/>
							)}
						</Box>
					</Paper>
				)}
			</Box>
		);
	}
}

export default NotebookCell;
