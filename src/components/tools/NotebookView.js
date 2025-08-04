import { Component } from "react";
import { Box, IconButton, Tooltip, Divider, Button } from "@mui/material";
import AddIcon from "@mui/icons-material/Add";
import NotebookCellEditor from "../parts/NotebookCellEditor";
import DraggableReorderItem from "../controls/DraggableReorderItem";
import EvaluateIcon from "@mui/icons-material/PlayCircle";
import Notebook from "../../model/Notebook";

class NotebookView extends Component {
	constructor(props) {
		super(props);
		this.notebook = props.notebook || new Notebook();
		this.state = {
			cells: [...this.notebook.cells],
			activeCellIndex: null,
			hoveredIndex: null,
			hoveredInsertIndex: null,
			dragging: false,
		};
		this.cellRefs = [];
	}

	componentDidUpdate(prevProps) {
		const { notebook } = this.props;
		if (notebook && prevProps.notebook?.cells !== notebook.cells) {
			this.setState({ cells: [...notebook.cells] });
		}
	}

	editCell = (event, index) => {
		event.stopPropagation();
		this.setState({ activeCellIndex: index });
	};

	notebookChanged = () => {
		const { onChange } = this.props;
		if (onChange) onChange(this.notebook);
	};

	addCellClicked = (event, index, type, source) => {
		event.preventDefault();
		event.stopPropagation();
		const place = this.notebook.addCell(type, source, index);
		this.setState(
			{ cells: this.notebook.cells, activeCellIndex: place },
			() => {
				this.notebookChanged();
				setTimeout(() => {
					const ref = this.cellRefs[place];
					if (ref && ref.focusEditor) ref.focusEditor();
				}, 100);
			}
		);
	};

	evaluateActiveCell = () => {
		const { activeCellIndex } = this.state;
		const ref = this.cellRefs[activeCellIndex];
		if (ref && ref.evaluateClicked) {
			ref.evaluateClicked(new Event("evaluate"));
		}
	};

	deleteActiveCell = () => {
		const { activeCellIndex } = this.state;
		if (activeCellIndex != null) this.deleteCell(activeCellIndex);
	};

	deleteCell = (index) => {
		this.notebook.deleteCell(index);
		this.setState({ cells: this.notebook.cells }, this.notebookChanged);
	};

	cellEvaluated = (cell) => {
		this.setState({ activeCellIndex: null });
	};

	moveCell = (from, to) => {
		this.notebook.moveCell(from, to);
		this.setState(
			{ cells: this.notebook.cells, activeCellIndex: to },
			this.notebookChanged
		);
	};

	moveUpCell = (index) => {
		if (index > 0) this.moveCell(index, index - 1);
	};

	moveDownCell = (index) => {
		if (index < this.state.cells.length - 1)
			this.moveCell(index, index + 1);
	};

	evaluateAllCells = () => {
		this.state.cells.forEach((cell, index) => {
			if (cell.isCode()) {
				const ref = this.cellRefs[index];
				if (ref && ref.evaluateClicked) {
					ref.evaluateClicked(new Event("evaluate"));
				}
			}
		});
	};

	render() {
		const { evaluationContext } = this.props;
		const { cells, activeCellIndex, hoveredIndex, hoveredInsertIndex } =
			this.state;
		return (
			<Box
				display="flex"
				flexDirection="column"
				height="100%"
				width="100%"
				onClick={() => this.setState({ activeCellIndex: null })}
			>
				<Box
					display="flex"
					alignItems="center"
					justifyContent="space-between"
					sx={{ px: 2, py: 0, maxHeight: 40 }}
				>
					<Box
						display="flex"
						alignItems="center"
						justifyContent="space-between"
						sx={{ px: 2, py: 1 }}
					>
						<Box display="flex" gap={1}>
							<Tooltip title="Add code cell at the bottom">
								<Button
									size="small"
									startIcon={<AddIcon />}
									onClick={(event) =>
										this.addCellClicked(event, null, "code")
									}
								>
									Code
								</Button>
							</Tooltip>
							<Tooltip title="Add text cell at the bottom">
								<Button
									size="small"
									startIcon={<AddIcon />}
									onClick={(event) =>
										this.addCellClicked(event, null, "text")
									}
								>
									Text
								</Button>
							</Tooltip>
						</Box>
						<Box display="flex" gap={1}>
							<Tooltip title="Evaluate all cells">
								<IconButton onClick={this.evaluateAllCells}>
									<EvaluateIcon fontSize="small" />
								</IconButton>
							</Tooltip>
						</Box>
					</Box>
				</Box>
				<Divider />
				<Box mt={1}>
					{[...Array(cells.length + 1)].map((_, index) => {
						return (
							<Box key={`insert-${index}`}>
								<Box
									sx={{
										width: "100%",
										height: 20,
										position: "relative",
									}}
									onMouseEnter={() =>
										this.setState({
											hoveredInsertIndex: index,
										})
									}
									onMouseLeave={() =>
										this.setState({
											hoveredInsertIndex: null,
										})
									}
								>
									{hoveredInsertIndex === index && (
										<>
											<Divider
												sx={{
													position: "absolute",
													top: "50%",
													width: "100%",
												}}
											/>
											{
												<Box
													sx={{
														position: "absolute",
														top: "50%",
														left: "50%",
														transform:
															"translate(-50%, -50%)",
														display: "flex",
														gap: 1,
														backgroundColor:
															"background.paper",
														px: 1,
													}}
												>
													<Tooltip title="Inser code cell here">
														<Button
															variant="text"
															size="small"
															startIcon={
																<AddIcon fontSize="small" />
															}
															onClick={(event) =>
																this.addCellClicked(
																	event,
																	index,
																	"code"
																)
															}
														>
															Code
														</Button>
													</Tooltip>
													<Tooltip title="Inser text cell here">
														<Button
															variant="text"
															size="small"
															startIcon={
																<AddIcon fontSize="small" />
															}
															onClick={(event) =>
																this.addCellClicked(
																	event,
																	index,
																	"text"
																)
															}
														>
															Text
														</Button>
													</Tooltip>
												</Box>
											}
										</>
									)}
								</Box>
								{index < cells.length && (
									<Box
										display="flex"
										flexDirection="row"
										alignItems="center"
										onClick={(event) =>
											this.editCell(event, index)
										}
										onMouseEnter={() =>
											this.setState({
												hoveredIndex: index,
											})
										}
										onMouseLeave={() =>
											this.setState({
												hoveredIndex: null,
											})
										}
										width="100%"
										key={"cell-" + index}
										mb={1}
										px={4}
									>
										<DraggableReorderItem
											index={index}
											type="NOTEBOOK_CELL"
											onMove={this.moveCell}
											direction="vertical"
											dragHandle={true}
											sx={{
												width: "100%",
												display: "flex",
											}}
											onDraggingChange={(dragging) =>
												this.setState({ dragging })
											}
										>
											<NotebookCellEditor
												ref={(ref) =>
													(this.cellRefs[index] = ref)
												}
												editable={
													activeCellIndex === index
												}
												cell={cells[index]}
												onChange={this.notebookChanged}
												onEvaluate={this.cellEvaluated}
												onDelete={() =>
													this.deleteCell(index)
												}
												evaluationContext={
													evaluationContext
												}
												onMoveUp={() =>
													this.moveUpCell(index)
												}
												onMoveDown={() =>
													this.moveDownCell(index)
												}
												enableUp={index > 0}
												enableDown={
													index < cells.length - 1
												}
												hovered={hoveredIndex === index}
											/>
										</DraggableReorderItem>
									</Box>
								)}
							</Box>
						);
					})}
				</Box>
			</Box>
		);
	}
}

export default NotebookView;
