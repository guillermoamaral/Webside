import React from "react";
import Tool from "./Tool";
import { ide } from "../IDE";
import {
	Box,
	Paper,
	LinearProgress,
	ToggleButton,
	ToggleButtonGroup,
	Button,
} from "@mui/material";
import CodeEditor from "../parts/CodeEditor";
import CustomSplit from "../controls/CustomSplit";
import FastTree from "../controls/FastTree";
import { BarChart } from "@mui/x-charts/BarChart";

class Profiler extends Tool {
	constructor(props) {
		super(props);
		this.state = {
			expression: props.expression || "",
			tree: null,
			ranking: [],
			selectedMode: "tree",
			selectedMethod: null,
			expandedTreeNodes: [],
			selectedTreeNode: null,
			current: null,
			profiling: false,
		};
	}

	async updateResults(evaluation) {
		if (!evaluation || evaluation.state !== "finished") return;
		try {
			const id = evaluation.id;
			const tree = await ide.backend.profilerTreeResults(id);
			const ranking = await ide.backend.profilerRankingResults(id);
			this.setState({
				tree: tree,
				selectedTreeNode: tree,
				ranking: ranking,
			});
		} catch (error) {
			ide.reportError(error);
		}
	}

	async fetchMethod(signature) {
		if (signature.indexOf(">>") === -1) return;
		let method;
		const parts = signature.split(">>");
		try {
			method = await ide.backend.method(parts[0], parts[1]);
		} catch (ignored) {}
		return method;
	}

	modeChanged = (event, mode) => {
		this.setState({ selectedMode: mode });
	};

	treeNodeSelected = async (node) => {
		const method = await this.fetchMethod(node.method);
		this.setState({ selectedTreeNode: node, selectedMethod: method });
	};

	treeNodeExpanded = async (node) => {
		this.setState({
			expandedTreeNodes: [...this.state.expandedTreeNodes, node],
		});
	};

	treeNodeCollapsed = (node) => {
		const expanded = this.state.expandedTreeNodes;
		expanded.splice(expanded.indexOf(node), 1);
		this.setState({ expandedTreeNodes: expanded });
	};

	async rankingSelected(index) {
		const ranking = this.state.ranking;
		if (index && index >= 0 && index < ranking.length) {
			const method = await this.fetchMethod(ranking[index].method);
			this.setState({ selectedMethod: method });
		}
	}

	expressionChanged = (expression) => {
		this.setState({ expression: expression });
	};

	profileClicked = async () => {
		try {
			const evaluation = {
				expression: this.state.expression,
				profile: true,
				context: this.props.context,
			};
			let issued = await ide.backend.issueEvaluation(evaluation);
			Object.assign(evaluation, issued);
			this.setState({
				profiling: true,
				current: evaluation,
				tree: null,
				ranking: null,
			});
			const final = await this.context.waitForEvaluationFinalization(
				evaluation
			);
			await this.updateResults(final);
			this.setState({ profiling: false, current: null });
		} catch (error) {
			this.context.reportError(error);
			this.setState({ profiling: false, current: null });
		}
	};

	stopClicked = async () => {
		try {
			await ide.backend.deleteProfiler(this.state.current.id);
		} catch (error) {
			this.context.reportError(error);
		}
		this.setState({ profiling: false, current: null });
	};

	render() {
		const {
			expression,
			profiling,
			tree,
			ranking,
			selectedMode,
			selectedTreeNode,
			selectedMethod,
		} = this.state;
		const evaluationContext = this.props.context;
		const appearance = ide.settings.section("appearance");
		const mode = appearance.section(appearance.get("mode"));
		const colors = mode.section("colors");
		return (
			<Box display="flex" flexDirection="column" sx={{ height: "100%" }}>
				<CustomSplit mode="vertical">
					<Box
						display="flex"
						flexDirection="column"
						sx={{ height: "20%" }}
					>
						<Box flexGrow={1}>
							<Paper
								variant="outlined"
								style={{ height: "100%" }}
							>
								<CodeEditor
									context={evaluationContext}
									source={expression}
									showAccept={false}
									showPlay={false}
									onChange={this.expressionChanged}
									readOnly={profiling}
								/>
							</Paper>
						</Box>
						<Box mt={1} display="flex" flexDirection="row">
							<Box flexGrow={1}>
								{profiling && (
									<LinearProgress variant="indeterminate" />
								)}
							</Box>
							<Box ml={1}>
								<Button
									variant="outlined"
									color={profiling ? "secondary" : "primary"}
									onClick={() =>
										profiling
											? this.stopClicked()
											: this.profileClicked()
									}
								>
									{profiling ? "Stop" : "Profile"}
								</Button>
							</Box>
						</Box>
					</Box>
					<Box
						display="flex"
						flexDirection="column"
						sx={{ height: "80%" }}
					>
						<Box>
							<ToggleButtonGroup
								label="primary"
								value={selectedMode}
								exclusive
								onChange={this.modeChanged}
							>
								<ToggleButton
									value="tree"
									variant="outlined"
									size="small"
								>
									Tally
								</ToggleButton>
								<ToggleButton
									value="ranking"
									variant="outlined"
									size="small"
								>
									Ranking
								</ToggleButton>
							</ToggleButtonGroup>
						</Box>
						<Box flexGrow={1}>
							<CustomSplit mode="vertical">
								<Box sx={{ height: "80%" }}>
									{!profiling && selectedMode === "tree" && (
										<FastTree
											nodes={tree ? [tree] : []}
											nodeLabel="name"
											nodeChildren="children"
											onNodeSelect={this.treeNodeSelected}
											onNodeExpand={this.treeNodeExpanded}
											selectedNode={selectedTreeNode}
										/>
									)}
									{!profiling &&
										selectedMode === "ranking" && (
											<BarChart
												slotProps={{
													legend: { hidden: true },
												}}
												margin={{
													top: 10,
													left: 500,
													//bottom: 10,
													right: 10,
												}}
												onItemClick={(event, item) => {
													this.rankingSelected(
														item.dataIndex
													);
												}}
												dataset={ranking}
												yAxis={[
													{
														scaleType: "band",
														dataKey: "method",
														labelStyle: {
															fontSize: 6,
														},
													},
												]}
												series={[
													{
														dataKey: "value",
														color: colors.get(
															"primaryColor"
														),
														label: "Time",
													},
												]}
												layout="horizontal"
												{...{
													xAxis: [
														{
															label: "time (ms)",
														},
													],
												}}
											/>
										)}
								</Box>
								<Box sx={{ height: "20%" }}>
									{!profiling && (
										<Paper
											variant="outlined"
											style={{ height: "100%" }}
										>
											<CodeEditor
												source={
													!selectedMethod
														? ""
														: selectedMethod.source
												}
												showAccept={false}
												readOnly
											/>
										</Paper>
									)}
								</Box>
							</CustomSplit>
						</Box>
					</Box>
				</CustomSplit>
			</Box>
		);
	}
}

export default Profiler;
