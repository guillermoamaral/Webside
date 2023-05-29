import React, { Component, Fragment } from "react";
import { ide } from "../IDE";
import {
	Grid,
	Paper,
	LinearProgress,
	ToggleButton,
	ToggleButtonGroup,
} from "@mui/material";
import CodeEditor from "../parts/CodeEditor";
import AutoSizer from "react-virtualized-auto-sizer";
//import { FlameGraph } from "react-flame-graph";
import { Bar } from "react-chartjs-2";

class Profiler extends Component {
	constructor(props) {
		super(props);
		this.state = {
			loading: true,
			tree: null,
			ranking: null,
			selectedMode: "tree",
			selectedMethod: null,
		};
	}

	componentDidMount() {
		this.updateResults();
	}

	async updateResults() {
		try {
			const tree = await ide.api.profilerTreeResults(this.props.id);
			const ranking = await ide.api.profilerRankingResults(this.props.id);
			this.setState({ tree: tree, ranking: ranking, loading: false });
		} catch (error) {
			ide.reportError(error);
		}
	}

	async methodSelected(signature) {
		var method;
		if (signature.indexOf(">>") === -1) {
			method = null;
		} else {
			const parts = signature.split(") ")[1].split(">>");
			try {
				method = await ide.api.method(parts[0], parts[1]);
			} catch (error) {
				ide.reportError(error);
			}
		}
		this.setState({ selectedMethod: method });
	}

	modeChanged = (event, mode) => {
		this.setState({ selectedMode: mode });
	};

	rankingClicked(element) {
		if (element) {
			this.methodSelected(element._model.label);
		}
	}

	render() {
		const { loading, tree, ranking, selectedMode, selectedMethod } =
			this.state;
		var rankingData = {
			labels: ranking ? ranking.map((m) => m.name) : [],
			datasets: [
				{
					backgroundColor: "rgba(254, 188, 56, 0.8)",
					borderColor: "rgba(255, 255, 255, 0.8)",
					borderWidth: 1,
					hoverBackgroundColor: "rgba(255, 143, 0, 1)",
					hoverBorderColor: "white",
					data: ranking ? ranking.map((m) => m.value) : [],
				},
			],
		};
		return (
			<Grid container spacing={1}>
				<Grid item xs={12} md={12} lg={12}>
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
							Execution Tree
						</ToggleButton>
						<ToggleButton
							value="ranking"
							variant="outlined"
							size="small"
						>
							Ranking
						</ToggleButton>
					</ToggleButtonGroup>
				</Grid>
				{loading && (
					<Grid item xs={12} md={12} lg={12}>
						<LinearProgress variant="indeterminate" />
					</Grid>
				)}
				<Grid item xs={12} md={12} lg={12}>
					{!loading && selectedMode === "tree" && (
						<div style={{ height: 300 }}>
							{/* <AutoSizer>
								{({ height: autoSizerHeight, width }) => (
									<Fragment>
										<FlameGraph
											data={tree}
											height={autoSizerHeight}
											width={width}
											onChange={(node) => this.methodSelected(node.name)}
										/>
									</Fragment>
								)}
							</AutoSizer> */}
						</div>
					)}
				</Grid>
				<Grid item xs={12} md={12} lg={12}>
					{!loading && selectedMode === "ranking" && (
						<Bar
							height={80}
							onClick={(elems) => {
								this.rankingClicked(elems[0]);
							}}
							data={rankingData}
							options={{
								indexAxis: "y",
								legend: { display: false },
							}}
						/>
					)}
				</Grid>
				<Grid item xs={12} md={12} lg={12}>
					{!loading && (
						<Paper variant="outlined">
							<CodeEditor
								styles={this.props.styles}
								lineNumbers={true}
								source={
									!selectedMethod ? "" : selectedMethod.source
								}
								showAccept={false}
							/>
						</Paper>
					)}
				</Grid>
			</Grid>
		);
	}
}

export default Profiler;
