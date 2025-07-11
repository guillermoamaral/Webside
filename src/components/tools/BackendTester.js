import React from "react";
import Tool from "./Tool";
import {
	Box,
	Button,
	IconButton,
	LinearProgress,
	Typography,
} from "@mui/material";
import { BackendTestSuite } from "../BackendTest";
import CustomTable from "../controls/CustomTable";
import CustomSplit from "../controls/CustomSplit";
import JSONView from "../parts/JSONView";
import { BarChart } from "@mui/x-charts/BarChart";
import CloseIcon from "@mui/icons-material/Close";
import PlayIcon from "@mui/icons-material/PlayArrow";

class BackendTester extends Tool {
	constructor(props) {
		super(props);
		this.suite = new BackendTestSuite(this.props.url);
		this.state = {
			tests: this.suite.tests,
			selectedTest: null,
			showTestLog: true,
			running: false,
			progress: 0,
		};
	}

	columns() {
		return [
			{
				field: (t) => t.section(),
				label: "Section",
				align: "left",
			},
			{
				field: (t) => t.description(),
				label: "Test",
				align: "left",
			},
			{
				field: (t) => t.state(),
				label: "State",
				align: "left",
			},
			{
				field: (t) => t.resultDescription(),
				label: "Description",
				align: "left",
			},
		];
	}

	runClicked = async () => {
		this.setState({ running: true });
		this.suite.run();
		this.updateProgress();
	};

	stopClicked = () => {
		this.suite.stop();
		this.setState({
			running: false,
			selectedTest: null,
		});
	};

	runTest = async (test) => {
		this.setState({ running: true });
		await this.suite.runTest(test);
		this.setState({ running: false });
	};

	updateProgress() {
		if (this.suite.state === "running") {
			this.setState(
				{
					running: true,
					progress: (this.suite.ran / this.suite.count) * 100,
				},
				() => {
					setTimeout(() => this.updateProgress(), 1000);
				}
			);
		} else {
			this.setState({
				running: false,
				progress: 0,
			});
		}
	}

	testSelected = (test) => {
		this.setState({
			selectedTest: test,
			showTestLog: true,
		});
	};

	stateColor(state) {
		let color;
		switch (state) {
			case "Passed":
				color = "#28a745";
				break;
			case "Failed":
				color = "#ef6011";
				break;
			case "Error":
				color = "#dc3545";
				break;
			default:
				color = "default";
		}
		return color;
	}

	closeTestLog = () => {
		this.setState({ showTestLog: false });
	};

	rowActions() {
		return [
			{
				label: "Run",
				icon: <PlayIcon fontSize="small" />,
				handler: this.runTest,
				visible: !this.state.running,
			},
		];
	}

	menuOptions() {
		return [{ label: "Run", action: this.runTest }];
	}

	render() {
		const { tests, selectedTest, running, progress, showTestLog } =
			this.state;
		const showLog = selectedTest && showTestLog;
		return (
			<Box
				display="flex"
				flexDirection="column"
				style={{ width: "100%", height: "100%" }}
			>
				<Box display="flex" flexDirection="row" alignItems="center">
					<Box>
						<Button
							variant="outlined"
							color={running ? "secondary" : "primary"}
							onClick={() =>
								running ? this.stopClicked() : this.runClicked()
							}
						>
							{running ? "Stop" : "Test"}
						</Button>
					</Box>
					<Box ml={1} flexGrow={1}>
						{running && (
							<LinearProgress
								variant="determinate"
								value={Math.round(progress)}
							/>
						)}
					</Box>
					<Box ml={1}>
						{running && (
							<Typography
								variant="subtitle1"
								color="textSecondary"
							>
								{this.suite.ran} / {this.suite.count}
							</Typography>
						)}
					</Box>
				</Box>
				{tests.length > 0 && (
					<Box mt={1}>
						<BarChart
							margin={{
								top: 0,
								left: 0,
								bottom: 0,
								right: 0,
							}}
							slotProps={{
								legend: {
									hidden: true,
								},
							}}
							layout="horizontal"
							series={[
								{
									stack: "Results",
									label: "Passed",
									color: this.stateColor("Passed"),
									data: [
										tests.filter(
											(r) => r.state() === "Passed"
										).length,
									],
								},
								{
									stack: "Results",
									label: "Failed",
									color: this.stateColor("Failed"),
									data: [
										tests.filter(
											(r) => r.state() === "Failed"
										).length,
									],
								},
								{
									stack: "Results",
									label: "Error",
									color: this.stateColor("Error"),
									data: [
										tests.filter(
											(r) => r.state() === "Error"
										).length,
									],
								},
							]}
							barLabel="value"
							yAxis={[
								{
									scaleType: "band",
									data: ["Results"],
								},
							]}
							leftAxis={null}
							bottomAxis={null}
							height={30}
						/>
					</Box>
				)}
				<CustomSplit>
					<Box flex={1}>
						<CustomTable
							columns={this.columns()}
							rows={tests}
							onRowSelect={this.testSelected}
							selectedRow={selectedTest}
							rowColor={(r) => this.stateColor(r.state())}
							rowActions={this.rowActions()}
							menuOptions={this.menuOptions()}
						/>
					</Box>
					{showLog && (
						<Box width="40%" height="100%">
							<Box display="flex" flexDirection="row">
								<Box flexGrow={1}>
									<Typography variant="h6">Log</Typography>
								</Box>
								<IconButton
									onClick={this.closeTestLog}
									size="small"
								>
									<CloseIcon fontSize="small" />
								</IconButton>
							</Box>
							<Box flexGrow={1} ml={2} mt={1}>
								<JSONView
									source={selectedTest.log}
									sx={{ height: "100%" }}
								/>
							</Box>
						</Box>
					)}
				</CustomSplit>
			</Box>
		);
	}
}

export default BackendTester;
