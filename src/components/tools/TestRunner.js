import React from "react";
import Tool from "./Tool";
import { ide } from "../IDE";
import LinearProgress from "@mui/material/LinearProgress";
import {
	Accordion,
	AccordionSummary,
	Typography,
	Grid,
	Button,
	Card,
	CardContent,
	CardActionArea,
	FormGroup,
	FormControlLabel,
	Checkbox,
	Box,
	Chip,
} from "@mui/material";
import ExpandMoreIcon from "@mui/icons-material/ExpandMore";
import CustomTable from "../controls/CustomTable";
import {
	Chart as ChartJS,
	CategoryScale,
	LinearScale,
	BarElement,
	Tooltip,
} from "chart.js";
import { Bar } from "react-chartjs-2";
import ChartDataLabels from "chartjs-plugin-datalabels";
import DebuggerIcon from "../icons/DebuggerIcon";

ChartJS.register(
	CategoryScale,
	LinearScale,
	BarElement,
	Tooltip,
	ChartDataLabels
);

class TestRunner extends Tool {
	constructor(props) {
		super(props);
		this.state = {
			status: this.newStatus(),
			results: this.newResults(),
			updating: false,
			filterType: "run",
			showGroups: false,
			selectedTest: null,
		};
	}

	aboutToSelect() {
		this.updateStatus(true);
	}

	async aboutToClose() {
		try {
			await ide.backend.deleteTestRun(this.props.id);
		} catch (error) {
			ide.reportError(error);
		}
	}

	componentDidMount() {
		this.updateStatus();
	}

	newResults() {
		return {
			updated: false,
			run: [],
			passed: [],
			failed: [],
			errors: [],
			skipped: [],
			knownIssues: [],
			grouped: {},
		};
	}

	newStatus() {
		return {
			name: "",
			total: 0,
			running: false,
			current: null,
			summary: this.newSummary(),
		};
	}

	newSummary() {
		return {
			run: 0,
			passed: 0,
			failed: 0,
			errors: 0,
			skipped: 0,
			knownIssues: 0,
		};
	}

	typeColor(type) {
		let color;
		switch (type) {
			case "run":
				color = "#4f4f4f";
				break;
			case "passed":
				color = "#28a745";
				break;
			case "failed":
				color = "#ef6011";
				break;
			case "errors":
				color = "#dc3545";
				break;
			case "skipped":
				color = "#6c757d";
				break;
			case "knownIssues":
				color = "#007bff";
				break;
			default:
				color = "default";
		}
		return color;
	}

	async runClicked() {
		const status = this.newStatus();
		status.running = true;
		const results = this.newResults();
		this.setState({ status: status, results: results });
		try {
			await ide.backend.runTestRun(this.props.id);
			this.updateStatus();
		} catch (error) {
			ide.reportError(error);
		}
	}

	async stopClicked() {
		try {
			await ide.backend.stopTestRun(this.props.id);
			this.updateStatus();
		} catch (error) {
			ide.reportError(error);
		}
	}

	async updateStatus(force) {
		if (this.state.updating) {
			return;
		}
		this.setState({ updating: true });
		try {
			const status = await ide.backend.testRunStatus(this.props.id);
			if (status.running) {
				setTimeout(() => {
					this.updateStatus();
				}, 1000);
			}
			this.setState({ status: status, updating: false });
			if (!status.running) {
				this.updateResults(force);
			}
		} catch (error) {
			ide.reportError(error);
		}
	}

	async updateResults(force = false) {
		if (!force && this.state.results.updated) {
			return;
		}
		try {
			const results = await ide.backend.testRunResults(this.props.id);
			results.grouped = this.groupResults(results);
			results.updated = true;
			this.setState({ results: results });
		} catch (error) {
			ide.reportError(error);
		}
	}

	groupResults(results) {
		const grouped = {};
		results.run = [];
		["passed", "failed", "errors", "skipped", "knownIssues"].forEach(
			(type) => {
				const tests = results[type] || [];
				const color = this.typeColor(type);
				tests.forEach((test) => {
					results.run.push(test);
					test.type = type;
					test.color = color;
					const group = test.class;
					if (!grouped[group]) {
						grouped[group] = {
							summary: this.newSummary(),
							run: [],
						};
					}
					if (!grouped[group][type]) {
						grouped[group][type] = [];
					}
					grouped[group][type].push(test);
					grouped[group].run.push(test);
					grouped[group].summary[type] += 1;
					grouped[group].summary.run += 1;
				});
			}
		);
		return grouped;
	}

	filterTests(type) {
		this.setState({ filterType: type });
	}

	browseImplementors = (test) => {
		if (test) {
			this.context.browseImplementors(test.selector);
		}
	};

	browseClass = (test) => {
		if (test) {
			this.context.browseClass(test.class);
		}
	};

	menuOptions() {
		return [
			{
				label: "Debug",
				action: this.debugTest,
				enabled: this.canDebugTest,
			},
			{ label: "Browse implementors", action: this.browseImplementors },
			{ label: "Browse class", action: this.browseClass },
		];
	}

	debugTest = async (test) => {
		try {
			const d = await ide.backend.debugTest(
				this.props.id,
				test.class,
				test.selector
			);
			this.context.openDebugger(
				d.id,
				d.description || "Debugging test " + test.selector
			);
		} catch (error) {
			ide.reportError(error);
		}
	};

	canDebugTest = (test) => {
		return test && (test.type === "failed" || test.type === "errors");
	};

	columns() {
		return [
			{
				field: "selector",
				label: "Selector",
				minWidth: 100,
				align: "left",
			},
			{
				field: "class",
				label: "Clas",
				minWidth: 100,
				align: "left",
			},
			{
				field: "time",
				label: "Time (ms)",
				minWidth: 100,
				align: "right",
			},
		];
	}

	rowActions() {
		return [
			{
				label: "Debug",
				icon: <DebuggerIcon fontSize="small" />,
				handler: this.debugTest,
				visible: this.canDebugTest,
			},
		];
	}

	sortedGroups() {
		const grouped = this.state.results.grouped;
		const sorted = Object.keys(grouped).sort((g1, g2) => {
			const n1 =
				grouped[g1].summary.failed ||
				0 + grouped[g1].summary.error ||
				0;
			const n2 =
				grouped[g2].summary.failed ||
				0 + grouped[g2].summary.error ||
				0;
			return n1 > n2 ? -1 : n1 < n2 ? 1 : 0;
		});
		return sorted;
	}

	showGroups(show) {
		this.setState({ showGroups: show });
	}

	barChartData(label, summary) {
		return {
			labels: [label],
			datasets: Object.keys(summary)
				.filter((type) => {
					return type !== "run" && summary[type] > 0;
				})
				.map((type) => {
					return {
						label: type,
						backgroundColor: this.typeColor(type),
						borderWidth: 0,
						data: [summary[type] || 0],
					};
				}),
		};
	}

	barChartOptions() {
		return {
			indexAxis: "y",
			scales: {
				x: {
					stacked: true,
					display: false,
				},
				y: {
					stacked: true,
					display: true,
				},
			},
			plugins: {
				legend: {
					display: false,
				},
				title: {
					display: false,
				},
				datalabels: {
					display: true,
					color: "white",
				},
			},
		};
	}

	testSelected = (test) => {
		this.setState({ selectedTest: test });
	};

	render() {
		console.log("rendering test runer");
		const { status, results, filterType, showGroups, selectedTest } =
			this.state;
		const { total, running, current } = status;
		const summary = status.summary || this.newSummary();
		const percent = total > 0 ? (summary.run / total) * 100 : 0;
		const grouped = results.grouped;
		return (
			<Box display="flex" flexDirection="column" sx={{ height: "100%" }}>
				<Box>
					<Typography variant="h6" color="primary">
						Suite: {status.name}
					</Typography>
				</Box>
				<Box display="flex" flexDirection="row" alignItems="center">
					<Box flexGrow={1}>
						{running && (
							<LinearProgress
								variant="determinate"
								value={percent}
							/>
						)}
					</Box>
					<Box ml={1}>
						{running && (
							<Typography
								variant="subtitle1"
								color="textSecondary"
							>
								{summary.run} / {total}
							</Typography>
						)}
					</Box>
					<Box ml={1}>
						<Button
							variant="outlined"
							color={running ? "secondary" : "primary"}
							onClick={() =>
								running ? this.stopClicked() : this.runClicked()
							}
						>
							{running ? "Stop" : "Run"}
						</Button>
					</Box>
				</Box>
				<Box>
					{running && (
						<Typography variant="subtitle1" color="textSecondary">
							Running:{" "}
							{current
								? current.class + ": " + current.selector
								: ""}
						</Typography>
					)}
				</Box>
				<Box mb={1}>
					<Grid
						container
						spacing={1}
						justifyContent="space-around"
						alignItems="flex-end"
					>
						{Object.keys(summary).map((type) => {
							return (
								<Grid
									item
									xs={2}
									md={2}
									lg={2}
									key={"grid-" + type}
								>
									<Card
										id={"card-" + type}
										sx={{
											background: this.typeColor(type),
										}}
									>
										<CardActionArea
											onClick={(event) =>
												this.filterTests(type)
											}
										>
											<CardContent
												key={"card-content-" + type}
											>
												<Typography
													variant={
														type === "run"
															? "h2"
															: "h4"
													}
													sx={{
														color: "white",
													}}
												>
													{summary[type] || 0}
												</Typography>
												<Typography
													variant="button"
													sx={{ color: "white" }}
													align="right"
												>
													{type}
												</Typography>
											</CardContent>
										</CardActionArea>
									</Card>
								</Grid>
							);
						})}
					</Grid>
				</Box>
				<Box>
					<Box
						display="flex"
						flexDirection="row"
						justifyContent="space-between"
						alignContent="center"
					>
						<Box
							display="flex"
							flexDirection="row"
							justifyContent="center"
							alignContent="center"
						>
							<Chip
								label={"Showing: " + filterType}
								sx={{
									color: "white",
									background: this.typeColor(filterType),
								}}
							/>
						</Box>
						<FormGroup>
							<FormControlLabel
								control={
									<Checkbox
										size="small"
										checked={showGroups}
										color="primary"
										onChange={(event) =>
											this.showGroups(
												event.target.checked
											)
										}
									/>
								}
								label="Group tests"
							/>
						</FormGroup>
					</Box>
				</Box>
				<Box>
					{showGroups &&
						this.sortedGroups()
							.filter(
								(group) =>
									grouped[group].summary[filterType] > 0
							)
							.map((group) => {
								const groupTests = grouped[group][filterType];
								const showBars = filterType === "run";
								return (
									<Accordion key={group}>
										<AccordionSummary
											sx={{ maxHeight: 15 }}
											expandIcon={<ExpandMoreIcon />}
											id={group}
										>
											<Box
												display="flex"
												flexDirection="row"
												alignContent="center"
											>
												{!showBars && group}
												<Box>
													{showBars && (
														<Bar
															height={60}
															//width={"100%"}
															data={this.barChartData(
																group,
																grouped[group]
																	.summary
															)}
															options={this.barChartOptions()}
														/>
													)}
												</Box>
											</Box>
										</AccordionSummary>
										<Grid container>
											<Grid item xs={1} md={1} lg={1} />
											<Grid
												item
												xs={11}
												md={11}
												lg={11}
												sx={{ minHeight: 300 }}
											>
												<CustomTable
													columns={this.columns()}
													rows={groupTests}
													menuOptions={this.menuOptions()}
													rowActions={this.rowActions()}
													selectedRow={selectedTest}
													onRowSelect={
														this.testSelected
													}
												></CustomTable>
											</Grid>
										</Grid>
									</Accordion>
								);
							})}
				</Box>
				<Box flexGrow={1}>
					{!showGroups && (
						<CustomTable
							columns={this.columns()}
							rows={results[filterType]}
							menuOptions={this.menuOptions()}
							rowActions={this.rowActions()}
							rowsPerPage={50}
							usePagination
							selectedRow={selectedTest}
							onRowSelect={this.testSelected}
						></CustomTable>
					)}
				</Box>
			</Box>
		);
	}
}

export default TestRunner;
