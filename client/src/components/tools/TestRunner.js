import React, { Component } from "react";
import { IDEContext } from "../IDEContext";
import LinearProgress from "@material-ui/core/LinearProgress";
import {
	Accordion,
	AccordionSummary,
	Typography,
	Grid,
	Button,
	Card,
	CardContent,
	CardActionArea,
} from "@material-ui/core";
import ExpandMoreIcon from "@material-ui/icons/ExpandMore";
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

class TestRunner extends Component {
	static contextType = IDEContext;

	constructor(props) {
		super(props);
		this.state = {
			status: this.newStatus(),
			results: this.newResults(),
			updating: false,
			filterType: null,
		};
	}

	componentDidMount() {
		this.updateStatus();
	}

	newResults() {
		return {
			updated: false,
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
			await this.context.api.runTestRun(this.props.id);
			this.updateStatus();
		} catch (error) {
			this.context.reportError(error);
		}
	}

	async stopClicked() {
		try {
			await this.context.api.stopTestRun(this.props.id);
			this.updateStatus();
		} catch (error) {
			this.context.reportError(error);
		}
	}

	async updateStatus() {
		if (this.state.updating) {
			return;
		}
		this.setState({ updating: true });
		try {
			const status = await this.context.api.getTestRunStatus(this.props.id);
			if (status.running) {
				setTimeout(() => {
					this.updateStatus();
				}, 1000);
			}
			this.setState({ status: status, updating: false });
			if (!status.running) {
				this.updateResults();
			}
		} catch (error) {
			this.context.reportError(error);
		}
	}

	async updateResults() {
		if (this.state.results.updated) {
			return;
		}
		try {
			const results = await this.context.api.getTestRunResults(this.props.id);
			results.grouped = this.groupByClass(results);
			results.updated = true;
			this.setState({ results: results });
		} catch (error) {
			this.context.reportError(error);
		}
	}

	groupByClass(results) {
		const grouped = {};
		["passed", "failed", "errors", "skipped", "knownIssues"].forEach((k) => {
			const tests = results[k] || [];
			tests.forEach((t) => {
				const c = t.class;
				t.type = k;
				if (!grouped[c]) {
					grouped[c] = { summary: this.newSummary(), all: [] };
				}
				if (!grouped[c][k]) {
					grouped[c][k] = [];
				}
				grouped[c][k].push(t);
				grouped[c].all.push(t);
				grouped[c].summary[k] += 1;
				grouped[c].summary.run += 1;
			});
		});
		return grouped;
	}

	filterTests(type) {
		const filter = type === "run" ? null : type;
		this.setState({ filterType: filter });
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
			{ label: "Debug", action: this.debugTest, enabled: this.canDebugTest },
			{ label: "Implementors", action: this.browseImplementors },
			{ label: "Browse class", action: this.browseClass },
		];
	}

	debugTest = async (test) => {
		try {
			const id = await this.context.api.debugTest(
				this.props.id,
				test.class,
				test.selector
			);
			this.context.openDebugger(id, "Debugging expression");
		} catch (error) {
			this.context.reportError(error);
		}
	};

	canDebugTest = (test) => {
		return test && (test.type === "failed" || test.type === "errors");
	};

	testColumns() {
		return [
			{ field: "selector", label: "Selector", minWidth: 100, align: "left" },
			{ field: "time", label: "Time (ms)", minWidth: 100, align: "right" },
		];
	}

	testActions() {
		return [
			{
				label: "Debug",
				icon: <DebuggerIcon fontSize="small" />,
				handler: this.debugTest,
				visible: this.canDebugTest,
			},
		];
	}

	render() {
		const styles = this.props.styles;
		const { status, results, filterType } = this.state;
		const { total, running, current } = status;
		const summary = status.summary || this.newSummary();
		const percent = total > 0 ? (summary.run / total) * 100 : 0;
		const grouped = results.grouped;
		const ranking = Object.keys(grouped).sort((c1, c2) => {
			const n1 =
				grouped[c1].summary.failed || 0 + grouped[c1].summary.error || 0;
			const n2 =
				grouped[c2].summary.failed || 0 + grouped[c2].summary.error || 0;
			return n1 > n2 ? -1 : n1 < n2 ? 1 : 0;
		});
		return (
			<div>
				<Grid
					container
					spacing={1}
					display="flex"
					alignItems="center"
					justify="center"
				>
					<Grid item xs={12} md={12} lg={12}>
						<Typography variant="h6" color="primary">
							Suite: {status.name}
						</Typography>
					</Grid>
					<Grid item xs={10} md={10} lg={10}>
						{running && (
							<LinearProgress variant="determinate" value={percent} />
						)}
					</Grid>
					<Grid item xs={1} md={1} lg={1}>
						{running && (
							<Typography variant="subtitle1" color="textSecondary">
								{summary.run} / {total}
							</Typography>
						)}
					</Grid>
					<Grid item xs={1} md={1} lg={1}>
						<Button
							variant="outlined"
							color={running ? "secondary" : "primary"}
							onClick={() => (running ? this.stopClicked() : this.runClicked())}
						>
							{running ? "Stop" : "Run"}
						</Button>
					</Grid>
					<Grid item xs={12} md={12} lg={12}>
						{running && (
							<Typography variant="subtitle1" color="textSecondary">
								Running:{" "}
								{current ? current.class + ": " + current.selector : ""}
							</Typography>
						)}
					</Grid>
					<Grid item xs={12} md={12} lg={12}>
						<Grid
							container
							spacing={1}
							justify="space-around"
							alignItems="flex-end"
						>
							{Object.keys(summary).map((type) => {
								return (
									<Grid item xs={2} md={2} lg={2} key={"grid-" + type}>
										<Card
											id={"card-" + type}
											style={{ backgroundColor: this.typeColor(type) }}
										>
											<CardActionArea
												onClick={(event) => this.filterTests(type)}
											>
												<CardContent key={"card-content-" + type}>
													<Typography
														variant={type === "run" ? "h2" : "h3"}
														style={{ color: "white" }}
													>
														{summary[type] || 0}
													</Typography>
													<Typography
														variant={"button"}
														style={{ color: "white" }}
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
					</Grid>
					<Grid item xs={12} md={12} lg={12}>
						{ranking
							.filter((c) => {
								return !filterType || grouped[c].summary[filterType] > 0;
							})
							.map((c) => {
								const classSummary = grouped[c].summary;
								const classTests = filterType
									? grouped[c][filterType]
									: grouped[c].all;
								classTests.forEach((t) => {
									t.color = this.typeColor(t.type);
								});
								const data = {
									labels: [c],
									datasets: Object.keys(classSummary)
										.filter((type) => {
											return type !== "run" && classSummary[type] > 0;
										})
										.map((type) => {
											return {
												label: type,
												backgroundColor: this.typeColor(type),
												borderWidth: 0,
												data: [classSummary[type] || 0],
											};
										}),
								};
								const options = {
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
								return (
									<Accordion key={c}>
										<AccordionSummary
											style={{ maxHeight: 15 }}
											expandIcon={<ExpandMoreIcon />}
											id={c}
										>
											<Bar height={15} data={data} options={options} />
										</AccordionSummary>
										<Grid container>
											<Grid item xs={1} md={1} lg={1} />
											<Grid
												item
												xs={11}
												md={11}
												lg={11}
												style={{ minHeight: 300 }}
											>
												<CustomTable
													styles={styles}
													columns={this.testColumns()}
													rows={classTests}
													menuOptions={this.menuOptions()}
													rowActions={this.testActions()}
												></CustomTable>
											</Grid>
										</Grid>
									</Accordion>
								);
							})}
					</Grid>
				</Grid>
			</div>
		);
	}
}

export default TestRunner;
