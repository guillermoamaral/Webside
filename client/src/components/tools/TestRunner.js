import React, { Component } from "react";
import { IDEContext } from "../IDEContext";
import LinearProgress from "@material-ui/core/LinearProgress";
import {
	Accordion,
	AccordionSummary,
	Typography,
	Grid,
	Box,
	Button,
	Card,
	CardContent,
	CardActionArea,
} from "@material-ui/core";
import ExpandMoreIcon from "@material-ui/icons/ExpandMore";
import CustomTable from "../controls/CustomTable";

class TestRunner extends Component {
	static contextType = IDEContext;
	
	constructor(props) {
		super(props);
		this.state = {
			status: {
				name: "",
				total: 0,
				running: false,
				current: null,
				summary: this.newSummary(),
			},
			results: {
				updated: false,
				groups: {},
			},
			filterType: null,
		};
	}

	componentDidMount() {
		this.updateStatus();
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
				color = "#ffc107";
				break;
			case "error":
				color = "#dc3545";
				break;
			case "skipped":
				color = "#6c757d";
				break;
			case "knownIssue":
				color = "#007bff";
				break;
			default:
				color = "default";
		}
		return color;
	}

	summaryLabels() {
		return [
			{ type: "passed", text: "Passed" },
			{ type: "failed", text: "Failed" },
			{ type: "error", text: "Errors" },
			{ type: "skipped", text: "Skipped" },
			{ type: "knownIssue", text: "Known Issues" },
			{ type: "run", text: "Run" },
		];
	}

	async runClicked() {
		this.setState({
			status: {
				total: 0,
				running: true,
				current: null,
				summary: this.newSummary(),
			},
			results: { updated: false, groups: {} },
		});
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
			const tests = await this.context.api.getTestRunResults(this.props.id);
			const groups = {};
			tests.forEach((t) => {
				if (!groups[t.class]) {
					groups[t.class] = { summary: {}, tests: [] };
				}
				groups[t.class].tests.push(t);
			});
			Object.keys(groups).forEach((c) => {
				groups[c].tests.forEach((t) => {
					groups[c].summary[t.type] = groups[c].summary[t.type] + 1 || 1;
					groups[c].summary.run = groups[c].summary.run + 1 || 1;
				});
			});
			this.setState({ results: { updated: true, groups: groups } });
		} catch (error) {
			this.context.reportError(error);
		}
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
			{ label: "Debug", action: this.debugTest },
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

	render() {
		const styles = this.props.styles;
		const { status, results, filterType } = this.state;
		const { total, running, current } = status;
		const summary = status.summary || this.newSummary();
		const percent = total > 0 ? (summary.run / total) * 100 : 0;
		const groups = results.groups;
		const ranking = Object.keys(groups).sort((c1, c2) => {
			const n1 = groups[c1].summary.failed || 0 + groups[c1].summary.error || 0;
			const n2 = groups[c2].summary.failed || 0 + groups[c2].summary.error || 0;
			return n1 > n2 ? -1 : n1 < n2 ? 1 : 0;
		});
		const testColumns = [
			{ field: "selector", label: "Selector", minWidth: 100, align: "left" },
			{ field: "time", label: "Time (ms)", minWidth: 100, align: "right" },
		];
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
							{this.summaryLabels().map((label) => {
								return (
									<Grid item xs={2} md={2} lg={2} key={"grid-" + label.type}>
										<Card
											id={"card-" + label.type}
											style={{ backgroundColor: this.typeColor(label.type) }}
										>
											<CardActionArea
												onClick={(event) => this.filterTests(label.type)}
											>
												<CardContent key={"card-content-" + label.type}>
													<Typography
														variant={label.type === "run" ? "h2" : "h3"}
														style={{ color: "white" }}
													>
														{summary[label.type] || 0}
													</Typography>
													<Typography
														variant={"button"}
														style={{ color: "white" }}
														align="right"
													>
														{label.text}
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
							.filter((c) => !filterType || groups[c].summary[filterType] > 0)
							.map((c) => {
								const classSummary = groups[c].summary;
								const classTests = groups[c].tests.filter(
									(t) => !filterType || t.type === filterType
								);
								classTests.forEach((r) => {
									r.color = this.typeColor(r.type);
								});
								return (
									<Accordion key={c}>
										<AccordionSummary expandIcon={<ExpandMoreIcon />} id={c}>
											<Box display="flex" p={1}>
												<Box p={1} width="100%">
													<Typography>{c}</Typography>
												</Box>
												{this.summaryLabels().map((label) => {
													return (
														<Box p={1} flexShrink={0} key={"box-" + label.type}>
															<Typography
																style={{ color: this.typeColor(label.type) }}
															>
																{(classSummary[label.type] || 0) +
																	" " +
																	label.text}
															</Typography>
														</Box>
													);
												})}
											</Box>
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
													columns={testColumns}
													rows={classTests}
													menuOptions={this.menuOptions()}
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
