import React from "react";
import Tool from "./Tool";
import {
	Grid,
	Paper,
	Box,
	Button,
	TextField,
	Typography,
	LinearProgress,
} from "@mui/material";
import { ide } from "../IDE";
import ChangesTable from "../parts/ChangesTable";
import Backend from "../Backend";
import { AddClass, AddMethod } from "../../model/StChange";

class CodeMigrator extends Tool {
	constructor(props) {
		super(props);
		const packages = this.props.package ? [this.props.package] : [];
		const classes = this.props.class ? [this.props.class] : [];
		const methods = this.props.method ? [this.props.method] : [];
		this.state = {
			sources: { packages: packages, classes: classes, methods: methods },
			targetURL: "",
			changes: [],
			generating: false,
			migrating: false,
			selectedChange: null,
		};
	}

	sourceLabel() {
		if (this.props.package) {
			return "Package " + this.props.package;
		}
		if (this.props.class) {
			return "Class " + this.props.class;
		}
		if (this.props.method) {
			return (
				"Method " +
				this.props.method.methodClass +
				">>" +
				this.props.method.selector
			);
		}
		return "Mixed source";
	}

	generateChanges = async () => {
		const changes = [];
		this.setState({ changes: [], generating: true });
		const packages = await this.sourcePackages();
		const classes = await this.sourceClasses(packages);
		classes.forEach((species) => {
			const change = this.classDefinition(species);
			changes.push(change);
		});
		const methods = await this.sourceMethods(packages, classes);
		methods.forEach((method) => {
			const change = this.methodDefinition(method);
			changes.push(change);
		});
		this.setState({ changes: changes, generating: false });
	};

	async sourcePackages() {
		const packages = [];
		await Promise.all(
			this.state.sources.packages.map(async (name) => {
				const pack = await ide.backend.packageNamed(name);
				packages.push(pack);
			})
		);
		return packages;
	}

	async sourceClasses(packages) {
		const classes = [];
		await Promise.all(
			packages.map(async (pack) => {
				await Promise.all(
					pack.classes.map(async (classname) => {
						const species = await ide.backend.classNamed(classname);
						classes.push(species);
						const meta = await ide.backend.classNamed(
							species.class
						);
						classes.push(meta);
					})
				);
			})
		);
		await Promise.all(
			this.state.sources.classes.map(async (name) => {
				const species = await ide.backend.classNamed(name);
				classes.push(species);
				const meta = await ide.backend.classNamed(species.class);
				classes.push(meta);
			})
		);
		return classes;
	}

	async sourceMethods(packages, classes) {
		const methods = [];
		await Promise.all(
			packages.map(async (pack) => {
				Object.entries(pack.methods).forEach(async (selectors) => {
					selectors[1].forEach(async (selector) => {
						const method = await ide.backend.method(
							selectors[0],
							selector
						);
						methods.push(method);
					});
				});
			})
		);
		await Promise.all(
			classes.map(async (species) => {
				const retrieved = await ide.backend.methods(species.name, true);
				retrieved.forEach(async (method) => {
					methods.push(method);
				});
			})
		);
		methods.push(...this.state.sources.methods);
		return methods;
	}

	classDefinition(species) {
		const change = new AddClass();
		change.author = ide.backend.author;
		change.className = species.name;
		change.label = species.name;
		change.package = species.package;
		change.definition = species.definition;
		return change;
	}

	methodDefinition(method) {
		const change = new AddMethod();
		change.author = ide.backend.author;
		change.className = method.methodClass;
		change.label = method.methodClass + ">>" + method.selector;
		change.package = method.package;
		change.source = method.source;
		return change;
	}

	applyChanges = async () => {
		const backend = new Backend(
			this.state.targetURL,
			ide.backend.author,
			this.reportError,
			this.reportChange
		);
		await Promise.all(
			this.state.changes.map(async (change) => {
				try {
					await backend.postChange(change.asJson());
					change.color = "#28a745";
				} catch (error) {
					change.error = error.data;
					change.color = "#dc3545";
				}
			})
		);
		this.setState({ changes: this.state.changes });
	};

	render() {
		const { targetURL, changes, selectedChange, generating, migrating } =
			this.state;
		const error =
			selectedChange && selectedChange.error
				? selectedChange.error.description
				: null;
		return (
			<Grid container spacing={1}>
				<Grid item xs={12} md={12} lg={12}>
					<Typography variant="h6">
						Source: {this.sourceLabel()}
					</Typography>
				</Grid>
				<Grid item xs={12} md={12} lg={12}>
					{(generating || migrating) && (
						<LinearProgress variant="indeterminate" />
					)}
				</Grid>
				<Grid item xs={12} md={12} lg={12}>
					<TextField
						size="small"
						value={targetURL}
						onChange={(event) =>
							this.setState({ targetURL: event.target.value })
						}
						placeholder="Target URL ..."
						name="text"
						variant="outlined"
						fullWidth
						margin="dense"
						autoFocus
						type="text"
						disabled={migrating || generating}
					/>
				</Grid>
				<Grid item xs={12} md={12} lg={12}>
					<Paper variant="outlined" style={{ height: 400 }}>
						<ChangesTable
							style={{ height: "100%" }}
							changes={changes}
							onChangeSelect={(change) =>
								this.setState({ selectedChange: change })
							}
						/>
					</Paper>
				</Grid>
				<Grid item xs={12} md={12} lg={12}>
					<Box
						display="flex"
						flexWrap="nowrap"
						alignItems="right"
						justifyContent="right"
					>
						<Button
							variant="outlined"
							disabled={generating || migrating}
							onClick={this.generateChanges}
						>
							Generate changes
						</Button>
						<Button
							variant="outlined"
							disabled={
								generating ||
								migrating ||
								changes.length === 0 ||
								!targetURL
							}
							onClick={this.applyChanges}
						>
							Apply changes
						</Button>
					</Box>
				</Grid>
				{error && (
					<Grid item xs={12} md={12} lg={12}>
						<Paper variant="outlined" style={{ minHeight: 200 }}>
							{error}
						</Paper>
					</Grid>
				)}
			</Grid>
		);
	}
}

export default CodeMigrator;
