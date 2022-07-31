import React, { Component } from "react";
import {
	Grid,
	Paper,
	Box,
	Button,
	TextField,
	Typography,
	LinearProgress,
} from "@material-ui/core";
import { IDEContext } from "../IDEContext";
import ChangesTable from "../parts/ChangesTable";
import API from "../API";

class CodeMigrator extends Component {
	static contextType = IDEContext;

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
				"Method " + this.props.method.class + ">>" + this.props.method.selector
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
				const pack = await this.context.api.getPackage(name);
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
						const species = await this.context.api.getClass(classname);
						classes.push(species);
						const meta = await this.context.api.getClass(species.class);
						classes.push(meta);
					})
				);
			})
		);
		await Promise.all(
			this.state.sources.classes.map(async (name) => {
				const species = await this.context.api.getClass(name);
				classes.push(species);
				const meta = await this.context.api.getClass(species.class);
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
						const method = await this.context.api.getMethod(
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
				const retrieved = await this.context.api.getMethods(species.name, true);
				retrieved.forEach(async (method) => {
					methods.push(method);
				});
			})
		);
		methods.push(...this.state.sources.methods);
		return methods;
	}

	classDefinition(species) {
		return {
			type: "AddClass",
			author: this.context.api.author,
			class: species.name,
			label: species.name,
			package: species.package,
			definition: species.definition,
		};
	}

	methodDefinition(method) {
		return {
			type: "AddMethod",
			author: this.context.api.author,
			class: method.class,
			label: method.class + ">>" + method.selector,
			package: method.package,
			sourceCode: method.source,
		};
	}

	applyChanges = async () => {
		const api = new API(
			this.state.targetURL,
			this.context.api.author,
			this.reportError,
			this.reportChange
		);
		await Promise.all(
			this.state.changes.map(async (change) => {
				try {
					await api.postChange(change);
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
					<Typography variant="h6">Source: {this.sourceLabel()}</Typography>
				</Grid>
				<Grid item xs={12} md={12} lg={12}>
					{(generating || migrating) && (
						<LinearProgress variant="indeterminate" />
					)}
				</Grid>
				<Grid item xs={12} md={12} lg={12}>
					<TextField
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
							styles={this.props.styles}
							style={{ height: "100%" }}
							changes={changes}
							onSelect={(change) => this.setState({ selectedChange: change })}
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
								generating || migrating || changes.length === 0 || !targetURL
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
