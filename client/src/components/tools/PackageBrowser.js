import React, { Component } from "react";
import {
	Grid,
	Box,
	Paper,
	RadioGroup,
	FormControlLabel,
	Radio,
} from "@material-ui/core";
import clsx from "clsx";
import { IDEContext } from "../IDEContext";
import PackageList from "../parts/PackageList";
import ClassTree from "../parts/ClassTree";
import CategoryList from "../parts/CategoryList";
import MethodList from "../parts/MethodList";
import CodeBrowser from "../parts/CodeBrowser";

class PackageBrowser extends Component {
	static contextType = IDEContext;

	constructor(props) {
		super(props);
		this.cache = { packages: {}, classes: {} };
		this.state = {
			packages: [],
			selectedPackage: null,
			selectedClass: null,
			selectedCategory: null,
			selectedMethod: null,
			selectedSide: "instance",
		};
	}

	async componentDidMount() {
		await this.initializePackages();
	}

	initializePackages = async () => {
		let names;
		try {
			names = await this.context.api.getPackageNames();
		} catch (error) {
			names = [];
			this.context.reportError(error);
		}
		var selected;
		const packages = names.map((name) => {
			const pack = { name: name };
			if (this.props.selectedPackage === name) {
				selected = pack;
			}
			return pack;
		});
		this.setState({ packages: packages }, () => {
			if (selected) {
				this.packageSelected(selected);
			}
		});
	};

	currentSelections() {
		return {
			package: this.state.selectedPackage,
			species: this.state.selectedClass,
			category: this.state.selectedCategory,
			method: this.state.selectedMethod,
		};
	}

	applySelections(selections) {
		this.setState((prevState, props) => {
			const pack = selections.package;
			if (pack && !this.cache.packages[pack.name]) {
				this.cache.packages[pack.name] = pack;
			}
			return {
				selectedPackage: selections.package,
				selectedClass: selections.species,
				selectedCategory: selections.category,
				selectedMethod: selections.method,
			};
		});
	}

	// Contents...
	currentClasses() {
		const pack = this.state.selectedPackage;
		return !pack ? [] : pack.classes;
	}

	currentVariables() {
		const species = this.state.selectedClass;
		return !species || !species.variables ? [] : species.variables;
	}

	currentCategories = () => {
		const species = this.state.selectedClass;
		return !species || !species.categories ? [] : species.categories;
	};

	currentMethods = () => {
		const pack = this.state.selectedPackage;
		const species = this.state.selectedClass;
		const category = this.state.selectedCategory;
		if (!pack || !species) {
			return [];
		}
		var methods = species.methods.filter((m) => (m.package = pack.name));
		if (category) {
			methods = methods.filter((m) => m.category === category);
		}
		return methods;
	};

	// Updating...
	async updatePackage(selections, force = false) {
		const pack = selections.package;
		try {
			if (force || !pack.classes || !pack.methods) {
				const retrieved = await this.context.api.getPackage(pack.name);
				Object.assign(pack, retrieved);
			}
		} catch (error) {
			this.context.reportError(error);
		}
	}

	async updateClasses(selections, force = false) {
		const pack = selections.package;
		if (force || !pack.classes) {
			try {
				pack.classes = await this.context.api.getPackageClasses(
					pack.name,
					true
				);
			} catch (error) {
				this.context.reportError(error);
			}
		}
	}

	async updateClass(selections, force = false) {
		const species = selections.species;
		try {
			if (force || !species.definition) {
				const definition = await this.context.api.getClass(species.name);
				Object.assign(species, definition);
			}
			if (force || !species.subclasses) {
				species.subclasses = await this.context.api.getSubclasses(species.name);
			}
		} catch (error) {
			this.context.reportError(error);
		}
	}

	async updateSubclasses(species) {
		try {
			if (species.subclasses) {
				await Promise.all(
					species.subclasses.map(async (c) => {
						if (!c.subclasses) {
							c.subclasses = await this.context.api.getSubclasses(c.name);
						}
					})
				);
			}
		} catch (error) {
			this.context.reportError(error);
		}
	}

	async updateVariables(selections, force = false) {
		const { species, variable } = selections;
		try {
			if (force || !species.variables) {
				species.variables = await this.context.api.getVariables(species.name);
			}
			if (variable) {
				const found = species.variables.find((v) => v.name === variable.name);
				selections.variable = !found ? null : found;
			}
		} catch (error) {
			this.context.reportError(error);
		}
	}

	async updateCategories(selections, force = false) {
		const species = selections.species;
		try {
			if (force || !species.categories) {
				const categories = await this.context.api.getCategories(species.name);
				species.categories = categories.sort();
			}
			if (!species.categories.includes(selections.category)) {
				selections.category = null;
			}
		} catch (error) {
			this.context.reportError(error);
		}
	}

	async updateMethods(selections, force = false) {
		const pack = selections.package;
		const { species, variable, access, method } = selections;
		if (!pack || !species) {
			return;
		}
		try {
			if (force || !species.methods) {
				species.methods = await this.context.api.getMethods(species.name, true);
			}
			if (
				variable &&
				(force || !species[variable.name] || !species[variable.name][access])
			) {
				const accessors = await this.context.api.getMethodsAccessing(
					species.name,
					variable.name,
					access,
					true
				);
				species[variable.name] = {};
				species[variable.name][access] = accessors;
			}
			if (method) {
				const found = species.methods.find(
					(m) => m.selector === method.selector
				);
				selections.method = !found ? null : found;
			}
		} catch (error) {
			this.context.reportError(error);
		}
	}

	async updateMethod(selections, force = true) {
		const species = selections.species;
		const selector = selections.method.selector;
		try {
			if (force) {
				const method = await this.context.api.getMethod(species.name, selector);
				if (method) {
					species.methods = species.methods.map((m) =>
						m.selector === selector ? method : m
					);
					selections.method = method;
				}
			}
		} catch (error) {
			this.context.reportError(error);
		}
	}

	// Events...
	sideChanged = (event, side) => {
		if (!side) return;
		this.setState({ selectedSide: side });
		console.log("to do");
	};

	packageSelected = async (pack) => {
		const selections = this.currentSelections();
		selections.package = pack;
		await this.updatePackage(selections, true);
		await this.updateClasses(selections, true);
		this.applySelections(selections);
	};

	classSelected = async (species) => {
		const selections = this.currentSelections();
		selections.species = species;
		await this.updateClass(selections);
		await this.updateSubclasses(species);
		await this.updateVariables(selections);
		await this.updateCategories(selections);
		await this.updateMethods(selections);
		this.applySelections(selections);
	};

	classExpanded = async (species) => {
		await this.updateSubclasses(species);
	};

	classLabelStyle = (species) => {
		const pack = this.state.selectedPackage;
		return pack && pack.methods && pack.methods[species.name]
			? "italic"
			: "normal";
	};

	methodLabelStyle = (method) => {
		const pack = this.state.selectedPackage;
		return pack &&
			pack.methods &&
			pack.methods[method.class] &&
			pack.methods[method.class].includes(method.selector)
			? "italic"
			: "normal";
	};

	classDefined = async (species) => {
		var cached = this.cache.classes[species.name];
		if (cached) {
			cached.definition = species.definition;
		} else {
			this.cache.classes[species.name] = species;
			cached = species;
			const superclass = this.cache.classes[species.superclass];
			if (superclass) {
				superclass.subclasses.push(species);
				superclass.subclasses.sort((a, b) => (a.name <= b.name ? -1 : 1));
			}
		}
		const selections = this.currentSelections();
		selections.species = cached;
		await this.updateVariables(selections, true);
		this.classSelected(cached);
	};

	classCommented = async (species) => {
		this.cache.classes[species.name].comment = species.comment;
	};

	classRemoved = (species) => {
		delete this.cache.classes[species.name];
		const superclass = this.cache.classes[species.superclass];
		if (superclass) {
			superclass.subclasses = superclass.subclasses.filter(
				(c) => c !== species
			);
			this.classSelected(superclass);
		} else {
			this.changeRootProject("Object");
		}
	};

	classRenamed = (species) => {
		this.classSelected(species);
	};

	accessSelected = async (event) => {
		const access = event.target.value;
		const selections = this.currentSelections();
		selections.access = access;
		await this.updateMethods(selections);
		this.applySelections(selections);
	};

	variableSelected = async (variable) => {
		const selections = this.currentSelections();
		selections.variable = variable;
		await this.updateMethods(selections);
		this.applySelections(selections);
	};

	variableAdded = async () => {
		const selections = this.currentSelections();
		await this.updateVariables(selections, true);
		await this.updateMethods(selections, true);
		this.applySelections(selections);
	};

	variableRenamed = async (variable) => {
		const selections = this.currentSelections();
		await this.updateVariables(selections, true);
		await this.updateMethods(selections, true);
		this.variableSelected(
			selections.species.variables.find((v) => v.name === variable.name)
		);
	};

	variableRemoved = async () => {
		const selections = this.currentSelections();
		await this.updateClass(selections, true);
		await this.updateVariables(selections, true);
		await this.updateMethods(selections);
		this.applySelections(selections);
	};

	categorySelected = async (category) => {
		const selections = this.currentSelections();
		selections.category = category;
		await this.updateMethods(selections);
		this.applySelections(selections);
	};

	categoryAdded = async (category) => {
		const selections = this.currentSelections();
		selections.category = category;
		selections.species.categories.push(category);
		selections.species.categories.sort();
		this.applySelections(selections);
	};

	categoryRenamed = async (category, renamed) => {
		const selections = this.currentSelections();
		await this.updateCategories(selections, true);
		await this.updateMethods(selections, true);
		this.categorySelected(
			selections.species.categories.find((c) => c === renamed)
		);
	};

	categoryRemoved = async (category) => {
		const selections = this.currentSelections();
		selections.category = null;
		await this.updateCategories(selections, true);
		await this.updateMethods(selections);
		this.applySelections(selections);
	};

	methodSelected = async (method) => {
		const selections = this.currentSelections();
		selections.method = method;
		await this.updateMethod(selections);
		this.applySelections(selections);
	};

	methodRenamed = async (method) => {
		const selections = this.currentSelections();
		await this.updateMethods(selections, true);
		const species = selections.species;
		selections.method = species.methods.find(
			(m) => m.selector === method.selector
		);
		this.applySelections(selections);
	};

	methodRemoved = (method) => {
		this.cache.classes[method.class].methods = this.cache.classes[
			method.class
		].methods.filter((m) => m.selector !== method.selector);
		this.setState({ selectedMethod: null });
	};

	methodClassified = (method) => {
		const selections = this.currentSelections();
		if (selections.category) {
			selections.category = method.category;
		}
		this.applySelections(selections);
	};

	methodCompiled = async (method) => {
		const selections = this.currentSelections();
		const species = this.cache.classes[method.class];
		selections.species = species;
		if (!species.categories.includes(method.category)) {
			await this.updateCategories(selections, true);
		}
		selections.category = species.categories.find((c) => c === method.category);
		const methods = species.methods;
		const index = methods
			? methods.findIndex((m) => m.selector === method.selector)
			: -1;
		if (index === -1) {
			await this.updateMethods(selections, true);
			selections.method = species.methods.find(
				(m) => m.selector === method.selector
			);
		} else {
			methods.splice(index, 1, method);
			selections.method = method;
		}
		this.applySelections(selections);
	};

	render() {
		const {
			packages,
			selectedSide,
			selectedPackage,
			selectedClass,
			selectedCategory,
			selectedMethod,
		} = this.state;
		const styles = this.props.styles;
		const fixedHeightPaper = clsx(styles.paper, styles.fixedHeight);
		return (
			<Grid container spacing={1}>
				<Grid item xs={12} md={12} lg={12}>
					<Grid container spacing={1}>
						<Grid item xs={3} md={3} lg={3} />
						<Grid item xs={3} md={3} lg={3} />
						<Grid item xs={3} md={3} lg={3}>
							<Box display="flex" justifyContent="center">
								<RadioGroup
									name="side"
									value={selectedSide}
									onChange={this.sideChanged}
									defaultValue="instance"
									row
								>
									<FormControlLabel
										value="instance"
										control={<Radio size="small" color="primary" />}
										label="Instance"
									/>
									<FormControlLabel
										value="class"
										control={<Radio size="small" color="primary" />}
										label="Class"
									/>
								</RadioGroup>
							</Box>
						</Grid>
						<Grid item xs={3} md={3} lg={3} />
						<Grid item xs={12} md={3} lg={3}>
							<Paper className={fixedHeightPaper} variant="outlined">
								<PackageList
									packages={packages}
									selected={selectedPackage}
									onSelect={this.packageSelected}
								/>
							</Paper>
						</Grid>
						<Grid item xs={12} md={3} lg={3}>
							<Paper className={fixedHeightPaper} variant="outlined">
								<ClassTree
									roots={selectedPackage ? selectedPackage.classes : []}
									selected={selectedClass}
									labelStyle={this.classLabelStyle}
									onExpand={this.classExpanded}
									onSelect={this.classSelected}
									onRemove={this.classRemoved}
									onRename={this.classRenamed}
									onCreate={this.classDefined}
								/>
							</Paper>
						</Grid>
						<Grid item xs={12} md={3} lg={3}>
							<Paper className={fixedHeightPaper} variant="outlined">
								<CategoryList
									class={selectedClass}
									categories={this.currentCategories()}
									selected={selectedCategory}
									onAdd={this.categoryAdded}
									onRename={this.categoryRenamed}
									onSelect={this.categorySelected}
									onRemove={this.categoryRemoved}
								/>
							</Paper>
						</Grid>
						<Grid item xs={12} md={3} lg={3}>
							<Paper className={fixedHeightPaper} variant="outlined">
								<MethodList
									methods={this.currentMethods()}
									selected={selectedMethod}
									labelStyle={this.methodLabelStyle}
									onSelect={this.methodSelected}
									onRename={this.methodRenamed}
									onRemove={this.methodRemoved}
									onClassify={this.methodClassified}
								/>
							</Paper>
						</Grid>
					</Grid>
				</Grid>
				<Grid item xs={12} md={12} lg={12}>
					<CodeBrowser
						context={{ class: selectedClass ? selectedClass.name : null }}
						styles={styles}
						class={selectedClass}
						method={selectedMethod}
						onMethodCompiled={this.methodCompiled}
						onClassDefined={this.classDefined}
						onClassCommented={this.classCommented}
					/>
				</Grid>
			</Grid>
		);
	}
}

export default PackageBrowser;
