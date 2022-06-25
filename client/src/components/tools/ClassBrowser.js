import React, { Component } from "react";
import {
	Grid,
	Box,
	Paper,
	RadioGroup,
	FormControlLabel,
	Radio,
	Select,
	MenuItem,
	OutlinedInput,
	IconButton,
	Tooltip,
} from "@material-ui/core";
import clsx from "clsx";
import { IDEContext } from "../IDEContext";
import SearchList2 from "../controls/SearchList2";
import ClassTree from "../parts/ClassTree";
import VariableList from "../parts/VariableList";
import CategoryList from "../parts/CategoryList";
import MethodList from "../parts/MethodList";
import CodeBrowser from "../parts/CodeBrowser";
import UpIcon from "@material-ui/icons/ArrowDropUp";

class ClassBrowser extends Component {
	static contextType = IDEContext;

	constructor(props) {
		super(props);
		this.cache = {};
		this.state = {
			root: this.props.root,
			selectedClass: null,
			selectedAccess: "accessing",
			selectedVariable: null,
			selectedCategory: null,
			selectedMethod: null,
			selectedSide: "instance",
		};
	}

	componentDidMount() {
		this.changeRootClass(this.state.root);
	}

	changeRootClass = async (name) => {
		if (!name) {
			return;
		}
		try {
			const species = await this.context.api.getClassTree(name, 3);
			this.cache[name] = species;
			const side = name.endsWith(" class") ? "class" : "instance";
			this.setState({ root: name, selectedSide: side }, () => {
				this.classSelected(species);
			});
		} catch (error) {
			this.context.reportError(error);
		}
	};

	currentSelections() {
		return {
			class: this.state.selectedClass,
			access: this.state.selectedAccess,
			variable: this.state.selectedVariable,
			category: this.state.selectedCategory,
			method: this.state.selectedMethod,
		};
	}

	applySelections(selections) {
		this.setState((prevState, props) => {
			const species = selections.class;
			if (species && !this.cache[species.name]) {
				this.cache[species.name] = species;
			}
			return {
				selectedClass: selections.class,
				selectedAccess: selections.access,
				selectedVariable: selections.variable,
				selectedCategory: selections.category,
				selectedMethod: selections.method,
			};
		});
	}

	reviseSelections(selections) {
		const species = selections.class;
		var variable = selections.variable;
		var method = selections.method;
		if (variable) {
			variable = species.variables.find((v) => v.name === variable.name);
			selections.variable = !variable ? null : variable;
		}
		if (!species.categories.includes(selections.category)) {
			selections.category = null;
		}
		if (method) {
			method = species.methods.find((m) => m.selector === method.selector);
			selections.method = !method ? null : method;
		}
	}

	// Contents..
	currentVariables() {
		const species = this.state.selectedClass;
		return !species || !species.variables ? [] : species.variables;
	}

	currentCategories = () => {
		const species = this.state.selectedClass;
		return !species || !species.categories ? [] : species.categories;
	};

	currentMethods = () => {
		const species = this.state.selectedClass;
		const category = this.state.selectedCategory;
		const variable = this.state.selectedVariable;
		const access = this.state.selectedAccess;
		if (!species) {
			return [];
		}
		var methods = species.methods;
		if (category) {
			methods = methods.filter((m) => m.category === category);
		}
		if (variable && species.accessors && species.accessors[variable.name]) {
			const accessing = species.accessors[variable.name][access];
			methods = methods.filter((m) =>
				accessing.some((n) => n.selector === m.selector)
			);
		}
		if (methods && methods.length === 0) {
			methods.push({
				class: species,
				category: category,
				selector: "<new>",
				source: 'messagePattern\r\t"comment"\r\t| temporaries |\r\tstatements',
			});
		}
		return methods;
	};

	// Updating...
	async updateClass(selections, force = false) {
		const species = selections.class;
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
		const species = selections.class;
		try {
			if (force || !species.variables) {
				species.variables = await this.context.api.getVariables(species.name);
			}
			var variable = selections.variable;
			if (variable) {
				variable = species.variables.find((v) => v.name === variable.name);
				selections.variable = !variable ? null : variable;
			}
		} catch (error) {
			this.context.reportError(error);
		}
	}

	async updateCategories(selections, force = false) {
		const species = selections.class;
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
		const species = selections.class;
		const variable = selections.variable;
		const access = selections.access;
		var method = selections.method;
		if (!species) {
			return;
		}
		try {
			if (force || !species.methods) {
				const methods = await this.context.api.getMethods(species.name, true);
				species.methods = methods;
			}
			if (
				variable &&
				(force ||
					!species.accessors ||
					!species.accessors[variable.name] ||
					!species.accessors[variable.name][access])
			) {
				const accessing = await this.context.api.getMethodsAccessing(
					species.name,
					variable.name,
					access,
					true
				);
				species.accessors = {};
				species.accessors[variable.name] = {};
				species.accessors[variable.name][access] = accessing;
			}
			if (method) {
				method = species.methods.find((m) => m.selector === method.selector);
				selections.method = !method ? null : method;
			}
		} catch (error) {
			this.context.reportError(error);
		}
	}

	async updateMethod(selections, force = true) {
		const species = selections.class;
		const selector = selections.method.selector;
		var method;
		if (force) {
			try {
				method = await this.context.api.getMethod(species.name, selector);
			} catch (error) {
				this.context.reportError(error);
			}
			if (method) {
				species.methods = species.methods.map((m) =>
					m.selector === selector ? method : m
				);
				selections.method = method;
			}
		}
	}

	// Events...
	goToSuperclassClicked = () => {
		const current = this.cache[this.state.root];
		if (current && current.superclass) {
			this.changeRootClass(current.superclass);
		}
	};

	sideChanged = (event, side) => {
		if (!side || !this.state.root) {
			return;
		}
		var root = this.state.root;
		root =
			side === "instance" ? root.slice(0, root.length - 6) : root + " class";
		this.changeRootClass(root);
	};

	classSelected = async (species) => {
		// this.context.updatePageLabel(this.props.id, species.name);
		const selections = this.currentSelections();
		selections.class = species;
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

	classDefined = async (species) => {
		var cached = this.cache[species.name];
		if (cached) {
			cached.definition = species.definition;
		} else {
			this.cache[species.name] = species;
			cached = species;
			const superclass = this.cache[species.superclass];
			if (superclass) {
				superclass.subclasses.push(species);
				superclass.subclasses.sort((a, b) => (a.name <= b.name ? -1 : 1));
			}
		}
		const selections = this.currentSelections();
		selections.class = cached;
		await this.updateVariables(selections, true);
		this.classSelected(cached);
	};

	classCommented = async (species) => {
		this.cache[species.name].comment = species.comment;
	};

	classRemoved = (species) => {
		delete this.cache[species.name];
		const superclass = this.cache[species.superclass];
		if (superclass) {
			superclass.subclasses = superclass.subclasses.filter(
				(c) => c !== species
			);
			this.classSelected(superclass);
		} else {
			this.changeRootClass("Object");
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
		await this.updateClass(selections, true);
		await this.updateVariables(selections, true);
		await this.updateMethods(selections, true);
		this.applySelections(selections);
	};

	variableRenamed = async (variable) => {
		const selections = this.currentSelections();
		await this.updateClass(selections, true);
		await this.updateVariables(selections, true);
		await this.updateMethods(selections, true);
		this.variableSelected(
			selections.class.variables.find((v) => v.name === variable.name)
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
		selections.class.categories.push(category);
		selections.class.categories.sort();
		this.applySelections(selections);
	};

	categoryRenamed = async (category, renamed) => {
		const selections = this.currentSelections();
		await this.updateCategories(selections, true);
		await this.updateMethods(selections, true);
		this.categorySelected(
			selections.class.categories.find((c) => c === renamed)
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
		const species = selections.class;
		selections.method = species.methods.find(
			(m) => m.selector === method.selector
		);
		this.applySelections(selections);
	};

	methodRemoved = (method) => {
		this.cache[method.class].methods = this.cache[method.class].methods.filter(
			(m) => m.selector !== method.selector
		);
		const selections = this.currentSelections();
		selections.method = null;
		this.applySelections(selections);
	};

	methodClassified = (method) => {
		const selections = this.currentSelections();
		if (selections.category) {
			selections.category = method.category;
		}
		this.applySelections(selections);
	};

	methodCompiled = async (method) => {
		if (!method) {
			return;
		}
		const selections = this.currentSelections();
		const species = this.cache[method.class];
		selections.class = species;
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
		console.log("rendering browser");
		const {
			root,
			selectedSide,
			selectedClass,
			selectedAccess,
			selectedVariable,
			selectedCategory,
			selectedMethod,
		} = this.state;
		const styles = this.props.styles;
		const fixedHeightPaper = clsx(styles.paper, styles.fixedHeight);
		const rootclass = this.cache[root];
		return (
			<Grid container spacing={1}>
				<Grid item xs={12} md={12} lg={12}>
					<Grid container spacing={1}>
						<Grid item xs={3} md={3} lg={3}>
							<Grid container direction="row" alignItems="center">
								<Grid item xs={11} md={11} lg={11}>
									<SearchList2
										value={selectedClass ? selectedClass.name : null}
										options={this.context.classNames}
										onChange={(classname) => {
											this.changeRootClass(classname);
										}}
									/>
								</Grid>
								<Grid item xs={1} md={1} lg={1}>
									<Tooltip
										title={rootclass ? rootclass.superclass : ""}
										placement="top"
									>
										<IconButton
											color="inherit"
											size="small"
											onClick={this.goToSuperclassClicked}
										>
											<UpIcon />
										</IconButton>
									</Tooltip>
								</Grid>
							</Grid>
						</Grid>
						<Grid item xs={3} md={3} lg={3}>
							<Select
								value={selectedAccess}
								input={<OutlinedInput margin="dense" fullWidth />}
								onChange={this.accessSelected}
							>
								<MenuItem value={"using"}>using</MenuItem>
								<MenuItem value={"assigning"}>assigning</MenuItem>
								<MenuItem value={"accessing"}>referencing</MenuItem>
							</Select>
						</Grid>
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
								<ClassTree
									roots={
										root ? (this.cache[root] ? [this.cache[root]] : []) : []
									}
									selected={selectedClass}
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
								<VariableList
									class={selectedClass}
									variables={this.currentVariables()}
									selected={selectedVariable}
									onAdd={this.variableAdded}
									onRename={this.variableRenamed}
									onSelect={this.variableSelected}
									onRemove={this.variableRemoved}
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
									categories={this.currentCategories()}
									selected={selectedMethod}
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

export default ClassBrowser;
