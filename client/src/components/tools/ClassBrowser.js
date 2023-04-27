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
import { ide } from "../IDE";
import { container } from "../ToolsContainer";
import SearchList2 from "../controls/SearchList2";
import ClassTree from "../parts/ClassTree";
import VariableList from "../parts/VariableList";
import CategoryList from "../parts/CategoryList";
import MethodList from "../parts/MethodList";
import CodeBrowser from "../parts/CodeBrowser";
import UpIcon from "@material-ui/icons/ArrowDropUp";

class ClassBrowser extends Component {
	constructor(props) {
		super(props);
		this.classnames = [];
		this.cache = {};
		this.state = {
			root: this.props.root,
			selectedClass: null,
			selectedAccess: "accessing",
			selectedVariable: null,
			selectedCategory: null,
			selectedMethod: null,
			selectedSide: this.props.side || "instance",
		};
	}

	async componentDidMount() {
		this.initializeClassNames();
	}

	async initializeClassNames() {
		try {
			this.classnames = await ide.api.classNames();
			if (this.state.root) {
				this.changeRootClass(this.state.root);
			} else {
				this.setState({});
			}
		} catch (error) {
			ide.reportError(error);
		}
	}

	changeRootClass = async (classsname) => {
		if (!classsname) {
			return;
		}
		try {
			const species = await ide.api.classTree(classsname, 3);
			this.cache[classsname] = species;
			this.setState({ root: classsname }, () => {
				this.classSelected(species);
			});
		} catch (error) {
			ide.reportError(error);
		}
	};

	// Selections...
	currentSelections() {
		return {
			species: this.state.selectedClass,
			access: this.state.selectedAccess,
			variable: this.state.selectedVariable,
			category: this.state.selectedCategory,
			method: this.state.selectedMethod,
			side: this.state.selectedSide,
		};
	}

	applySelections(selections) {
		this.reviseSelections(selections);
		this.setState((prevState, props) => {
			const species = selections.species;
			if (species && !this.cache[species.name]) {
				this.cache[species.name] = species;
			}
			return {
				selectedClass: selections.species,
				selectedAccess: selections.access,
				selectedVariable: selections.variable,
				selectedCategory: selections.category,
				selectedMethod: selections.method,
				selectedSide: selections.side,
			};
		});
	}

	reviseSelections(selections) {
		const { access, category, side, method } = selections;
		var variable = selections.variable;
		var species = selections.species;
		if (!species) {
			selections.variable = null;
			selections.category = null;
			selections.method = null;
			return;
		}
		if (side === "class") {
			species = selections.species.metaclass;
		}
		if (variable) {
			selections.variable = species.variables.find(
				(v) => v.name === variable.name
			);
		}
		if (category) {
			selections.category = species.categories.find(
				(c) => c === category
			);
		}
		if (method && !method.template) {
			selections.method = species.methods.find(
				(m) => m.selector === method.selector
			);
			const accessors = species.accessors;
			variable = selections.variable;
			if (selections.method) {
				if (
					selections.category &&
					selections.method.category !== selections.category
				) {
					selections.method = null;
				}
				if (
					selections.method &&
					access &&
					variable &&
					accessors &&
					accessors[variable.name] &&
					accessors[variable.name][access] &&
					!accessors[variable.name][access].some(
						(m) => m.selector === selections.method.selector
					)
				) {
					selections.method = null;
				}
			}
		}
	}

	// Contents...
	currentClass() {
		const { selectedClass, selectedSide } = this.state;
		if (!selectedClass) return null;
		return selectedSide === "instance"
			? selectedClass
			: selectedClass.metaclass;
	}

	currentVariables() {
		const species = this.currentClass();
		return species ? species.variables || [] : [];
	}

	currentCategories = () => {
		const species = this.currentClass();
		return species ? species.categories || [] : [];
	};

	currentMethods = () => {
		const species = this.currentClass();
		if (!species) {
			return [];
		}
		const { variable, access, category } = this.currentSelections();
		var methods = species.methods;
		if (category) {
			methods = methods.filter((m) => m.category === category);
		}
		if (variable && species.accessors && species.accessors[variable.name]) {
			const accessing = species.accessors[variable.name][access] || [];
			methods = methods.filter((m) =>
				accessing.some((n) => n.selector === m.selector)
			);
		}
		if (methods && methods.length === 0) {
			const template = ide.api.methodTemplate();
			template.methodClass = species;
			template.category = category;
			methods.push(template);
		}
		return methods;
	};

	// Updating...
	async updateClass(species, forced = false) {
		try {
			if (forced || !species.definition || !species.metaclass) {
				const definition = await ide.api.classNamed(species.name);
				Object.assign(species, definition);
				species.metaclass = await ide.api.classNamed(definition.class);
			}
			if (forced || !species.subclasses) {
				species.subclasses = await ide.api.subclasses(species.name);
			}
		} catch (error) {
			ide.reportError(error);
		}
	}

	async updateSubclasses(species) {
		try {
			if (species.subclasses) {
				await Promise.all(
					species.subclasses.map(async (c) => {
						if (!c.subclasses) {
							c.subclasses = await ide.api.subclasses(c.name);
						}
					})
				);
			}
		} catch (error) {
			ide.reportError(error);
		}
	}

	async updateVariables(species, forced = false) {
		try {
			if (forced || !species.variables) {
				species.variables = await ide.api.variables(species.name);
			}
		} catch (error) {
			ide.reportError(error);
		}
	}

	async updateCategories(species, forced = false) {
		try {
			if (forced || !species.categories) {
				species.categories = await ide.api.categories(species.name);
				species.categories.sort();
			}
		} catch (error) {
			ide.reportError(error);
		}
	}

	async updateMethods(species, variable, access, forced = false) {
		if (!species) {
			return;
		}
		try {
			if (forced || !species.methods) {
				species.methods = await ide.api.methods(species.name, true);
				species.accessors = null;
			}
			if (
				variable &&
				access &&
				(forced ||
					!species.accessors ||
					!species.accessors[variable.name] ||
					!species.accessors[variable.name][access])
			) {
				const accessing = await ide.api.methodsAccessing(
					species.name,
					variable.name,
					access,
					true
				);
				species.accessors = {};
				species.accessors[variable.name] = {};
				species.accessors[variable.name][access] = accessing;
			}
		} catch (error) {
			ide.reportError(error);
		}
	}

	async updateMethod(method) {
		try {
			const retrieved = await ide.api.method(
				method.methodClass,
				method.selector
			);
			Object.assign(method, retrieved);
			if (!retrieved) {
				method.source = "method cannot be found";
				method.ast = null;
				method.bytecodes = null;
			}
		} catch (error) {
			ide.reportError(error);
		}
	}

	// Events...
	goToSuperclassClicked = () => {
		const current = this.cache[this.state.root];
		if (current && current.superclass) {
			this.changeRootClass(current.superclass);
		}
	};

	sideChanged = async (side) => {
		const selections = this.currentSelections();
		selections.side = side;
		var species = selections.species;
		if (species && side === "class") {
			species = selections.species.metaclass;
		}
		if (species) {
			await this.updateVariables(species);
			await this.updateCategories(species);
			await this.updateMethods(species);
		}
		this.applySelections(selections);
	};

	classSelected = async (species, keepSelections = true) => {
		container.updatePageLabel(this.props.id, species.name);
		const selections = this.currentSelections();
		selections.species = species;
		selections.category = null;
		await this.updateClass(species);
		await this.updateSubclasses(species);
		const target =
			selections.side === "instance" ? species : species.metaclass;
		await this.updateVariables(target);
		await this.updateCategories(target);
		await this.updateMethods(target);
		if (!keepSelections) {
			selections.variable = null;
			selections.category = null;
			selections.method = null;
		}
		this.applySelections(selections);
	};

	classExpanded = async (species) => {
		await this.updateSubclasses(species);
	};

	classDefined = async (species) => {
		var name = species.name;
		var side = "instance";
		if (name.endsWith(" class")) {
			name = name.slice(0, name.length - 6);
			side = "class";
		}
		if (!this.classnames.includes(name)) {
			this.classnames.push(name);
		}
		const instance = { name: name };
		await this.updateClass(instance, true);
		this.cache[name] = instance;
		const superclass = this.cache[instance.superclass];
		if (superclass && !superclass.subclasses.some((c) => c.name === name)) {
			superclass.subclasses.push(instance);
			superclass.subclasses.sort((a, b) => (a.name <= b.name ? -1 : 1));
		}
		const target = side === "instance" ? instance : instance.metaclass;
		await this.updateVariables(target, true);
		if (
			this.state.root == instance.name ||
			this.isInSubclasses(this.state.root, instance.superclass)
		) {
			this.classSelected(instance, false);
		} else {
			this.changeRootClass(instance.name);
		}
	};

	isInSubclasses(rootname, classname) {
		if (rootname === classname) {
			return true;
		}
		const root = this.cache[rootname];
		if (!root || !root.subclasses) {
			return false;
		}
		return root.subclasses.some((c) => {
			return c.name === classname || this.isInSubclasses(c, classname);
		});
	}

	classCommented = async (species) => {
		this.cache[species.name].comment = species.comment;
	};

	classRemoved = (species) => {
		delete this.cache[species.name];
		const superclass = this.cache[species.superclass];
		if (superclass) {
			superclass.subclasses = superclass.subclasses.filter(
				(c) => c.name !== species.name
			);
			this.classSelected(superclass);
		} else {
			this.changeRootClass("Object");
		}
	};

	classRenamed = (species) => {
		this.classSelected(species);
	};

	accessSelected = async (access) => {
		const selections = this.currentSelections();
		selections.access = access;
		const species = this.currentClass();
		const variable = selections.variable;
		await this.updateMethods(species, variable, access);
		this.applySelections(selections);
	};

	variableSelected = async (variable) => {
		const selections = this.currentSelections();
		selections.variable = variable;
		const species = this.currentClass();
		const access = selections.access;
		await this.updateMethods(species, variable, access);
		this.applySelections(selections);
	};

	variableAdded = async () => {
		const selections = this.currentSelections();
		const { species } = selections;
		await this.updateClass(species, true);
		const target = this.currentClass();
		await this.updateVariables(target, true);
		this.applySelections(selections);
	};

	variableRenamed = async (variable, newName) => {
		const selections = this.currentSelections();
		const { species, access } = selections;
		await this.updateClass(species, true);
		const target = this.currentClass();
		await this.updateVariables(target, true);
		const renamed = target.variables.find((v) => v.name === newName);
		selections.variable = renamed;
		await this.updateMethods(target, renamed, access, true);
		this.applySelections(selections);
	};

	variableRemoved = async () => {
		const selections = this.currentSelections();
		const { species } = selections;
		selections.variable = null;
		await this.updateClass(species, true);
		const target = this.currentClass();
		await this.updateVariables(target, true);
		await this.updateMethods(target);
		this.applySelections(selections);
	};

	categorySelected = async (category) => {
		const selections = this.currentSelections();
		selections.category = category;
		const { species, side } = selections;
		await this.updateMethods(species, side);
		this.applySelections(selections);
	};

	categoryAdded = async (category) => {
		const selections = this.currentSelections();
		selections.category = category;
		const species = this.currentClass();
		species.categories.push(category);
		species.categories.sort();
		this.applySelections(selections);
	};

	categoryRenamed = async (category, renamed) => {
		const selections = this.currentSelections();
		const species = this.currentClass();
		await this.updateCategories(species, true);
		await this.updateMethods(species, null, null, true);
		selections.category = species.categories.find((c) => c === renamed);
		this.applySelections(selections);
	};

	categoryRemoved = async (category) => {
		const selections = this.currentSelections();
		selections.category = null;
		const species = this.currentClass();
		await this.updateCategories(species, true);
		await this.updateMethods(species, null, null, true);
		this.applySelections(selections);
	};

	methodSelected = async (method) => {
		const selections = this.currentSelections();
		if (!method.template) {
			await this.updateMethod(method);
		}
		selections.method = method;
		this.applySelections(selections);
	};

	methodRenamed = async (method) => {
		const selections = this.currentSelections();
		const species = this.currentClass();
		await this.updateMethods(species, null, null, true);
		selections.method = species.methods.find(
			(m) => m.selector === method.selector
		);
		this.applySelections(selections);
	};

	methodRemoved = (method) => {
		const selections = this.currentSelections();
		const species = this.currentClass();
		species.methods = species.methods.filter(
			(m) => m.selector !== method.selector
		);
		selections.method = null;
		this.applySelections(selections);
	};

	methodClassified = (method) => {
		const selections = this.currentSelections();
		if (selections.category) {
			selections.category = method.category;
		}
		selections.method = method;
		this.applySelections(selections);
	};

	methodCompiled = async (method) => {
		if (!method) {
			return;
		}
		const selections = this.currentSelections();
		const target = this.currentClass();
		if (!target.categories.includes(method.category)) {
			await this.updateCategories(target, true);
		}
		selections.category = method.category;
		const methods = target.methods;
		const index = methods
			? methods.findIndex((m) => m.selector === method.selector)
			: -1;
		if (index === -1) {
			await this.updateMethods(target, null, null, true);
			selections.method = target.methods.find(
				(m) => m.selector === method.selector
			);
		} else {
			methods.splice(index, 1, method);
			selections.method = method;
		}
		this.applySelections(selections);
	};

	evaluationContext() {
		const species = this.currentClass();
		return species
			? {
					class: species.name,
			  }
			: {};
	}

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
										value={
											selectedClass
												? selectedClass.name
												: null
										}
										options={this.classnames}
										onChange={(classname) => {
											this.changeRootClass(classname);
										}}
									/>
								</Grid>
								<Grid item xs={1} md={1} lg={1}>
									<Tooltip
										title={
											rootclass
												? rootclass.superclass
												: ""
										}
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
								input={
									<OutlinedInput margin="dense" fullWidth />
								}
								onChange={(event) => {
									this.accessSelected(event.target.value);
								}}
							>
								<MenuItem value={"using"}>using</MenuItem>
								<MenuItem value={"assigning"}>
									assigning
								</MenuItem>
								<MenuItem value={"accessing"}>
									referencing
								</MenuItem>
							</Select>
						</Grid>
						<Grid item xs={3} md={3} lg={3}>
							<Box display="flex" justifyContent="center">
								<RadioGroup
									name="side"
									value={selectedSide}
									onChange={(event, side) =>
										this.sideChanged(side)
									}
									defaultValue="instance"
									row
								>
									<FormControlLabel
										value="instance"
										control={
											<Radio
												size="small"
												color="primary"
											/>
										}
										label="Instance"
									/>
									<FormControlLabel
										value="class"
										control={
											<Radio
												size="small"
												color="primary"
											/>
										}
										label="Class"
									/>
								</RadioGroup>
							</Box>
						</Grid>
						<Grid item xs={3} md={3} lg={3} />
						<Grid item xs={12} md={3} lg={3}>
							<Paper
								className={fixedHeightPaper}
								variant="outlined"
							>
								<ClassTree
									roots={rootclass ? [rootclass] : []}
									selected={selectedClass}
									onExpand={this.classExpanded}
									onSelect={this.classSelected}
									onRemove={this.classRemoved}
									onRename={this.classRenamed}
									onDefine={this.classDefined}
								/>
							</Paper>
						</Grid>
						<Grid item xs={12} md={3} lg={3}>
							<Paper
								className={fixedHeightPaper}
								variant="outlined"
							>
								<VariableList
									class={this.currentClass()}
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
							<Paper
								className={fixedHeightPaper}
								variant="outlined"
							>
								<CategoryList
									class={this.currentClass()}
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
							<Paper
								className={fixedHeightPaper}
								variant="outlined"
							>
								<MethodList
									methods={this.currentMethods()}
									categories={this.currentCategories()}
									selected={selectedMethod}
									onSelect={this.methodSelected}
									onRename={this.methodRenamed}
									onRemove={this.methodRemoved}
									onClassify={this.methodClassified}
									onCategoryAdd={this.categoryAdded}
									showNewOption
								/>
							</Paper>
						</Grid>
					</Grid>
				</Grid>
				<Grid item xs={12} md={12} lg={12}>
					<CodeBrowser
						context={this.evaluationContext()}
						styles={styles}
						class={this.currentClass()}
						method={selectedMethod}
						onCompileMethod={this.methodCompiled}
						onDefineClass={this.classDefined}
						onCommentClass={this.classCommented}
					/>
				</Grid>
			</Grid>
		);
	}
}

export default ClassBrowser;
