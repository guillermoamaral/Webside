import React from "react";
import {
	Grid,
	Box,
	RadioGroup,
	FormControlLabel,
	Radio,
	Select,
	MenuItem,
	OutlinedInput,
	IconButton,
	Tooltip,
} from "@mui/material";
import RefactoringBrowser from "./RefactoringBrowser";
import { ide } from "../IDE";
import ToolContainerContext from "../ToolContainerContext";
import SearchList2 from "../controls/SearchList2";
import ClassTree from "../parts/ClassTree";
import VariableList from "../parts/VariableList";
import CategoryList from "../parts/CategoryList";
import MethodList from "../parts/MethodList";
import CodeBrowser from "../parts/CodeBrowser";
import UpIcon from "@mui/icons-material/ArrowDropUp";
import CustomPaper from "../controls/CustomPaper";

class ClassBrowser extends RefactoringBrowser {
	static contextType = ToolContainerContext;

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
		await super.componentDidMount();
		await this.initializeClassNames();
	}

	async initializeClassNames() {
		try {
			this.classnames = await ide.backend.classNames();
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
			const species = await ide.backend.classTree(classsname, 10, true);
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
	currentVariables() {
		const species = this.currentClass();
		return species ? species.variables || [] : [];
	}

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
			const template = ide.backend.methodTemplate();
			template.methodClass = species.name;
			template.category = category;
			methods.push(template);
		}
		return methods;
	};

	// Updating...
	async updateVariables(species, forced = false) {
		try {
			if (forced || !species.variables) {
				species.variables = await ide.backend.variables(species.name);
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
				species.methods = await ide.backend.methods(species.name, true);
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
				const accessing = await ide.backend.methodsAccessing(
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
		this.context.updatePageLabel(this.props.id, species.name);
		const selections = this.currentSelections();
		selections.species = species;
		selections.category = null;
		await this.updateClass(species, true);
		await this.updateSubclasses(species);
		const target =
			selections.side === "instance" ? species : species.metaclass;
		await this.updateVariables(target, true);
		await this.updateCategories(target, true);
		await this.updateMethods(target);
		if (!keepSelections) {
			selections.variable = null;
			selections.category = null;
			selections.method = null;
		}
		this.applySelections(selections);
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
			this.state.root === instance.name ||
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
								size="small"
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
						<Grid item xs={3} md={3} lg={3}>
							<CustomPaper>
								<ClassTree
									roots={rootclass ? [rootclass] : []}
									selectedClass={selectedClass}
									onClassExpand={this.classExpanded}
									onClassSelect={this.classSelected}
									onClassRemove={this.classRemoved}
									onClassRename={this.classRenamed}
									onClassDefine={this.classDefined}
								/>
							</CustomPaper>
						</Grid>
						<Grid item xs={3} md={3} lg={3}>
							<CustomPaper>
								<VariableList
									class={this.currentClass()}
									variables={this.currentVariables()}
									selectedVariable={selectedVariable}
									onVariableAdd={this.variableAdded}
									onVariableRename={this.variableRenamed}
									onVariableSelect={this.variableSelected}
									onVariableRemove={this.variableRemoved}
								/>
							</CustomPaper>
						</Grid>
						<Grid item xs={3} md={3} lg={3}>
							<CustomPaper>
								<CategoryList
									class={this.currentClass()}
									categories={this.currentCategories()}
									usualCategories={this.usualCategories}
									usedCategories={this.currentUsedCategories()}
									selectedCategory={selectedCategory}
									highlightedCategory={
										selectedMethod
											? selectedMethod.category
											: null
									}
									onCategoryAdd={this.categoryAdded}
									onCategoryRename={this.categoryRenamed}
									onCategorySelect={this.categorySelected}
									onCategoryRemove={this.categoryRemoved}
								/>
							</CustomPaper>
						</Grid>
						<Grid item xs={3} md={3} lg={3}>
							<CustomPaper>
								<MethodList
									methods={this.currentMethods()}
									selectedMethod={selectedMethod}
									categories={this.currentCategories()}
									usualCategories={this.usualCategories}
									usedCategories={this.currentUsedCategories()}
									onMethodSelect={this.methodSelected}
									onMethodRename={this.methodRenamed}
									onMethodRemove={this.methodRemoved}
									onMethodClassify={this.methodClassified}
									onCategoryAdd={this.categoryAdded}
									showNewOption
								/>
							</CustomPaper>
						</Grid>
					</Grid>
				</Grid>
				<Grid item xs={12} md={12} lg={12}>
					<CodeBrowser
						context={this.evaluationContext()}
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
