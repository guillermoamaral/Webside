import React from "react";
import Tool from "./Tool";
import {
	Grid,
	Box,
	Paper,
	RadioGroup,
	FormControlLabel,
	Radio,
	Accordion,
	AccordionSummary,
	Typography,
} from "@mui/material";
import { ide } from "../IDE";
import SearchList2 from "../controls/SearchList2";
import ClassTree from "../parts/ClassTree";
import ExpandMoreIcon from "@mui/icons-material/ExpandMore";
import CodeEditor from "../parts/CodeEditor";

class CoderLikeBrowser extends Tool {
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
			visibleMethods: {},
		};
	}

	componentDidMount() {
		const root = this.state.root;
		if (root) {
			this.changeRootClass(root);
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

	currentSelections() {
		return {
			species: this.state.selectedClass,
			access: this.state.selectedAccess,
			variable: this.state.selectedVariable,
			category: this.state.selectedCategory,
			method: this.state.selectedMethod,
		};
	}

	applySelections(selections) {
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
			};
		});
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
		if (variable && species[variable.name]) {
			const accessors = species[variable.name][access];
			methods = methods.filter((m) =>
				accessors.some((n) => n.selector === m.selector)
			);
		}
		if (methods.length === 0) {
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
	async updateClass(selections, forced = false) {
		const species = selections.species;
		try {
			if (forced || !species.definition) {
				const definition = await ide.backend.classNamed(species.name);
				species.definition = definition.definition;
				species.comment = definition.comment;
				species.superclass = definition.superclass;
			}
			if (forced || !species.subclasses) {
				species.subclasses = await ide.backend.subclasses(species.name);
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
							c.subclasses = await ide.backend.subclasses(c.name);
						}
					})
				);
			}
		} catch (error) {
			ide.reportError(error);
		}
	}

	async updateVariables(selections, forced = false) {
		const { species, variable } = selections;
		try {
			if (forced || !species.variables) {
				species.variables = await ide.backend.variables(species.name);
			}
			if (variable) {
				const found = species.variables.find(
					(v) => v.name === variable.name
				);
				selections.variable = !found ? null : found;
			}
		} catch (error) {
			ide.reportError(error);
		}
	}

	async updateCategories(selections, forced = false) {
		const species = selections.species;
		try {
			if (forced || !species.categories) {
				const categories = await ide.backend.categories(species.name);
				species.categories = categories.sort();
			}
			if (!species.categories.includes(selections.category)) {
				selections.category = null;
			}
		} catch (error) {
			ide.reportError(error);
		}
	}

	async updateMethods(selections, forced = false) {
		const { species, variable, access, method } = selections;
		if (!species) {
			return;
		}
		try {
			if (forced || !species.methods) {
				species.methods = await ide.backend.methods(species.name, true);
			}
			if (
				variable &&
				(forced ||
					!species[variable.name] ||
					!species[variable.name][access])
			) {
				const accessors = await ide.backend.accessors(
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
			ide.reportError(error);
		}
	}

	async updateMethod(selections, forced = true) {
		const species = selections.species;
		const selector = selections.method.selector;
		var method;
		if (forced) {
			try {
				method = await ide.backend.method(species.name, selector);
			} catch (error) {
				ide.reportError(error);
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
	sideChanged = (event, side) => {
		if (!side) return;
		this.setState({ selectedSide: side });
		if (!this.state.root) {
			return;
		}
		if (side === "instance") {
			const name = this.state.root;
			this.changeRootClass(name.slice(0, name.length - 6));
		} else {
			this.changeRootClass(this.state.root + " class");
		}
	};

	classSelected = async (species) => {
		const selections = this.currentSelections();
		selections.species = species;
		await this.updateClass(selections, true);
		await this.updateSubclasses(species);
		await this.updateVariables(selections, true);
		await this.updateCategories(selections, true);
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
				superclass.subclasses.sort((a, b) =>
					a.name <= b.name ? -1 : 1
				);
			}
		}
		const selections = this.currentSelections();
		selections.species = cached;
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
		if (!method.template) {
			await this.updateMethod(method);
		}
		selections.method = method;
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
		this.cache[method.methodClass].methods = this.cache[
			method.methodClass
		].methods.filter((m) => m.selector !== method.selector);
		this.setState({ selectedMethod: null });
	};

	methodCompiled = async (method) => {
		if (!method) {
			return;
		}
		const selections = this.currentSelections();
		const species = this.cache[method.methodClass];
		selections.species = species;
		if (!species.categories.includes(method.category)) {
			await this.updateCategories(selections, true);
		}
		selections.category = species.categories.find(
			(c) => c === method.category
		);
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

	showMethod = (method) => {
		const visible = this.state.visibleMethods;
		visible[method.selector] = !(visible[method.selector] || false);
		this.setState({ visibleMethods: visible });
	};

	render() {
		const { root, selectedSide, selectedClass, visibleMethods } =
			this.state;
		return (
			<Grid container spacing={1}>
				<Grid item xs={3} md={3} lg={3}>
					<SearchList2
						options={[]}
						onChange={(classname) => {
							this.changeRootClass(classname);
						}}
					/>
				</Grid>
				<Grid item xs={9} md={9} lg={9}>
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
				<Grid item xs={3} md={3} lg={3}>
					<Paper style={{ height: "100%" }} variant="outlined">
						<ClassTree
							roots={
								root
									? this.cache[root]
										? [this.cache[root]]
										: []
									: []
							}
							selectedClass={selectedClass}
							onClassExpand={this.classExpanded}
							onClassSelect={this.classSelected}
							onClassRemove={this.classRemoved}
							onClassRename={this.classRenamed}
							onClassDefine={this.classDefined}
						/>
					</Paper>
				</Grid>
				<Grid item xs={9} md={9} lg={9}>
					<Paper variant="outlined">
						{this.currentMethods().map((m) => {
							const visible = visibleMethods[m.selector] || false;
							return (
								<Accordion key={m.selector}>
									<AccordionSummary
										expandIcon={<ExpandMoreIcon />}
										onClick={() => this.showMethod(m)}
									>
										<Box
											display="flex"
											flexWrap="nowrap"
											alignItems="center"
											justifyContent="center"
										></Box>
										<Box>
											<Typography>
												{m.selector}
											</Typography>
										</Box>
									</AccordionSummary>
									{visible && (
										<CodeEditor
											lineNumbers={true}
											source={m.source}
											mode="smalltalk-method"
											showAccept
											onAccept={this.acceptClicked}
											onRename={(target) =>
												this.renameClass(target)
											}
										/>
									)}
								</Accordion>
							);
						})}
					</Paper>
				</Grid>
			</Grid>
		);
	}
}

export default CoderLikeBrowser;
