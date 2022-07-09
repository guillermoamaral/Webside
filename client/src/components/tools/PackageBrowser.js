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

	methodTemplate() {
		// Retrieve from back-end to support custom templates for each dialect...
		return {
			selector: "<new>",
			source: 'messagePattern\r\t"comment"\r\t| temporaries |\r\tstatements',
		};
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

	// Selections...
	currentSelections() {
		return {
			package: this.state.selectedPackage,
			species: this.state.selectedClass,
			category: this.state.selectedCategory,
			method: this.state.selectedMethod,
			side: this.state.selectedSide,
		};
	}

	applySelections(selections) {
		this.reviseSelections(selections);
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
				selectedSide: selections.side,
			};
		});
	}

	reviseSelections(selections) {
		const { category, side, method } = selections;
		const species = selections.species
			? side === "instance"
				? selections.species
				: selections.species.metaclass
			: null;
		if (!species) {
			selections.category = null;
			selections.method = null;
		} else {
			if (category) {
				selections.category = species.categories.find((c) => c === category);
			}
			if (method) {
				selections.method = species.methods.find(
					(m) => m.selector === method.selector
				);
				if (selections.method) {
					if (
						selections.category &&
						selections.method.category !== selections.category
					) {
						selections.method = null;
					}
				}
			}
		}
	}

	// Contents...
	currentClasses() {
		const pack = this.state.selectedPackage;
		return !pack ? [] : pack.classes;
	}

	currentClass() {
		const { selectedClass, selectedSide } = this.state;
		if (!selectedClass) return null;
		return selectedSide === "instance"
			? selectedClass
			: selectedClass.metaclass;
	}

	currentCategories = () => {
		const species = this.currentClass();
		return species ? species.categories || [] : [];
	};

	currentMethods = () => {
		const pack = this.state.selectedPackage;
		const species = this.currentClass();
		const category = this.state.selectedCategory;
		if (!pack || !species) {
			return [];
		}
		var methods = species.methods.filter((m) => (m.package = pack.name));
		if (category) {
			methods = methods.filter((m) => m.category === category);
		}
		if (methods && methods.length === 0) {
			const template = this.methodTemplate();
			template.class = species;
			template.category = category;
			methods.push(template);
		}
		return methods;
	};

	// Updating...
	async updatePackage(pack, force = false) {
		try {
			if (force || !pack.classes || !pack.methods) {
				const retrieved = await this.context.api.getPackage(pack.name);
				Object.assign(pack, retrieved);
			}
		} catch (error) {
			this.context.reportError(error);
		}
	}

	async updateClasses(pack, force = false) {
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

	async updateClass(species, force = false) {
		try {
			if (force || !species.definition || !species.metaclass) {
				const definition = await this.context.api.getClass(species.name);
				Object.assign(species, definition);
				species.metaclass = await this.context.api.getClass(definition.class);
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

	async updateCategories(species, force = false) {
		try {
			if (force || !species.categories) {
				species.categories = await this.context.api.getCategories(species.name);
				species.categories.sort();
			}
		} catch (error) {
			this.context.reportError(error);
		}
	}

	async updateMethods(species, force = false) {
		if (!species) {
			return;
		}
		try {
			if (force || !species.methods) {
				species.methods = await this.context.api.getMethods(species.name, true);
			}
		} catch (error) {
			this.context.reportError(error);
		}
	}

	async updateMethod(method) {
		try {
			const retrieved = await this.context.api.getMethod(
				method.class,
				method.selector
			);
			Object.assign(method, retrieved);
		} catch (error) {
			this.context.reportError(error);
		}
	}

	// Events...
	sideChanged = async (side) => {
		const selections = this.currentSelections();
		selections.side = side;
		const species =
			side === "instance" ? selections.species : selections.species.metaclass;
		await this.updateCategories(species);
		await this.updateMethods(species);
		this.applySelections(selections);
	};

	packageSelected = async (pack) => {
		const selections = this.currentSelections();
		selections.package = pack;
		await this.updatePackage(pack, true);
		await this.updateClasses(pack, true);
		this.applySelections(selections);
	};

	classSelected = async (species) => {
		// this.context.updatePageLabel(this.props.id, species.name);
		const selections = this.currentSelections();
		selections.species = species;
		await this.updateClass(species);
		await this.updateSubclasses(species);
		const target = selections.side === "instance" ? species : species.metaclass;
		await this.updateCategories(target);
		await this.updateMethods(target);
		this.applySelections(selections);
	};

	classExpanded = async (species) => {
		await this.updateSubclasses(species);
	};

	classLabelStyle = (species) => {
		const pack = this.state.selectedPackage;
		return pack &&
			pack.methods &&
			(pack.methods[species.name] || pack.methods[species.name + " class"])
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
		var name = species.name;
		var side = "instance";
		if (name.endsWith(" class")) {
			name = name.slice(0, name.length - 6);
			side = "class";
		}
		const instance = { name: name };
		await this.updateClass(instance, true);
		this.cache.classes[name] = instance;
		const superclass = this.cache.classes[instance.superclass];
		if (superclass && !superclass.subclasses.some((c) => c.name === name)) {
			superclass.subclasses.push(instance);
			superclass.subclasses.sort((a, b) => (a.name <= b.name ? -1 : 1));
		}
		this.classSelected(instance);
	};

	classCommented = async (species) => {
		this.cache.classes[species.name].comment = species.comment;
	};

	classRemoved = (species) => {
		delete this.cache.classes[species.name];
		const superclass = this.cache.classes[species.superclass];
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
		await this.updateMethod(method);
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

	render() {
		const {
			packages,
			selectedPackage,
			selectedSide,
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
									onChange={(event, side) => this.sideChanged(side)}
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
						onCompileMethod={this.methodCompiled}
						onDefineClass={this.classDefined}
						onCommentClass={this.classCommented}
					/>
				</Grid>
			</Grid>
		);
	}
}

export default PackageBrowser;
