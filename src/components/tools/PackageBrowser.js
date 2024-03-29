import React from "react";
import {
	Grid,
	Box,
	RadioGroup,
	FormControlLabel,
	Radio,
	TextField,
} from "@mui/material";
import RefactoringBrowser from "./RefactoringBrowser";
import { ide } from "../IDE";
import PackageList from "../parts/PackageList";
import ClassTree from "../parts/ClassTree";
import CategoryList from "../parts/CategoryList";
import MethodList from "../parts/MethodList";
import CodeBrowser from "../parts/CodeBrowser";
import CustomPaper from "../controls/CustomPaper";
import CustomSplit from "../controls/CustomSplit";

class PackageBrowser extends RefactoringBrowser {
	constructor(props) {
		super(props);
		this.cache = { packages: {}, classes: {} };
		this.state = {
			packageFilter: "",
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
			names = await ide.backend.packageNames();
		} catch (error) {
			names = [];
			ide.reportError(error);
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
				selections.category = species.categories.find(
					(c) => c === category
				);
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

	currentMethods = () => {
		const pack = this.state.selectedPackage;
		const species = this.currentClass();
		const category = this.state.selectedCategory;
		if (!pack || !species || !species.methods) {
			return [];
		}
		var methods = species.methods.filter((m) => (m.package = pack.name));
		if (category) {
			methods = methods.filter((m) => m.category === category);
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
	async updatePackage(pack, forced = false) {
		try {
			if (forced || !pack.classes || !pack.methods) {
				const retrieved = await ide.backend.packageNamed(pack.name);
				Object.assign(pack, retrieved);
			}
		} catch (error) {
			ide.reportError(error);
		}
	}

	async updateClasses(pack, forced = false) {
		if (forced || !pack.classes) {
			try {
				pack.classes = await ide.backend.packageClasses(
					pack.name,
					true
				);
			} catch (error) {
				ide.reportError(error);
			}
		}
	}

	async updateMethods(species, forced = false) {
		if (!species) {
			return;
		}
		try {
			if (forced || !species.methods) {
				species.methods = await ide.backend.methods(species.name, true);
			}
		} catch (error) {
			ide.reportError(error);
		}
	}

	// Events...
	sideChanged = async (side) => {
		const selections = this.currentSelections();
		selections.side = side;
		selections.side = side;
		var species = selections.species;
		if (species && side === "class") {
			species = selections.species.metaclass;
		}
		if (species) {
			await this.updateCategories(species);
			await this.updateMethods(species);
		}
		this.applySelections(selections);
	};

	packageSelected = async (pack) => {
		this.context.updatePageLabel(this.props.id, pack.name);
		const selections = this.currentSelections();
		selections.package = pack;
		await this.updatePackage(pack, true);
		await this.updateClasses(pack, true);
		this.applySelections(selections);
	};

	packageCreated = async (pack) => {
		await this.updatePackage(pack, true);
		this.cache.packages[pack.name] = pack;
		const packages = this.state.packages;
		packages.push(pack);
		packages.sort((a, b) => (a.name <= b.name ? -1 : 1));
		this.setState({
			packages: packages,
			selectedPackage: pack,
			selectedClass: null,
			selectedCategory: null,
			selectedMethod: null,
			selectedSide: null,
		});
	};

	packageRemoved = (pack) => {
		delete this.cache.packages[pack.name];
		const packages = this.state.packages;
		const index = packages.indexOf(pack);
		if (index > -1) {
			packages.splice(index, 1);
			const selected =
				index - 1 >= 0
					? packages[index - 1]
					: index + 1 < packages.length
					? packages[index + 1]
					: null;
			this.setState({
				packages: packages,
				selectedPackage: selected,
				selectedClass: null,
				selectedCategory: null,
				selectedMethod: null,
				selectedSide: null,
			});
		}
	};

	packageRenamed = (pack) => {
		this.packageSelected(pack);
	};

	classSelected = async (species) => {
		this.context.updatePageLabel(this.props.id, species.name);
		const selections = this.currentSelections();
		selections.species = species;
		selections.category = null;
		await this.updateClass(species);
		await this.updateSubclasses(species);
		const target =
			selections.side === "instance" ? species : species.metaclass;
		await this.updateCategories(target);
		await this.updateMethods(target);
		this.applySelections(selections);
	};

	classLabelStyle = (species) => {
		const pack = this.state.selectedPackage;
		return pack &&
			pack.methods &&
			(pack.methods[species.name] ||
				pack.methods[species.name + " class"])
			? "italic"
			: "normal";
	};

	methodLabelStyle = (method) => {
		const pack = this.state.selectedPackage;
		return pack &&
			pack.methods &&
			pack.methods[method.methodClass] &&
			pack.methods[method.methodClass].includes(method.selector)
			? "italic"
			: "normal";
	};

	classDefined = async (species) => {
		var name = species.name;
		if (name.endsWith(" class")) {
			name = name.slice(0, name.length - 6);
		}
		const instance = { name: name };
		await this.updateClass(instance, true);
		this.cache.classes[name] = instance;
		const superclass = this.cache.classes[instance.superclass];
		if (superclass && !superclass.subclasses.some((c) => c.name === name)) {
			superclass.subclasses.push(instance);
			superclass.subclasses.sort((a, b) => (a.name <= b.name ? -1 : 1));
		}
		const selections = this.currentSelections();
		const pack = selections.package;
		selections.species = instance;
		await this.updateClasses(pack, true);
		await this.updateSubclasses(instance);
		const target =
			selections.side === "instance" ? instance : instance.metaclass;
		await this.updateCategories(target);
		await this.updateMethods(target);
		this.applySelections(selections);
	};

	classCommented = async (species) => {
		this.cache.classes[species.name].comment = species.comment;
	};

	classRemoved = async (species) => {
		delete this.cache.classes[species.name];
		const superclass = this.cache.classes[species.superclass];
		if (superclass) {
			superclass.subclasses = superclass.subclasses.filter(
				(c) => c.name !== species.name
			);
		}
		const selections = this.currentSelections();
		const pack = selections.package;
		await this.updateClasses(pack, true);
		selections.species = null;
		this.applySelections(selections);
	};

	render() {
		const {
			packageFilter,
			packages,
			selectedPackage,
			selectedSide,
			selectedClass,
			selectedCategory,
			selectedMethod,
		} = this.state;
		const prefix = packageFilter.toLowerCase();
		const filtered =
			prefix !== ""
				? packages.filter((p) => {
						return p.name.toLowerCase().startsWith(prefix);
				  })
				: packages;
		return (
			<Box display="flex" flexDirection="column" sx={{ height: "100%" }}>
				<Box mb={1}>
					<Grid container spacing={1}>
						<Grid item xs={3} md={3} lg={3}>
							<TextField
								value={packageFilter}
								onChange={(event) =>
									this.setState({
										packageFilter: event.target.value,
									})
								}
								placeholder="Filter ..."
								name="text"
								variant="outlined"
								fullWidth
								margin="dense"
								autoFocus
								type="text"
								size="small"
							/>
						</Grid>
						<Grid item xs={3} md={3} lg={3} />
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
					</Grid>
				</Box>
				<Box flexGrow={1}>
					<CustomSplit mode="vertical">
						<Box sx={{ minHeight: 50, height: "35%" }}>
							<Grid container spacing={1} sx={{ height: "100%" }}>
								<Grid item xs={3} md={3} lg={3}>
									<CustomPaper>
										<PackageList
											packages={filtered}
											selectedPackage={selectedPackage}
											onPackageSelect={
												this.packageSelected
											}
											onPackageRemove={
												this.packageRemoved
											}
											onPackageRename={
												this.packageRenamed
											}
											onPackageCreate={
												this.packageCreated
											}
										/>
									</CustomPaper>
								</Grid>
								<Grid item xs={3} md={3} lg={3}>
									<CustomPaper>
										<ClassTree
											roots={
												selectedPackage
													? selectedPackage.classes
													: []
											}
											selectedClass={selectedClass}
											labelStyle={this.classLabelStyle}
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
										<CategoryList
											class={selectedClass}
											categories={this.currentCategories()}
											usualCategories={this.currentUsualCategories()}
											usedCategories={this.currentUsedCategories()}
											selectedCategory={selectedCategory}
											highlightedCategory={
												selectedMethod
													? selectedMethod.category
													: null
											}
											onCategoryAdd={this.categoryAdded}
											onCategoryRename={
												this.categoryRenamed
											}
											onCategorySelect={
												this.categorySelected
											}
											onCategoryRemove={
												this.categoryRemoved
											}
										/>
									</CustomPaper>
								</Grid>
								<Grid item xs={3} md={3} lg={3}>
									<CustomPaper>
										<MethodList
											methods={this.currentMethods()}
											selectedMethod={selectedMethod}
											categories={this.currentCategories()}
											usualCategories={this.currentUsualCategories()}
											usedCategories={this.currentUsedCategories()}
											labelStyle={this.methodLabelStyle}
											onMethodSelect={this.methodSelected}
											onMethodRename={this.methodRenamed}
											onMethodRemove={this.methodRemoved}
											onMethodClassify={
												this.methodClassified
											}
											showNewOption
										/>
									</CustomPaper>
								</Grid>
							</Grid>
						</Box>
						<Box sx={{ height: "60%" }}>
							<CodeBrowser
								context={this.evaluationContext()}
								package={selectedPackage}
								class={selectedClass}
								method={selectedMethod}
								onMethodCompile={this.methodCompiled}
								onClassDefine={this.classDefined}
								onClassComment={this.classCommented}
							/>
						</Box>
					</CustomSplit>
				</Box>
			</Box>
		);
	}
}

export default PackageBrowser;
