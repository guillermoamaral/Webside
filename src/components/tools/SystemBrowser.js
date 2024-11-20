import React from "react";
import Tool from "./Tool";
import {
	Box,
	Select,
	MenuItem,
	OutlinedInput,
	RadioGroup,
	FormControlLabel,
	Radio,
} from "@mui/material";
import CustomSplit from "../controls/CustomSplit";
import { ide } from "../IDE";
import PackageTree from "../parts/PackageTree";
import ClassTree from "../parts/ClassTree";
import VariableList from "../parts/VariableList";
import CategoryList from "../parts/CategoryList";
import MethodList from "../parts/MethodList";
import CodeBrowser from "../parts/CodeBrowser";

class SystemBrowser extends Tool {
	constructor(props) {
		super(props);
		this.classTreeRef = React.createRef();
		this.categoryListRef = React.createRef();
		this.methodListRef = React.createRef();
		this.state = {
			roots: [{ name: "Object" }],
			selectedSide: "instance",
			selectedPackage: null,
			selectedClassCategory: null,
			selectedClass: null,
			selectedVariable: null,
			selectedCategory: null,
			selectedAccess: "accessing",
			selectedMethod: null,
			preselectedClass: null,
			preselectedPackage: null,
		};
	}

	static getDerivedStateFromProps(props, state) {
		if (
			props.preselectedClass &&
			state.roots.length === 1 &&
			state.roots[0].name !== props.preselectedClass.name
		) {
			return {
				roots: [{ name: props.preselectedClass.name }],
				selectedClass: props.preselectedClass,
				preselectedClass: props.preselectedClass,
				selectedSide: props.side || "instance",
			};
		}
		if (
			props.showPackages &&
			props.preselectedPackage &&
			(!state.preselectedPackage ||
				state.preselectedPackage.name !== props.preselectedPackage.name)
		) {
			return {
				selectedPackage: props.preselectedPackage,
				preselectedPackage: props.preselectedPackage,
			};
		}
		return null;
	}

	async updateClass(species) {
		if (!species) return;
		if (species.template) return;
		try {
			let retrieved = await ide.backend.classNamed(species.name);
			Object.assign(species, retrieved);
			species.metaclass = await ide.backend.classNamed(species.class);
		} catch (error) {
			ide.reportError(error);
		}
	}

	async updatePackage(pack) {
		try {
			let retrieved = await ide.backend.packageNamed(pack.name);
			Object.assign(pack, retrieved);
		} catch (error) {
			ide.reportError(error);
		}
	}

	targetClass() {
		let { selectedSide, selectedClass } = this.state;
		return selectedClass
			? selectedSide === "instance"
				? selectedClass
				: selectedClass.metaclass
			: null;
	}

	evaluationContext() {
		const species = this.targetClass();
		return species ? { class: species.name } : {};
	}

	// Events...

	packageSelected = async (pack) => {
		this.updateLabel(pack.name);
		await this.updatePackage(pack);
		let {
			selectedClass,
			selectedCategory,
			selectedVariable,
			selectedMethod,
		} = this.state;
		if (selectedClass && selectedClass.package !== pack.name) {
			selectedClass =
				selectedCategory =
				selectedVariable =
				selectedMethod =
					null;
		}
		this.setState({
			selectedPackage: pack,
			selectedClassCategory: null,
			selectedClass: selectedClass,
			selectedCategory: selectedCategory,
			selectedVariable: selectedVariable,
			selectedMethod: selectedMethod,
		});
	};

	classCategorySelected = async (category, pack) => {
		let {
			selectedClass,
			selectedCategory,
			selectedVariable,
			selectedMethod,
		} = this.state;
		if (
			selectedClass &&
			(selectedClass.package !== pack.name ||
				selectedClass.category !== category.name)
		) {
			selectedClass =
				selectedCategory =
				selectedVariable =
				selectedMethod =
					null;
		}
		this.setState({
			selectedPackage: pack,
			selectedClassCategory: category,
			selectedClass: selectedClass,
			selectedCategory: selectedCategory,
			selectedVariable: selectedVariable,
			selectedMethod: selectedMethod,
		});
	};

	packageCreated = async (pack) => {
		this.updateLabel(pack.name);
		this.setState({ selectedPackage: pack });
	};

	sideSelected = async (side) => {
		await this.updateClass(this.state.selectedClass);
		this.setState({
			selectedSide: side,
			selectedCategory: null,
			selectedVariable: null,
			selectedMethod: null,
		});
	};

	classSelected = async (species) => {
		this.updateLabel(species.name);
		await this.updateClass(species);
		const { selectedMethod, selectedCategory, selectedVariable } =
			this.state;
		let method;
		if (selectedMethod) {
			try {
				method = await ide.backend.method(
					species.name,
					selectedMethod.selector
				);
			} catch (ignored) {}
		}
		let category;
		if (selectedCategory) {
			try {
				const categories = await ide.backend.categories(species.name);
				category = categories.find((c) => c === selectedCategory);
			} catch (ignored) {}
		}
		let variable;
		if (selectedVariable) {
			try {
				const variables = await ide.backend.variables(species.name);
				variable = variables.find(
					(v) =>
						v.name === selectedVariable.name &&
						v.type === selectedVariable.type
				);
			} catch (ignored) {}
		}
		this.setState({
			selectedClass: species,
			selectedCategory: category,
			selectedVariable: variable,
			selectedMethod: method,
		});
	};

	classDefined = async (species) => {
		await this.updateClass(species);
		this.updateLabel(species.name);
		if (this.props.showPackages) {
			const pack = this.state.selectedPackage;
			if (pack && pack.name === species.package) {
				await this.updatePackage(pack);
			}
		}
		const ref = this.classTreeRef;
		if (ref && ref.current) {
			ref.current.refreshEnsuring(species);
		}
		this.setState({ selectedClass: species });
	};

	classCommented = async (species) => {
		this.setState({ selectedClass: species });
	};

	classRenamed = async (species) => {
		this.classDefined(species);
		const ref = this.methodListRef;
		if (ref && ref.current) {
			ref.current.refreshEnsuring();
		}
	};

	classRemoved = async (species) => {
		this.setState({ selectedClass: null });
	};

	accessSelected = async (access) => {
		this.setState({ selectedAccess: access });
	};

	variableSelected = (variable) => {
		this.setState({ selectedVariable: variable, selectedMethod: null });
	};

	variableAdded = async (variable) => {
		let species = this.state.selectedClass;
		await this.updateClass(species);
		this.setState({
			selectedClass: species,
			selectedCategory: null,
			selectedVariable: variable,
			selectedMethod: null,
		});
	};

	variableRenamed = async (variable) => {
		let species = this.state.selectedClass;
		await this.updateClass(species);
		this.setState({
			selectedClass: species,
			selectedCategory: null,
			selectedVariable: variable,
			selectedMethod: null,
		});
	};

	variableRemoved = async (variable) => {
		let species = this.state.selectedClass;
		await this.updateClass(species);
		this.setState({
			selectedClass: species,
			selectedCategory: null,
			selectedVariable: null,
			selectedMethod: null,
		});
	};

	categorySelected = (category) => {
		const method = this.state.selectedMethod;
		const selected =
			!category || (method && method.category === category)
				? method
				: null;
		this.setState({ selectedCategory: category, selectedMethod: selected });
	};

	categoryAdded = (category) => {
		this.setState({ selectedCategory: category });
	};

	categoryRenamed = (category) => {
		this.setState({ selectedCategory: category });
	};

	categoryRemoved = (category) => {
		this.setState({ selectedCategory: null });
	};

	async updateMethod(method) {
		try {
			const retrieved = await ide.backend.method(
				method.methodClass,
				method.selector
			);
			if (retrieved) {
				Object.assign(method, retrieved);
			} else {
				method.source = "method cannot be found";
				method.ast = null;
				method.bytecodes = null;
			}
		} catch (error) {
			ide.reportError(error);
		}
	}

	methodSelected = async (method) => {
		if (method) {
			this.updateLabel(method.methodClass + ">>" + method.selector);

			if (!method.template) await this.updateMethod(method);
		}
		this.setState({ selectedMethod: method });
	};

	methodRenamed = async (method) => {
		this.methodSelected(method);
	};

	methodRemoved = (method) => {
		this.setState({ selectedMethod: null });
	};

	methodCompiled = async (method) => {
		if (!method) return;
		if (method) {
			this.updateLabel(method.methodClass + ">>" + method.selector);
		}
		let ref = this.methodListRef;
		if (ref && ref.current) {
			ref.current.refreshEnsuring(method);
		}
		ref = this.categoryListRef;
		if (ref && ref.current) {
			ref.current.refreshEnsuring(method.category);
		}
		this.setState({ selectedMethod: method });
	};

	methodClassified = async (method) => {
		if (!method) return;
		const ref = this.categoryListRef;
		if (ref && ref.current) {
			ref.current.refreshEnsuring(method.category);
		}
		this.setState({ selectedCategory: method.category });
	};

	codeExtendedOptionPerformed = async () => {
		const ref = this.methodListRef;
		const method = this.state.selectedMethod;
		if (ref && ref.current) {
			ref.current.refreshEnsuring(method);
		}
		await this.updateMethod(method);
		this.setState({ selectedMethod: method });
	};

	render() {
		const {
			roots,
			selectedPackage,
			selectedClassCategory,
			selectedSide,
			selectedVariable,
			selectedAccess,
			selectedCategory,
			selectedMethod,
			preselectedClass,
			preselectedPackage,
		} = this.state;
		const targetClass = this.targetClass();
		const showPackages = this.props.showPackages;
		const width = showPackages ? "20%" : "25%";
		const background = showPackages
			? ide.colorSetting("systemBrowserColor")
			: ide.colorSetting("classBrowserColor");
		return (
			<Box
				sx={{
					height: "100%",
					background: background,
					padding: 1,
				}}
			>
				<CustomSplit mode="vertical">
					<Box sx={{ minHeight: 50, height: "35%" }}>
						<Box
							display="flex"
							flexDirection="row"
							width="100%"
							height="100%"
						>
							<CustomSplit>
								{showPackages && (
									<Box sx={{ width: width }}>
										<PackageTree
											onPackageSelect={
												this.packageSelected
											}
											onPackageCreate={
												this.packageCreated
											}
											selectedPackage={preselectedPackage}
											onCategorySelect={
												this.classCategorySelected
											}
										/>
									</Box>
								)}
								<Box sx={{ width: width }}>
									<ClassTree
										ref={this.classTreeRef}
										roots={!showPackages ? roots : null}
										package={
											showPackages
												? selectedPackage
												: null
										}
										category={
											showPackages
												? selectedClassCategory
												: null
										}
										onClassSelect={this.classSelected}
										onClassDefine={this.classDefined}
										onClassRename={this.classRenamed}
										onClassRemove={this.classRemoved}
										labelColor={this.classLabelColor}
										selectedClass={preselectedClass}
									/>
								</Box>
								<Box
									display="flex"
									flexDirection="column"
									sx={{ height: "100%", width: width }}
								>
									<Box>
										<Select
											size="small"
											value={selectedAccess}
											input={
												<OutlinedInput
													margin="dense"
													fullWidth
												/>
											}
											onChange={(event) => {
												this.accessSelected(
													event.target.value
												);
											}}
										>
											<MenuItem value={"using"}>
												using
											</MenuItem>
											<MenuItem value={"assigning"}>
												assigning
											</MenuItem>
											<MenuItem value={"accessing"}>
												referencing
											</MenuItem>
										</Select>
									</Box>
									<Box mt={1} flexGrow={1}>
										<VariableList
											class={targetClass}
											onVariableSelect={
												this.variableSelected
											}
											onVariableAdd={this.variableAdded}
											onVariableRename={
												this.variableRenamed
											}
											onVariableRemove={
												this.variableRemoved
											}
										/>
									</Box>
								</Box>
								<Box
									display="flex"
									flexDirection="column"
									sx={{ height: "100%", width: width }}
								>
									<Box display="flex" justifyContent="center">
										<RadioGroup
											name="side"
											value={selectedSide}
											onChange={(event, side) =>
												this.sideSelected(side)
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
									<Box mt={1.2} flexGrow={1}>
										<CategoryList
											ref={this.categoryListRef}
											class={targetClass}
											onCategorySelect={
												this.categorySelected
											}
											highlightedCategory={
												selectedMethod
													? selectedMethod.category
													: null
											}
											onCategoryAdd={this.categoryAdded}
											onCategoryRename={
												this.categoryRenamed
											}
											onCategoryRemove={
												this.categoryRemoved
											}
										/>
									</Box>
								</Box>
								<Box sx={{ width: width }}>
									<MethodList
										ref={this.methodListRef}
										package={
											showPackages
												? selectedPackage
												: null
										}
										class={targetClass}
										category={selectedCategory}
										access={selectedAccess}
										variable={selectedVariable}
										onMethodSelect={this.methodSelected}
										onMethodRename={this.methodRenamed}
										onMethodRemove={this.methodRemoved}
										onMethodClassify={this.methodClassified}
										onCategoryAdd={this.categoryAdded}
										showNewOption
									/>
								</Box>
							</CustomSplit>
						</Box>
					</Box>
					<Box sx={{ height: "60%" }}>
						<CodeBrowser
							context={this.evaluationContext()}
							category={selectedCategory}
							package={showPackages ? selectedPackage : null}
							class={targetClass}
							method={selectedMethod}
							onMethodCompile={this.methodCompiled}
							onClassDefine={this.classDefined}
							onClassComment={this.classCommented}
							onExtendedOptionPerform={
								this.codeExtendedOptionPerformed
							}
							sx={{ height: "100%" }}
						/>
					</Box>
				</CustomSplit>
			</Box>
		);
	}
}

export default SystemBrowser;
