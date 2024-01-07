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
import CustomPaper from "../controls/CustomPaper";
import { ide } from "../IDE";
import UPackageList from "../parts/UPackageList";
import UClassTree from "../parts/UClassTree";
import UVariableList from "../parts/UVariableList";
import UCategoryList from "../parts/UCategoryList";
import UMethodList from "../parts/UMethodList";
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
		if (props.preselectedClass && state.roots.length === 1 && state.roots[0].name !== props.preselectedClass.name) {
			return {
				roots: [{ name: props.preselectedClass.name }],
				selectedClass: props.preselectedClass,
				preselectedClass: props.preselectedClass,
				selectedSide: props.side || "instance"
			};
		}
		if (props.preselectedPackage && props.showPackages) {
			return {
				preselectedPackage: props.preselectedPackage,
			};
		}
		return null;
	}

	async updateClass(species) {
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

	packageSelected = async (pack) => {
		this.context.updatePageLabel(this.props.id, pack.name);
		await this.updatePackage(pack);
		this.setState({ selectedPackage: pack, selectedClass: null, selectedCategory: null, selectedVariable: null, selectedMethod: null })
	};

	sideSelected = async (side) => {
		await this.updateClass(this.state.selectedClass);
		this.setState({ selectedSide: side, selectedCategory: null, selectedVariable: null, selectedMethod: null })
	}

	classSelected = async (species) => {
		this.context.updatePageLabel(this.props.id, species.name);
		await this.updateClass(species);
		this.setState({ selectedClass: species, selectedCategory: null, selectedVariable: null, selectedMethod: null });
	}

	classDefined = async (species) => {
		this.context.updatePageLabel(this.props.id, species.name);
		if (this.classTreeRef && this.classTreeRef.current) {
			this.classTreeRef.current.refreshEnsuring(species);
		}
		this.setState({ selectedClass: species });
	}

	classRemoved = async (species) => {
		this.setState({ selectedClass: null });
	}

	accessSelected = async (access) => {
		this.setState({ selectedAccess: access });
	}

	variableSelected = (variable) => {
		this.setState({ selectedVariable: variable });
	}

	categorySelected = (category) => {
		this.setState({ selectedCategory: category });
	}

	categoryAdded = (category) => {
		this.setState({ selectedCategory: category });
	}

	categoryRenamed = (category, renamed) => {
		this.setState({ selectedCategory: renamed });
	}

	categoryRemoved = (category) => {
		this.setState({ selectedCategory: null })
	}

	async updateMethod(method) {
		try {
			let retrieved = await ide.backend.method(
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
			this.context.updatePageLabel(
				this.props.id,
				method.methodClass + ">>" + method.selector
			);
		}
		if (!method.template) {
			await this.updateMethod(method);
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
		if (this.methodListRef && this.methodListRef.current) {
			this.methodListRef.current.refreshEnsuring(method);
		}
		this.setState({ selectedMethod: method });
	};

	methodClassified = async (method) => {
		if (!method) return;
		if (this.categoryListRef && this.categoryListRef.current) {
			this.categoryListRef.current.refreshEnsuring(method.category);
		}
		this.setState({ selectedCategory: method.category });
	};

	targetClass() {
		let { selectedSide, selectedClass } = this.state;
		return selectedClass ?
			selectedSide === "instance" ? selectedClass : selectedClass.metaclass
			: null;
	}

	classLabelStyle = (species) => {
		if (!this.props.showPackages) return "normal";
		const pack = this.state.selectedPackage;
		return pack &&
			pack.methods &&
			(pack.methods[species.name] ||
				pack.methods[species.name + " class"])
			? "italic"
			: "normal";
	};

	methodLabelStyle = (method) => {
		if (!this.props.showPackages) return "normal";
		const pack = this.state.selectedPackage;
		return pack &&
			pack.methods &&
			pack.methods[method.methodClass] &&
			pack.methods[method.methodClass].includes(method.selector)
			? "italic"
			: "normal";
	};

	evaluationContext() {
		const species = this.targetClass();
		return species ? { class: species.name } : {};
	}

	render() {
		let {
			roots,
			selectedPackage,
			selectedSide,
			selectedVariable,
			selectedAccess,
			selectedCategory,
			selectedMethod,
			preselectedClass,
			preselectedPackage } = this.state;
		let targetClass = this.targetClass();
		let showPackages = this.props.showPackages;
		let width = showPackages ? "20%" : "25%";
		return (
			<CustomSplit mode="vertical">
				<Box sx={{ minHeight: 50, height: "35%" }}>
					<Box display="flex" flexDirection="row" width="100%" height="100%">
						<CustomSplit >
							{showPackages && <Box sx={{ width: width }}>
								<UPackageList
									onPackageSelect={this.packageSelected}
									preselectedPackage={preselectedPackage}
								/>
							</Box>}
							<Box sx={{ width: width }}>
								<UClassTree
									ref={this.classTreeRef}
									roots={!showPackages ? roots : null}
									package={showPackages ? selectedPackage : null}
									onClassSelect={this.classSelected}
									onClassDefine={this.classDefined}
									onClassRemove={this.classRemoved}
									showSearch={!showPackages}
									labelStyle={this.classLabelStyle}
									preselectedClass={preselectedClass}
								/>
							</Box>
							<Box display="flex" flexDirection="column" sx={{ height: "100%", width: width }}>
								<Box>
									<Select
										size="small"
										value={selectedAccess}
										input={
											<OutlinedInput margin="dense" fullWidth />
										}
										onChange={(event) => { this.accessSelected(event.target.value) }}
									>
										<MenuItem value={"using"}>using</MenuItem>
										<MenuItem value={"assigning"}>
											assigning
										</MenuItem>
										<MenuItem value={"accessing"}>
											referencing
										</MenuItem>
									</Select>
								</Box>
								<Box mt={1} flexGrow={1}>
									<UVariableList
										class={targetClass}
										onVariableSelect={this.variableSelected}
									/>
								</Box>
							</Box>
							<Box display="flex" flexDirection="column" sx={{ height: "100%", width: width }}>
								<Box display="flex" justifyContent="center">
									<RadioGroup
										name="side"
										value={selectedSide}
										onChange={(event, side) => this.sideSelected(side)}
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
								<Box mt={1} flexGrow={1}>
									<UCategoryList
										ref={this.categoryListRef}
										class={targetClass}
										onCategorySelect={this.categorySelected}
										highlightedCategory={selectedMethod ? selectedMethod.category : null}
										onCategoryAdd={this.categoryAdded}
										onCategoryRename={this.categoryRenamed}
										onCategoryRemove={this.categoryRemoved}
									/>
								</Box>
							</Box>
							<Box sx={{ width: width }}>
								<CustomPaper>
									<UMethodList
										ref={this.methodListRef}
										class={targetClass}
										category={selectedCategory}
										access={selectedAccess}
										variable={selectedVariable}
										onMethodSelect={this.methodSelected}
										onMethodRename={this.methodRenamed}
										onMethodRemove={this.methodRemoved}
										onMethodClassify={this.methodClassified}
										onCategoryAdd={this.categoryAdded}
										labelStyle={this.methodLabelStyle}
										showNewOption
									/>
								</CustomPaper>
							</Box>
						</CustomSplit>
					</Box>
				</Box>
				<Box sx={{ height: "60%" }}>
					<CodeBrowser
						context={this.evaluationContext()}
						class={targetClass}
						method={selectedMethod}
						onMethodCompile={this.methodCompiled}
						onClassDefine={this.classDefined}
						onClassComment={this.classCommented}
						sx={{ height: "100%" }}
					/>
				</Box>
			</CustomSplit>
		);
	}
}

export default SystemBrowser;