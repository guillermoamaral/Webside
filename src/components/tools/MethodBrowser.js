import Tool from "./Tool";
import {
	Box,
	FormGroup,
	FormControlLabel,
	Checkbox,
	Typography,
} from "@mui/material";
import { ide } from "../IDE";
import MethodList from "../parts/MethodList";
import CodeBrowser from "../parts/CodeBrowser";
import CustomSplit from "../controls/CustomSplit";
import CustomPaper from "../controls/CustomPaper";

class MethodBrowser extends Tool {
	constructor(props) {
		super(props);
		this.state = {
			methods: props.methods,
			selectedMethod: null,
			selectedClass: null,
			showTests: true,
			editorFullView: false,
		};
	}

	componentDidMount() {
		const method =
			this.state.methods.length > 0 ? this.state.methods[0] : null;
		if (method) {
			this.methodSelected(method);
		}
	}

	methodRemoved = async (method) => {
		const methods = this.state.methods;
		const index = methods.findIndex(
			(m) =>
				m.methodClass === method.methodClass &&
				m.selector === method.selector
		);
		if (index > -1) {
			methods.splice(index, 1);
			const selected =
				index - 1 >= 0
					? methods[index - 1]
					: index + 1 < methods.length
					? methods[index + 1]
					: null;
			let species;
			if (selected) {
				try {
					species = await ide.backend.classNamed(
						selected.methodClass
					);
				} catch (error) {}
			}
			this.setState({
				methods: methods,
				selectedMethod: selected,
				selectedClass: species,
			});
		}
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
		try {
			const species = await ide.backend.classNamed(method.methodClass);
			await this.updateMethod(method);
			this.setState({ selectedMethod: method, selectedClass: species });
		} catch (error) {
			ide.reportError(error);
		}
	};

	classDefined = async (species) => {
		const selected = this.state.selectedClass;
		if (species.name === selected.name) {
			selected.definition = species.definition;
		}
	};

	classCommented = async (species) => {
		const selected = this.state.selectedClass;
		if (species.name === selected.name) {
			selected.comment = species.comment;
		}
	};

	methodCompiled = async (method) => {
		const methods = this.state.methods;
		const index = methods.findIndex(
			(m) =>
				m.methodClass === method.methodClass &&
				m.selector === method.selector
		);
		if (index > -1) {
			const compiled = methods[index];
			compiled.source = method.source;
			this.setState({ selectedMethod: compiled });
		} else {
			const selected = this.state.selectedMethod;
			const index = selected ? methods.indexOf(selected) + 1 : 1;
			methods.splice(index, 0, method);
			this.setState({
				methods: methods,
				selectedMethod: method,
			});
		}
	};

	currentMethods() {
		if (this.state.showTests) return this.state.methods;
		return this.state.methods.filter((m) => {
			return !this.isTest(m);
		});
	}

	isTest(method) {
		return method && method.selector.startsWith("test");
	}

	updateLabel = () => {
		const label =
			(this.props.title || "Methods") +
			" (" +
			this.currentMethods().length +
			")";
		this.context.updatePageLabel(this.props.id, label);
	};

	showTests(show) {
		var selected = this.state.selectedMethod;
		if (!show && this.isTest(selected)) {
			selected = null;
		}

		this.setState(
			{ showTests: show, selectedMethod: selected },
			this.updateLabel
		);
	}

	evaluationContext() {
		const species = this.state.selectedClass;
		return species
			? {
					class: species.name,
			  }
			: {};
	}

	toggleFullView = () => {
		this.setState({ editorFullView: !this.state.editorFullView });
	};

	browseSendersInList = async (editor) => {
		const selector = await editor.targetSelector();
		console.log(selector);
		if (!selector) return;
		this.context.browseSendersInList(selector, this.currentMethods());
	};

	render() {
		const { selectedMethod, selectedClass, showTests, editorFullView } =
			this.state;
		const { selectedSelector, selectedIdentifier, title, description } = this.props;
		const methods = this.currentMethods();
		const background = ide.colorSetting("methodBrowserColor");
		return (
			<Box
				display="flex"
				flexDirection="column"
				sx={{
					height: "100%",
					background: background,
					padding: 1,
				}}
			>
				<Box>
					<Box display="flex" justifyContent="flex-end">
						<Box
							display="flex"
							flexGrow={1}
							justifyContent="center"
							alignItems="center"
						>
							<Typography>{description || title}</Typography>
						</Box>
						<FormGroup>
							<FormControlLabel
								control={
									<Checkbox
										size="small"
										checked={showTests}
										color="primary"
										onChange={(event) =>
											this.showTests(event.target.checked)
										}
									/>
								}
								label="Show tests"
							/>
						</FormGroup>
					</Box>
				</Box>
				<Box flexGrow={1}>
					<CustomSplit mode="vertical">
						<Box hidden={editorFullView} sx={{ height: "45%" }}>
							<CustomPaper>
								<MethodList
									useTable
									selectedMethod={selectedMethod}
									methods={methods}
									onMethodSelect={this.methodSelected}
									onMethodRemove={this.methodRemoved}
								/>
							</CustomPaper>
						</Box>
						<Box sx={{ height: editorFullView ? "100%" : "50%" }}>
							<CodeBrowser
								context={this.evaluationContext()}
								class={selectedClass}
								method={selectedMethod}
								selectedSelector={selectedSelector}
								selectedIdentifier={selectedIdentifier}
								onMethodCompile={this.methodCompiled}
								onClassDefine={this.classDefined}
								onClassComment={this.classCommented}
								onFullViewToggle={this.toggleFullView}
								externalOptions={[
									{
										label: "Browse senders in list",
										action: this.browseSendersInList,
									},
								]}
							/>
						</Box>
					</CustomSplit>
				</Box>
			</Box>
		);
	}
}

export default MethodBrowser;
