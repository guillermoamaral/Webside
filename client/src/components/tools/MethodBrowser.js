import React, { Component } from "react";
import {
	Grid,
	Box,
	Paper,
	FormGroup,
	FormControlLabel,
	Checkbox,
} from "@material-ui/core";
import clsx from "clsx";
import { IDEContext } from "../IDEContext";
import MethodList from "../parts/MethodList";
import CodeBrowser from "../parts/CodeBrowser";

class MethodBrowser extends Component {
	static contextType = IDEContext;

	constructor(props) {
		super(props);
		this.state = {
			selectedMethod: null,
			selectedClass: null,
			showTests: true,
		};
	}

	componentDidMount() {
		const method = this.props.methods.length > 0 ? this.props.methods[0] : null;
		if (method) {
			this.methodSelected(method);
		}
	}

	methodSelected = async (method) => {
		try {
			const species = await this.context.api.getClass(method.class);
			const retrieved = await this.context.api.getMethod(
				method.class,
				method.selector
			);
			Object.assign(method, retrieved);
			this.setState({ selectedMethod: method, selectedClass: species });
		} catch (error) {
			this.context.reportError(error);
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
		const selected = this.state.selectedMethod;
		if (method.selector === selected.selector) {
			selected.source = method.source;
			this.setState({ selectedMethod: selected });
		}
	};

	currentMethods() {
		if (this.state.showTests) {
			return this.props.methods;
		}
		return this.props.methods.filter((m) => {
			return !this.isTest(m);
		});
	}

	isTest(method) {
		return method && method.selector.startsWith("test");
	}

	showTests(show) {
		var selected = this.state.selectedMethod;
		if (!show && this.isTest(selected)) {
			selected = null;
		}
		this.setState({ showTests: show, selectedMethod: selected });
	}

	render() {
		const { selectedMethod, selectedClass, showTests } = this.state;
		const { selectedWord, styles } = this.props;
		const methods = this.currentMethods();
		const fixedHeightPaper = clsx(styles.paper, styles.fixedHeight);
		return (
			<Grid container spacing={1}>
				<Grid item xs={12} md={12} lg={12}>
					<Box display="flex" justifyContent="flex-end">
						<FormGroup>
							<FormControlLabel
								control={
									<Checkbox
										size="small"
										checked={showTests}
										color="primary"
										onChange={(event) => this.showTests(event.target.checked)}
									/>
								}
								label="Show tests"
							/>
						</FormGroup>
					</Box>
				</Grid>
				<Grid item xs={12} md={12} lg={12}>
					<Paper className={fixedHeightPaper} variant="outlined">
						<MethodList
							showClass={true}
							selected={selectedMethod}
							methods={methods}
							onSelect={this.methodSelected}
						/>
					</Paper>
				</Grid>
				<Grid item xs={12} md={12} lg={12}>
					<CodeBrowser
						styles={styles}
						class={selectedClass}
						method={selectedMethod}
						selectedWord={selectedWord}
						onCompileMethod={this.methodCompiled}
						onDefineClass={this.classDefined}
						onCommentClass={this.classCommented}
					/>
				</Grid>
			</Grid>
		);
	}
}

export default MethodBrowser;
