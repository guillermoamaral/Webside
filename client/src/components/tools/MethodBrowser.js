import React, { Component } from "react";
import { Grid, Paper } from "@material-ui/core";
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
		};
	}

	methodSelected = async (method) => {
		try {
			const species = await this.context.api.getClass(method.class);
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
		}
	};

	render() {
		const { selectedMethod, selectedClass } = this.state;
		const { selectedWord, styles } = this.props;
		const fixedHeightPaper = clsx(styles.paper, styles.fixedHeight);
		return (
			<Grid container spacing={1}>
				<Grid item xs={12} md={12} lg={12}>
					<Paper className={fixedHeightPaper} variant="outlined">
						<MethodList
							showClass={true}
							selectedMethod={selectedMethod}
							methods={this.props.methods}
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
						onMethodCompiled={this.methodCompiled}
						onClassDefined={this.classDefined}
						onClassCommented={this.classCommented}
					/>
				</Grid>
			</Grid>
		);
	}
}

export default MethodBrowser;
