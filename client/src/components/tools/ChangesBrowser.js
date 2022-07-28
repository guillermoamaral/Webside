import React, { Component } from "react";
import { Grid, Paper } from "@material-ui/core";
//import CodeMerge from "../parts/CodeMerge";
import CodeEditor from "../parts/CodeEditor";
import { IDEContext } from "../IDEContext";
import ChangesTable from "../parts/ChangesTable";

class ChangesBrowser extends Component {
	static contextType = IDEContext;

	constructor(props) {
		super(props);
		this.state = {
			selectedChange: null,
		};
	}

	changeSelected = (change) => {
		this.setState({ selectedChange: change });
	};

	evaluationContext() {
		const change = this.state.selectedChange;
		return change && change.class ? { class: change.class } : {};
	}

	render() {
		const styles = this.props.styles;
		const change = this.state.selectedChange;
		return (
			<Grid container spacing={1}>
				<Grid item xs={12} md={12} lg={12}>
					<Paper variant="outlined" style={{ height: 500 }}>
						<ChangesTable
							styles={styles}
							changes={this.props.changes}
							onSelect={this.changeSelected}
						/>
					</Paper>
				</Grid>
				<Grid item xs={12} md={12} lg={12}>
					<Paper variant="outlined">
						{/* <CodeMerge
							context={this.evaluationContext()}
							styles={this.props.styles}
							leftCode={change ? change.sourceCode : ""}
							rightCode={change ? change.currentSourceCode : ""}
						/> */}
						<CodeEditor
							context={this.evaluationContext()}
							styles={this.props.styles}
							lineNumbers
							source={change ? change.sourceCode : ""}
							showAccept={false}
						/>
					</Paper>
				</Grid>
			</Grid>
		);
	}
}

export default ChangesBrowser;
