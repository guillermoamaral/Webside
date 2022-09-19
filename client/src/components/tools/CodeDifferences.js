import React, { Component } from "react";
import { Grid, Paper } from "@material-ui/core";
import CodeMerge from "../parts/CodeMerge";

class CodeDifferences extends Component {
	render() {
		return (
			<Grid container spacing={1}>
				<Grid item xs={12} md={12} lg={12}>
					<Paper variant="outlined">
						<CodeMerge
							styles={this.props.styles}
							leftCode={this.props.leftSource}
							rightCode={this.props.rightSource}
						/>
					</Paper>
				</Grid>
			</Grid>
		);
	}
}

export default CodeDifferences;
