import React from "react";
import Tool from "./Tool";
import { Grid, Paper } from "@mui/material";
import CodeMerge from "../parts/CodeMerge";

class CodeDifferences extends Tool {
	render() {
		return (
			<Grid container spacing={1}>
				<Grid item xs={12} md={12} lg={12}>
					<Paper variant="outlined">
						<CodeMerge
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
