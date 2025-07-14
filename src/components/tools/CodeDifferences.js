import Tool from "./Tool";
import { Grid, Paper } from "@mui/material";
import CodeDiffEditorBackend from "../parts/CodeDiffEditorBackend";

class CodeDifferences extends Tool {
	render() {
		return (
			<Grid container spacing={1}>
				<Grid item xs={12} md={12} lg={12}>
					<Paper variant="outlined">
						<CodeDiffEditorBackend
							leftSource={this.props.leftSource}
							rightSource={this.props.rightSource}
						/>
					</Paper>
				</Grid>
			</Grid>
		);
	}
}

export default CodeDifferences;
