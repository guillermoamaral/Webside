import React, { Component } from "react";
import { Paper } from "@mui/material";

class CustomPaper extends Component {
	render() {
		return (
			<Paper
				variant="outlined"
				sx={{ padding: 1, height: this.props.height || 200 }}
			>
				{this.props.children}
			</Paper>
		);
	}
}

export default CustomPaper;
