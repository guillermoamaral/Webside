import React, { Component } from "react";
import { Paper } from "@mui/material";

class CustomPaper extends Component {
	render() {
		return (
			<Paper
				variant="outlined"
				sx={{ padding: 1, height: "100%" }}
			>
				{this.props.children}
			</Paper>
		);
	}
}

export default CustomPaper;
