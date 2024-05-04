import React from "react";
import Tool from "./Tool";
import { Box } from "@mui/material";

//This is is a component to test ideas in a tool-like tab.
class POC extends Tool {
	render() {
		return (
			<Box
				display="flex"
				alignItems="center"
				justifyItems={"center"}
			></Box>
		);
	}
}

export default POC;
