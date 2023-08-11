import React from "react";
import Tool from "./Tool";
import SearchList2 from "../controls/SearchList2";
import CustomList from "../controls/CustomList";
import { Paper } from "@mui/material";

//This is is a component to test ideas in a tool-like tab.
class POC extends Tool {
	render() {
		return (
			<div>
				<SearchList2 options={["aa", "ab", "ac", "ad", "ae"]} />
				<Paper variant="outlined" sx={{ height: 300 }}>
					<CustomList items={[1, 2, 3, 4]} />
				</Paper>
			</div>
		);
	}
}

export default POC;
