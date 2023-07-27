import React from "react";
import Tool from "./Tool";
import CustomSplit from "../controls/CustomSplit";
import CustomList from "../controls/CustomList";
import CustomTable from "../controls/CustomTable";
import { Paper } from "@mui/material";

//This is is a component to test ideas in a tool-like tab.
class POC extends Tool {
	render() {
		return (
			<CustomSplit mode="vertical">
				<Paper variant="outlined" style={{ height: "20%" }}>
					<CustomTable
						columns={[{ field: "label", label: "Label" }]}
						rows={[
							{ label: "a" },
							{ label: "b" },
							{ label: "c" },
							{ label: "d" },
							{ label: "e" },
						]}
					/>
				</Paper>
				<Paper variant="outlined" style={{ height: "10%" }}>
					<CustomList items={["a", "b", "c", "d", "e"]} />
				</Paper>
				<Paper variant="outlined" style={{ height: "70%" }}>
					<CustomTable
						columns={[{ field: "label", label: "Label" }]}
						rows={[
							{ label: "f" },
							{ label: "g" },
							{ label: "h" },
							{ label: "i" },
							{ label: "j" },
						]}
					/>
				</Paper>
			</CustomSplit>
		);
	}
}

export default POC;
