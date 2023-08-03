import React from "react";
import Tool from "./Tool";
import FastTree from "../controls/FastTree";

//This is is a component to test ideas in a tool-like tab.
class POC extends Tool {
	render() {
		const root = {
			label: "root",
			children: [
				{
					label: "child 1",
					children: [{ label: "grand child 1", children: [] }],
				},
				{
					label: "child 2",
					children: [{ label: "grand child 2", children: [] }],
				},
			],
		};
		return (
			<FastTree
				//sx={{ height: "100%" }}
				nodes={[root]}
				nodeLabel="label"
				nodeChildren="children"
				selectedNode={root.children[1].children[0]}
				//expandedNodes={}
			/>
		);
	}
}

export default POC;
