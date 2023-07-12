import React, { Component } from "react";
import { ide } from "../IDE";
import FastTree from "../controls/FastTree";

//This is is a component to test ideas in a tool-like tab.
class POC extends Component {
	constructor(props) {
		super(props);
		this.state = {
			tree: [
				{
					label: "root",
					children: [
						{ label: "a", children: [{ label: "b" }] },
						{ label: "c", children: [{ label: "d" }] },
					],
				},
			],
		};
	}

	render() {
		return (
			<FastTree
				style={{ height: 400, width: 500 }}
				nodes={this.state.tree}
				onNodeSelect={(node) => {
					console.log("select", node);
				}}
				onNodeExpand={(node) => {
					console.log("expand", node);
				}}
			/>
		);
	}
}

export default POC;
