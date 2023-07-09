import React, { Component } from "react";
import { ide } from "../IDE";
import FastTree from "../controls/FastTree";

//This is is a component to test ideas in a tool-like tab.
class POC extends Component {
	constructor(props) {
		super(props);
		this.state = {
			tree: [],
		};
	}

	async componentDidMount() {
		try {
			const tree = await ide.api.classTree("Object", 10);
			this.setState({ tree: [tree] });
		} catch (error) {
			ide.reportError(error);
		}
	}

	render() {
		return (
			<FastTree
				style={{ height: 400, width: 500 }}
				nodes={this.state.tree}
				nodeId="name"
				nodeLabel="name"
				nodeChildren="subclasses"
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
