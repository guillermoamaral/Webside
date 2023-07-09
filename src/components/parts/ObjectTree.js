import React, { Component } from "react";
//import CustomTree from "../controls/CustomTree";
import { ide } from "../IDE";
import ToolContainerContext from "../ToolContainerContext";
import FastTree from "../controls/FastTree";

class ObjectTree extends Component {
	static contextType = ToolContainerContext;

	browseClass = (object) => {
		if (object) {
			this.context.browseClass(object.class);
		}
	};

	inspect = async (object) => {
		try {
			const id = this.props.roots[0].id;
			const path = this.objectURIPath(object);
			const pinned = await ide.api.pinObjectSlot(id, path);
			this.context.openInspector(pinned);
		} catch (error) {
			ide.reportError(error);
		}
	};

	browseReferences = (object) => {};

	menuOptions() {
		return [
			{ label: "Browse class", action: this.browseClass },
			{ label: "Inspect", action: this.inspect },
		];
	}

	objectURIPath(object) {
		let path = "";
		if (object) {
			object.path.forEach((s) => (path = path + "/" + s));
		}
		return path;
	}

	objectId = (object) => {
		if (this.props.roots.includes(object) && object.id) return object.id;
		return this.objectURIPath(object);
	};

	render() {
		const roots = this.props.roots;
		return (
			// <CustomTree
			// 	items={roots ? roots : []}
			// 	itemLabel="slot"
			// 	itemId={this.objectId}
			// 	children={"slots"}
			// 	onItemExpand={this.props.onSlotExpand}
			// 	onItemSelect={this.props.onSlotSelect}
			// 	selectedItem={this.props.selectedObject}
			// 	menuOptions={this.menuOptions()}
			// />
			<FastTree
				nodes={roots ? roots : []}
				nodeId={this.objectId}
				nodeLabel="slot"
				nodeChildren="slots"
				onNodeSelect={this.props.onSlotSelect}
				onNodeExpand={this.props.onSlotExpand}
				menuOptions={this.menuOptions()}
			/>
		);
	}
}
export default ObjectTree;
