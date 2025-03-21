import React, { Component } from "react";
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
			const pinned = await ide.backend.pinObjectSlot(id, path);
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

	slotIcon = (object) => {
		if (!object || !object.iconName) return;
		const icon = ide.iconNamed(object.iconName);
		if (!icon) return;
		return (
			<img
				src={"data:image/png;base64," + icon.data}
				width={16}
				height={16}
				alt={this.objectId(object)}
			/>
		);
	};

	render() {
		const {
			roots,
			onSlotSelect,
			onSlotExpand,
			onSlotCollapse,
			selectedObject,
			expandedSlots,
		} = this.props;
		return (
			<FastTree
				nodes={roots ? roots : []}
				nodeLabel="slot"
				nodeChildren="slots"
				onNodeSelect={onSlotSelect}
				onNodeExpand={onSlotExpand}
				onNodeCollapse={onSlotCollapse}
				menuOptions={this.menuOptions()}
				selectedNode={selectedObject}
				expandedNodes={expandedSlots}
				nodeIcon={this.slotIcon}
			/>
		);
	}
}
export default ObjectTree;
