import React, { Component } from "react";
import CustomTree from "../controls/CustomTree";
import { ide } from "../IDE";

class ObjectTree extends Component {
	browseClass = (object) => {
		if (object) {
			ide.browseClass(object.class);
		}
	};

	inspect = async (object) => {
		try {
			const id = this.props.roots[0].id;
			const path = this.objectURIPath(object);
			const pinned = await ide.api.pinObjectSlot(id, path);
			ide.openInspector(pinned);
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
		object.path.forEach((s) => (path = path + "/" + s));
		return path;
	}

	render() {
		const roots = this.props.roots;
		return (
			<CustomTree
				items={roots ? roots : []}
				itemLabel="slot"
				itemId={(object) => this.objectURIPath(object)}
				children={"slots"}
				onExpand={this.props.onExpand}
				onSelect={this.props.onSelect}
				selectedItem={this.props.selected}
				menuOptions={this.menuOptions()}
			/>
		);
	}
}
export default ObjectTree;
