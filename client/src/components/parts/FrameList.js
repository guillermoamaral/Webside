import React, { Component } from "react";
import CustomList from "../controls/CustomList";
import { IDEContext } from "../IDEContext";

class FrameList extends Component {
	static contextType = IDEContext;

	frameSelected = (frame) => {
		if (this.props.onSelect) {
			this.props.onSelect(frame);
		}
	};

	browseClass = (frame) => {
		if (frame) {
			this.context.browseClass(frame.class.name);
		}
	};

	browseSenders = (frame) => {
		if (frame) {
			this.context.browseSenders(frame.method.selector);
		}
	};

	browseLocalSenders = (frame) => {
		if (frame) {
			this.context.browseLocalSenders(frame.method.selector, frame.class.name);
		}
	};

	browseImplementors = (frame) => {
		if (frame) {
			this.context.browseImplementors(frame.method.selector);
		}
	};

	browseLocalImplementors = (frame) => {
		if (frame) {
			this.context.browseLocalImplementors(
				frame.method.selector,
				frame.class.name
			);
		}
	};

	browseClassReferences = (frame) => {
		if (frame) {
			this.context.browseClassReferences(frame.class.name);
		}
	};

	menuOptions() {
		return [
			{ label: "Browse class", action: this.browseClass },
			{ label: "Senders", action: this.browseSenders },
			{ label: "Local senders", action: this.browseLocalSenders },
			{ label: "Implementors", action: this.browseImplementors },
			{ label: "Local implementors", action: this.browseLocalImplementors },
			{ label: "Class references", action: this.browseClassReferences },
		];
	}

	render() {
		return (
			<CustomList
				itemLabel="label"
				items={this.props.frames}
				selectedItem={this.props.selected}
				onSelect={this.frameSelected}
				menuOptions={this.menuOptions()}
			/>
		);
	}
}

export default FrameList;
