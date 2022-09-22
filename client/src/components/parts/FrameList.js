import React, { Component } from "react";
import FastCustomList from "../controls/FastCustomList";
import { ide } from "../IDE";

class FrameList extends Component {
	frameSelected = (frame) => {
		if (this.props.onSelect) {
			this.props.onSelect(frame);
		}
	};

	browseClass = (frame) => {
		if (frame) {
			ide.browseClass(frame.class.name);
		}
	};

	browseSenders = (frame) => {
		if (frame) {
			ide.browseSenders(frame.method.selector);
		}
	};

	browseLocalSenders = (frame) => {
		if (frame) {
			ide.browseLocalSenders(frame.method.selector, frame.class.name);
		}
	};

	browseImplementors = (frame) => {
		if (frame) {
			ide.browseImplementors(frame.method.selector);
		}
	};

	browseLocalImplementors = (frame) => {
		if (frame) {
			ide.browseLocalImplementors(frame.method.selector, frame.class.name);
		}
	};

	browseClassReferences = (frame) => {
		if (frame) {
			ide.browseClassReferences(frame.class.name);
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
			<FastCustomList
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
