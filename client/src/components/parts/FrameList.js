import React, { Component } from "react";
import CustomList from "../controls/CustomList";
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
		if (frame && frame.method && frame.method.selector) {
			ide.browseImplementors(frame.method.selector);
		}
	};

	browseLocalImplementors = (frame) => {
		if (frame) {
			ide.browseLocalImplementors(
				frame.method.selector,
				frame.class.name
			);
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
			{ label: "Browse senders", action: this.browseSenders },
			{ label: "Browse local senders", action: this.browseLocalSenders },
			{ label: "Browse implementors", action: this.browseImplementors },
			{
				label: "Browse local implementors",
				action: this.browseLocalImplementors,
			},
			{
				label: "Browse class references",
				action: this.browseClassReferences,
			},
		];
	}

	render() {
		return (
			<CustomList
				itemLabel="label"
				itemLink={this.browseImplementors}
				items={this.props.frames}
				selectedItem={this.props.selected}
				onSelect={this.frameSelected}
				menuOptions={this.menuOptions()}
			/>
		);
	}
}

export default FrameList;
