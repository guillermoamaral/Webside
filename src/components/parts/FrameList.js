import React, { Component } from "react";
import CustomList from "../controls/CustomList";
import { container } from "../ToolsContainer";

class FrameList extends Component {
	frameSelected = (frame) => {
		if (this.props.onFrameSelect) {
			this.props.onFrameSelect(frame);
		}
	};

	browseClass = (frame) => {
		if (frame) {
			container.browseClass(frame.class.name);
		}
	};

	browseSenders = (frame) => {
		if (frame) {
			container.browseSenders(frame.method.selector);
		}
	};

	browseLocalSenders = (frame) => {
		if (frame) {
			container.browseLocalSenders(frame.method.selector, frame.class.name);
		}
	};

	browseImplementors = (frame) => {
		if (frame && frame.method && frame.method.selector) {
			container.browseImplementors(frame.method.selector);
		}
	};

	browseLocalImplementors = (frame) => {
		if (frame) {
			container.browseLocalImplementors(
				frame.method.selector,
				frame.class.name
			);
		}
	};

	browseClassReferences = (frame) => {
		if (frame) {
			container.browseClassReferences(frame.class.name);
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
				onItemSelect={this.frameSelected}
				menuOptions={this.menuOptions()}
			/>
		);
	}
}

export default FrameList;
