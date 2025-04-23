import React, { Component } from "react";
import CustomList from "../controls/CustomList";
import ToolContainerContext from "../ToolContainerContext";

class FrameList extends Component {
	static contextType = ToolContainerContext;

	frameSelected = (frame) => {
		if (this.props.onFrameSelect) this.props.onFrameSelect(frame);
	};

	browseClass = (frame) => {
		if (frame) this.context.browseClass(frame.class.name);
	};

	browseSenders = (frame) => {
		if (frame) this.context.browseSenders(frame.method.selector);
	};

	browseLocalSenders = (frame) => {
		if (frame)
			this.context.browseLocalSenders(
				frame.method.selector,
				frame.class.name
			);
	};

	browseImplementors = (frame) => {
		if (frame && frame.method && frame.method.selector)
			this.context.browseImplementors(frame.method.selector);
	};

	browseLocalImplementors = (frame) => {
		if (frame)
			this.context.browseLocalImplementors(
				frame.method.selector,
				frame.class.name
			);
	};

	browseClassReferences = (frame) => {
		if (frame) this.context.browseClassReferences(frame.class.name);
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
				//itemLink={this.browseImplementors}
				items={this.props.frames}
				selectedItem={this.props.selectedFrame}
				onItemSelect={this.frameSelected}
				menuOptions={this.menuOptions()}
				onItemDoubleClick={this.browseClass}
			/>
		);
	}
}

export default FrameList;
