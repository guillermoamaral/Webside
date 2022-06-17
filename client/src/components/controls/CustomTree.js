import React, { Component } from "react";
import TreeView from "@material-ui/lab/TreeView";
import TreeItem from "@material-ui/lab/TreeItem";
import { Box, Typography } from "@material-ui/core";
import ArrowRightIcon from "@material-ui/icons/ArrowRight";
import ArrowDropDownIcon from "@material-ui/icons/ArrowDropDown";
import PopupMenu from "./PopupMenu";
import Scrollable from "./Scrollable";

class CustomTree extends Component {
	constructor(props) {
		super(props);
		this.state = {
			items: props.items,
			selectedItem: !props.selectedItem ? null : props.selectedItem,
			menuOpen: false,
			menuPosition: { x: null, y: null },
		};
	}

	static getDerivedStateFromProps(props, state) {
		if (props.selecteItem !== state.selectedItem) {
			return {
				items: props.items,
				selectedItem: !props.selectedItem ? null : props.selectedItem,
			};
		}
		return null;
	}

	createItems = (items) => {
		return items.map((item, index) => {
			const label = this.getItemLabel(item);
			const weight =
				this.props.selectedItem === item
					? "fontWeightBold"
					: "fontWeightRegular";
			const style = this.getItemStyle(item);
			const children = this.getItemChildren(item);
			const id = this.getItemId(item);
			return (
				<TreeItem
					key={label + "-item-" + index}
					nodeId={id}
					label={
						<Typography component="div">
							<Box fontWeight={weight} fontStyle={style}>
								{label}
							</Box>
						</Typography>
					}
					onLabelClick={(event) => this.itemSelected(event, item)}
					onIconClick={(event) => this.itemToggled(event, item)}
					onContextMenu={this.openMenu}
				>
					{Array.isArray(children) ? this.createItems(children) : null}
				</TreeItem>
			);
		});
	};

	getItemId = (item) => {
		const getter = this.props.itemId;
		if (!getter) {
			return this.getItemLabel(item);
		}
		if (typeof getter == "string") {
			const id = item[getter];
			return id ? id.toString() : "";
		}
		return getter(item);
	};

	getItemLabel = (item) => {
		const getter = this.props.itemLabel;
		if (!getter) {
			return item;
		}
		if (typeof getter == "string") {
			return item[getter];
		}
		return getter(item);
	};

	getItemStyle = (item) => {
		const style = this.props.itemStyle;
		if (!style) {
			return "normal";
		}
		if (typeof style == "string") {
			return style;
		}
		return style(item);
	};

	getItemChildren = (item) => {
		const getter = this.props.children;
		if (!getter) {
			return null;
		}
		if (typeof getter == "string") {
			return item[getter];
		}
		return getter(item);
	};

	itemSelected = (event, item) => {
		event.preventDefault();
		this.setState({ selectedItem: item });
		const handler = this.props.onSelect;
		if (handler) {
			handler(item);
		}
	};

	itemToggled = (event, item) => {
		const handler = this.props.onExpand;
		if (handler) {
			handler(item);
		}
	};

	openMenu = (event) => {
		event.preventDefault();
		this.setState({
			menuOpen: true,
			menuPosition: { x: event.clientX - 2, y: event.clientY - 4 },
		});
	};

	closeMenu = () => {
		this.setState({ menuOpen: false });
	};

	menuOptionClicked = (option) => {
		const selected = this.state.selectedItem;
		if (option.action) {
			option.action(selected);
		}
	};

	render() {
		const selected = !this.state.selectedItem
			? null
			: this.getItemId(this.state.selectedItem);
		return (
			<Scrollable>
				<TreeView
					defaultCollapseIcon={<ArrowDropDownIcon />}
					defaultExpanded={["root"]}
					defaultExpandIcon={<ArrowRightIcon />}
					selected={selected}
				>
					{this.createItems(this.props.items)}
				</TreeView>
				<PopupMenu
					options={this.props.menuOptions}
					open={this.state.menuOpen}
					position={this.state.menuPosition}
					onOptionClick={this.menuOptionClicked}
					onClose={this.closeMenu}
				/>
			</Scrollable>
		);
	}
}

export default CustomTree;
