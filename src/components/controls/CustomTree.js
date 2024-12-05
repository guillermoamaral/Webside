import React, { Component } from "react";
import TreeView from "@mui/lab/TreeView";
import TreeItem from "@mui/lab/TreeItem";
import { Box, Typography } from "@mui/material";
import ArrowRightIcon from "@mui/icons-material/ArrowRight";
import ArrowDropDownIcon from "@mui/icons-material/ArrowDropDown";
import PopupMenu from "./PopupMenu";
import Scrollable from "./Scrollable";

class CustomTree extends Component {
	constructor(props) {
		super(props);
		this.itemsById = {};
		this.state = {
			items: props.items,
			selectedItem: !props.selectedItem ? null : props.selectedItem,
			expandedItems: [...props.items],
			menuOpen: false,
			menuPosition: { x: null, y: null },
		};
	}

	static getDerivedStateFromProps(props, state) {
		if (
			props.items !== state.items &&
			(props.items.length > 1 || props.items[0] !== state.items[0])
		) {
			return {
				items: props.items,
				expandedItems: [...props.items],
				selectedItem: !props.selectedItem ? null : props.selectedItem,
			};
		}
		if (props.selectedItem !== state.selectedItem) {
			return {
				selectedItem: !props.selectedItem ? null : props.selectedItem,
			};
		}
		return null;
	}

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

	initializeItemsByIdFrom(items) {
		var children;
		return items.forEach((item) => {
			this.itemsById[this.getItemId(item)] = item;
			children = this.getItemChildren(item);
			if (Array.isArray(children)) {
				this.initializeItemsByIdFrom(children);
			}
		});
	}

	createItems = (items) => {
		return items.map((item, index) => {
			const label = this.getItemLabel(item) || "";
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
					onContextMenu={this.openMenu}
				>
					{Array.isArray(children)
						? this.createItems(children)
						: null}
				</TreeItem>
			);
		});
	};

	itemsSelected = (event, id) => {
		event.preventDefault();
		const item = this.itemsById[id];
		this.setState({ selectedItem: item });
		if (this.props.onItemSelect) {
			this.props.onItemSelect(item);
		}
	};

	itemsToggled = (event, ids) => {
		event.preventDefault();
		const { expandedItems } = this.state;
		const items = ids.map((id) => this.itemsById[id]);
		this.setState({ expandedItems: items });
		if (this.props.onItemExpand) {
			items.forEach((item) => {
				if (!expandedItems.includes(item)) {
					this.props.onItemExpand(item);
				}
			});
		}
	};

	menuOptions = () => {
		const options = this.props.menuOptions;
		return typeof options === "function"
			? options(this.state.selectedItem)
			: options;
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

	getMenuOptionEnabled = (option) => {
		const selected = this.props.selectedItem;
		if (option.enabled) {
			return option.enabled(selected);
		}
		return true;
	};

	render() {
		const { items, selectedItem, expandedItems, menuOpen, menuPosition } =
			this.state;
		const selected = !selectedItem ? null : this.getItemId(selectedItem);
		const expanded = expandedItems.map((i) => {
			return this.getItemId(i);
		});
		this.initializeItemsByIdFrom(items);
		const menuOptions = this.menuOptions();
		return (
			<Scrollable>
				<TreeView
					defaultCollapseIcon={<ArrowDropDownIcon />}
					defaultExpandIcon={<ArrowRightIcon />}
					selected={selected}
					expanded={expanded}
					onNodeSelect={(event, ids) => {
						this.itemsSelected(event, ids);
					}}
					onNodeToggle={(event, ids) => {
						this.itemsToggled(event, ids);
					}}
					//"
				>
					{this.createItems(items)}
				</TreeView>
				{menuOptions && menuOptions.length > 0 && (
					<PopupMenu
						options={menuOptions}
						open={menuOpen}
						position={menuPosition}
						onOptionClick={this.menuOptionClicked}
						onOptionEnable={this.getMenuOptionEnabled}
						onClose={this.closeMenu}
					/>
				)}
			</Scrollable>
		);
	}
}

export default CustomTree;
