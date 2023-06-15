import React, { Component } from "react";
import {
	ListItem,
	ListItemText,
	ListItemIcon,
	Box,
	TextField,
	Typography,
	Link,
} from "@mui/material";
import { FixedSizeList as List } from "react-window";
import AutoSizer from "react-virtualized-auto-sizer";
import PopupMenu from "./PopupMenu";
import RSC from "react-scrollbars-custom";

const ITEM_SIZE = 30;

const CustomScrollbars = ({
	children,
	forwardedRef,
	onScroll,
	style,
	className,
}) => {
	return (
		<RSC
			className={className}
			style={style}
			scrollerProps={{
				renderer: (props) => {
					const {
						elementRef,
						onScroll: rscOnScroll,
						...restProps
					} = props;
					return (
						<span
							{...restProps}
							onScroll={(e) => {
								onScroll(e);
								rscOnScroll(e);
							}}
							ref={(ref) => {
								forwardedRef(ref);
								elementRef(ref);
							}}
						/>
					);
				},
			}}
		>
			{children}
		</RSC>
	);
};

const CustomScrollbarsVirtualList = React.forwardRef((props, ref) => (
	<CustomScrollbars {...props} forwardedRef={ref} />
));

const listRef = React.createRef();
const outerRef = React.createRef();

class CustomList extends Component {
	constructor(props) {
		super(props);
		this.state = {
			menuOpen: false,
			menuPosition: { x: null, y: null },
			items: this.props.items,
			filterEnabled: false,
			filterText: "",
			filteredItems: this.props.items,
		};
	}

	static getDerivedStateFromProps(props, state) {
		if (state.items !== props.items) {
			return {
				menuOpen: false,
				items: props.items,
				filterEnabled: false,
				filterText: "",
				filteredItems: props.items,
			};
		}
		return null;
	}

	itemDoubleClicked = (item) => {
		if (this.props.onDoubleClick) {
			this.props.onDoubleClick(item);
		}
	};

	itemSelected = (item) => {
		if (this.props.onItemSelect) {
			this.props.onItemSelect(item);
		}
	};

	getItemDivider = (item) => {
		const getter = this.props.itemDivider;
		if (!getter) {
			return false;
		}
		if (typeof getter === "string") {
			return item[getter];
		}
		return getter(item);
	};

	getItemLabel = (item) => {
		const getter = this.props.itemLabel;
		if (!getter) {
			return item;
		}
		if (typeof getter === "string") {
			return item[getter];
		}
		return getter(item);
	};

	getItemColor = (item) => {
		const color = item.color;
		if (typeof color == "function") {
			return color(item);
		}
		if (typeof color == "string") {
			return color;
		}
		return item.color ? item.color : "default";
	};

	getItemValue = (item) => {
		const label = this.getItemLabel(item);
		if (!this.props.itemLink) {
			return label;
		}
		const color = this.getItemColor(item);
		return (
			<Link
				href="#"
				onClick={() => this.props.itemLink(item)}
				color="textPrimary"
				style={{ color: color }}
			>
				{label}
			</Link>
		);
	};

	getItemIcon = (item) => {
		const getter = this.props.itemIcon;
		if (!getter) {
			return null;
		}
		if (typeof getter === "function") {
			return getter(item);
		}
		return null;
	};

	getItemStyle = (item) => {
		const style = this.props.labelStyle;
		if (!style) {
			return "normal";
		}
		if (typeof style === "string") {
			return style;
		}
		return style(item);
	};

	getItemAlignment = (item) => {
		const alignment = this.props.labelAlignment;
		if (!alignment) {
			return "left";
		}
		if (typeof alignment === "string") {
			return alignment;
		}
		return alignment(item);
	};

	getItemSize = (item) => {
		const size = this.props.labelSize;
		if (!size) {
			return "normal";
		}
		if (typeof size === "string") {
			return size;
		}
		return size(item);
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
		const selected = this.props.selectedItem;
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

	moveUp = () => {
		const items = this.state.filteredItems;
		const index = items.indexOf(this.props.selectedItem);
		if (index > 0) {
			this.itemSelected(items[index - 1]);
		}
	};

	moveDown = () => {
		const items = this.state.filteredItems;
		const index = items.indexOf(this.props.selectedItem);
		if (index < items.length - 1) {
			this.itemSelected(items[index + 1]);
		}
	};

	clearFilter() {
		this.setState({
			filterEnabled: false,
			filterText: "",
			filteredItems: this.state.items,
		});
	}

	keyDown = (event) => {
		event.preventDefault();
		const key = event.key;
		if (key === "ArrowUp") {
			this.clearFilter();
			this.moveUp();
		}
		if (key === "ArrowDown") {
			this.clearFilter();
			this.moveDown();
		}
		if (key === "Escape") {
			this.clearFilter();
		}
		if (key.length === 1 && /[a-zA-Z0-9-_ ]/.test(key)) {
			this.filterItems(key);
		} else {
			return true;
		}
	};

	filterItems(text) {
		const enabled = text !== "";
		const all = this.props.items;
		const target = text.toLowerCase();
		const filtered = enabled
			? all.filter((i) => {
					return this.getItemLabel(i).toLowerCase().includes(target);
			  })
			: all;
		this.setState({
			filterEnabled: enabled,
			filterText: text,
			filteredItems: filtered,
		});
	}

	renderItem = ({ index, style }) => {
		const item = this.state.filteredItems[index];
		const value = this.getItemValue(item);
		const icon = this.getItemIcon(item);
		const divider = this.getItemDivider(item);
		const fontStyle = this.getItemStyle(item);
		const alignment = this.getItemAlignment(item);
		const size = this.getItemSize(item);
		const selected = this.props.selectedItem === item;
		const highlighted = this.props.highlightedItem == item;
		const weight =
			selected || highlighted ? "fontWeightBold" : "fontWeightRegular";
		return (
			<div style={style}>
				<ListItem
					disableGutters={divider}
					//autoFocus={selected}
					style={{
						paddingTop: 0,
						paddingBottom: 0,
						paddingLeft: 0,
						paddingRight: 0,
					}}
					button
					divider={divider}
					key={"item" + index}
					selected={selected}
					onClick={(event) => this.itemSelected(item)}
					onDoubleClick={(event) => this.itemDoubleClicked(item)}
					onContextMenu={this.openMenu}
					onKeyDown={this.keyDown}
				>
					<Box p={0} style={{ minWidth: 10 }}>
						<ListItemIcon style={{ minWidth: 0 }}>
							{icon}
						</ListItemIcon>
					</Box>
					<ListItemText
						primary={
							<Typography noWrap component="div">
								<Box
									fontWeight={weight}
									fontStyle={fontStyle}
									fontSize={size}
									align={alignment}
								>
									{value}
								</Box>
							</Typography>
						}
					/>
				</ListItem>
			</div>
		);
	};

	render() {
		const {
			filterEnabled,
			filterText,
			filteredItems,
			menuOpen,
			menuPosition,
		} = this.state;
		const count = filteredItems ? filteredItems.length : 0;
		const enableFilter =
			typeof this.props.enableFilter == "boolean"
				? this.props.enableFilter
				: true;
		const showFilter = enableFilter && filterEnabled;
		return (
			<Box style={{ height: showFilter ? "90%" : "100%" }}>
				<Box style={{ height: showFilter ? "90%" : "100%" }}>
					<AutoSizer>
						{({ height, width }) => (
							<List
								ref={listRef}
								height={height}
								width={width}
								itemSize={ITEM_SIZE}
								itemCount={count}
								overscanCount={5}
								onKeyDown={this.keyDown}
								style={{ paddingTop: 0, paddingBottom: 0 }}
								outerElementType={CustomScrollbarsVirtualList}
								outerRef={outerRef}
								onScroll={({
									scrollOffset,
									scrollUpdateWasRequested,
								}) => {
									if (scrollUpdateWasRequested) {
										console.log(
											"TODO: check scroll position",
											scrollOffset,
											outerRef.current.scrollHeight
										);
									}
								}}
							>
								{this.renderItem}
							</List>
						)}
					</AutoSizer>
				</Box>
				{showFilter && (
					<Box style={{ height: "10%" }}>
						<TextField
							id="filter"
							variant="standard"
							size="small"
							type="text"
							placeholder="Filter..."
							margin="dense"
							fullWidth
							autoFocus
							name="filter"
							value={filterText}
							onKeyDown={(event) => {
								if (event.key === "Escape") {
									this.clearFilter();
								}
							}}
							onChange={(event) =>
								this.filterItems(event.target.value)
							}
						/>
					</Box>
				)}
				<PopupMenu
					options={this.props.menuOptions}
					open={menuOpen}
					position={menuPosition}
					onOptionClick={this.menuOptionClicked}
					onOptionEnable={this.getMenuOptionEnabled}
					onClose={this.closeMenu}
				/>
			</Box>
		);
	}
}

export default CustomList;
