import React, { Component } from "react";
import {
	ListItemButton,
	ListItemText,
	ListItemIcon,
	Box,
	TextField,
	Typography,
	Link,
	Tooltip,
	IconButton,
	Skeleton,
} from "@mui/material";
import { FixedSizeList as List } from "react-window";
import AutoSizer from "react-virtualized-auto-sizer";
import PopupMenu from "./PopupMenu";
import RSC from "react-scrollbars-custom";
import { styled } from "@mui/material/styles";

const StyledListItemButton = styled(ListItemButton)(({ theme }) => ({
	"& .actionButtons": {
		display: "none",
	},
	"&:hover .actionButtons": {
		display: "flex",
	},
}));

const ITEM_SIZE = 24;

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

	componentDidUpdate() {
		const selected = this.props.selectedItem;
		if (!selected) return;
		const index = this.state.filteredItems.indexOf(selected);
		if (index < 0) return;
		if (listRef && listRef.current) {
			listRef.current.scrollToItem(index);
		}
	}

	itemDoubleClicked = (item) => {
		if (this.props.onItemDoubleClick) {
			this.props.onItemDoubleClick(item);
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
		const color = this.props.itemColor;
		if (typeof color === "function") {
			return color(item);
		}
		if (typeof color === "string") {
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
		const icon = this.props.itemIcon;
		if (!icon) {
			return null;
		}
		if (typeof icon === "function") {
			return icon(item);
		}
		return icon;
	};

	getItemStyle = (item) => {
		const style = this.props.itemStyle;
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

	getItemActions = (item) => {
		const actions = this.props.itemActions;
		if (typeof actions === "function") {
			return actions(item);
		}
		return actions || [];
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
		const color = this.getItemColor(item);
		const size = this.getItemSize(item);
		const selected = this.props.selectedItem === item;
		const highlighted = this.props.highlightedItem === item;
		const weight =
			selected || highlighted ? "fontWeightBold" : "fontWeightRegular";
		const actions = this.getItemActions(item);
		return (
			<div style={style}>
				<StyledListItemButton
					disableGutters={divider}
					//autoFocus={selected}
					style={{
						paddingTop: 0,
						paddingBottom: 0,
						paddingLeft: 0,
						paddingRight: 0,
					}}
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
						style={{
							marginLeft: 2,
							marginBottom: 0,
							marginRight: 0,
							marginTop: 0,
						}}
						primary={
							<Typography
								noWrap
								component="div"
								style={{ color: color }}
							>
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
					<Box
						display="flex"
						alignItems="center"
						key={"box" + index}
						className="actionButtons"
						justifyContent="flex-end"
						mr={2}
					>
						{actions.map((action, j) => {
							const visible =
								action.visible === undefined ||
								(typeof action.visible == "boolean" &&
									action.visible) ||
								(typeof action.visible == "function" &&
									action.visible(item));
							return (
								<Box key={"box" + index + "action" + j}>
									{visible && action.icon && (
										<Tooltip
											title={action.label}
											placement="top"
										>
											<IconButton
												style={{
													width: ITEM_SIZE,
													height: ITEM_SIZE,
												}}
												key={
													"button" +
													index +
													"action" +
													j
												}
												color="inherit"
												size="small"
												onClick={(event) => {
													event.stopPropagation();
													action.handler(item);
												}}
											>
												{action.icon}
											</IconButton>
										</Tooltip>
									)}
									{visible && !action.icon && (
										<Link
											className="actionButton"
											component="button"
											variant="contained"
											size="small"
											sx={{ marginLeft: 1 }}
											onClick={(e) => {
												action.handler(item);
											}}
										>
											{action.label}
										</Link>
									)}
								</Box>
							);
						})}
					</Box>
				</StyledListItemButton>
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
		const loading = this.props.loading;
		const showFilter = !loading && enableFilter && filterEnabled;
		return (
			<Box style={{ height: showFilter ? "90%" : "100%" }}>
				{loading && (
					<Box ml={2} width="50%">
						<Skeleton animation="wave" />
						<Skeleton animation="wave" />
						<Skeleton animation="wave" />
						<Skeleton animation="wave" />
					</Box>
				)}
				{!loading && (
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
									outerElementType={
										CustomScrollbarsVirtualList
									}
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
				)}
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
