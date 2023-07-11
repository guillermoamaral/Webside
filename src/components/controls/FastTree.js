import React, { useState, memo } from "react";
import { FixedSizeList as List, areEqual } from "react-window";
import {
	Box,
	ListItemButton,
	ListItemText,
	ListItemIcon,
	Typography,
} from "@mui/material";
import ArrowRight from "@mui/icons-material/ArrowRight";
import ArrowDown from "@mui/icons-material/ArrowDropDown";
import AutoSizer from "react-virtualized-auto-sizer";
import memoizeOne from "memoize-one";
import RSC from "react-scrollbars-custom";
import PopupMenu from "./PopupMenu";

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

const drawNode = memo(({ data, index, style }) => {
	const { flattenedData, toggleHandler, selectHandler, menuHandler } = data;
	const nodeInfo = flattenedData[index];
	const icon = nodeInfo.hasChildren ? (
		nodeInfo.collapsed ? (
			<ArrowRight />
		) : (
			<ArrowDown />
		)
	) : null;
	return (
		<div style={style}>
			<ListItemButton
				style={{
					paddingTop: 0,
					paddingBottom: 0,
					paddingLeft: nodeInfo.depth * 20,
					paddingRight: 0,
				}}
				key={"node" + index}
				selected={nodeInfo.selected}
				onClick={(event) => selectHandler(event, nodeInfo)}
				onDoubleClick={(event) => toggleHandler(event, nodeInfo)}
				onContextMenu={menuHandler}
			>
				<Box display="flex" alignItems="center">
					<Box p={0} style={{ width: 20 }}>
						{icon && (
							<ListItemIcon
								style={{ minWidth: 0 }}
								onClick={(event) =>
									toggleHandler(event, nodeInfo)
								}
							>
								{icon}
							</ListItemIcon>
						)}
					</Box>
					<ListItemText
						primary={
							<Typography
								noWrap
								component="div"
								//style={{ color: node.color }}
							>
								<Box
									fontWeight={
										nodeInfo.selected
											? "fontWeightBold"
											: "fontWeightRegular"
									}
									fontStyle={nodeInfo.style}
									// fontSize={size}
									// align={alignment}
								>
									{nodeInfo.label}
								</Box>
							</Typography>
						}
					/>
				</Box>
			</ListItemButton>
		</div>
	);
}, areEqual);

const getNodeData = memoizeOne(
	(toggleHandler, selectHandler, flattenedData, menuHandler) => ({
		toggleHandler,
		selectHandler,
		flattenedData,
		menuHandler,
	})
);

const FastTree = ({
	nodes,
	nodeId,
	nodeLabel,
	nodeChildren,
	nodeStyle,
	menuOptions,
	onNodeExpand,
	onNodeSelect,
	selectedNode,
}) => {
	const [expandedIds, setExpandedIds] = useState([]);
	const [menuOpen, setMenuOpen] = useState(false);
	const [menuPosition, setMenuPosition] = useState({ x: null, y: null });
	const [selected, setSelected] = useState();

	const getNodeId = (node) => {
		if (!node) return null;
		if (!nodeId) {
			return node.id || getNodeLabel(node);
		}
		if (typeof nodeId == "string") {
			return node[nodeId];
		}
		return nodeId(node);
	};

	const getNodeLabel = (node) => {
		if (!nodeLabel) {
			return node.label;
		}
		if (typeof nodeLabel == "string") {
			return node[nodeLabel];
		}
		return nodeLabel(node);
	};

	const getNodeStyle = (node) => {
		if (!nodeStyle) {
			return "normal";
		}
		if (typeof nodeStyle == "string") {
			return nodeStyle;
		}
		return nodeStyle(node);
	};

	const getNodeChildren = (node) => {
		if (!nodeChildren) {
			return node.children;
		}
		if (typeof nodeChildren == "string") {
			return node[nodeChildren];
		}
		return nodeChildren(node);
	};

	const flattenNodes = (data) => {
		const flatten = [];
		for (let node of data) {
			flattenNode(node, 0, flatten);
		}
		return flatten;
	};

	const nodeInfo = (node) => {
		const id = getNodeId(node);
		const label = getNodeLabel(node);
		const children = getNodeChildren(node);
		const style = getNodeStyle(node);
		return {
			id,
			label,
			children,
			hasChildren: children && children.length > 0,
			style,
			node,
		};
	};

	const flattenNode = (node, depth, result) => {
		const info = nodeInfo(node);
		info.depth = depth;
		info.collapsed = !expandedIds.includes(info.id);
		info.selected = selected
			? selected.id === info.id
			: selectedNode && getNodeId(selectedNode) === info.id;
		result.push(info);
		if (!info.collapsed && info.hasChildren) {
			for (let child of getNodeChildren(node)) {
				flattenNode(child, depth + 1, result);
			}
		}
	};

	const toggleHandler = (event, nodeInfo) => {
		event.stopPropagation();
		if (nodeInfo.collapsed) {
			setExpandedIds([...expandedIds, nodeInfo.id]);
			if (onNodeExpand) {
				onNodeExpand(nodeInfo.node);
			}
		} else {
			setExpandedIds(expandedIds.filter((id) => id !== nodeInfo.id));
		}
	};

	const selectHandler = (event, nodeInfo) => {
		event.stopPropagation();
		setSelected(nodeInfo);
		if (onNodeSelect) {
			onNodeSelect(nodeInfo.node);
		}
	};

	const menuHanlder = (event) => {
		event.preventDefault();
		setMenuOpen(true);
		setMenuPosition({ x: event.clientX - 2, y: event.clientY - 4 });
	};

	const menuOptionClicked = (option) => {
		if (option.action) {
			option.action(selectedNode);
		}
	};

	const getMenuOptionEnabled = (option) => {
		if (option.enabled) {
			return option.enabled(selectedNode);
		}
		return true;
	};

	const flattenedData = flattenNodes(nodes);

	const itemData = getNodeData(
		toggleHandler,
		selectHandler,
		flattenedData,
		menuHanlder
	);

	const listRef = React.createRef();

	return (
		<Box style={{ height: "100%", width: "100%" }}>
			<AutoSizer>
				{({ height, width }) => (
					<List
						ref={listRef}
						className="List"
						height={height}
						width={width}
						itemCount={flattenedData.length}
						itemSize={ITEM_SIZE}
						itemKey={(index) => flattenedData[index].id}
						itemData={itemData}
						outerElementType={CustomScrollbarsVirtualList}
					>
						{drawNode}
					</List>
				)}
			</AutoSizer>
			{menuOptions && (
				<PopupMenu
					options={menuOptions}
					open={menuOpen}
					position={menuPosition}
					onOptionClick={menuOptionClicked}
					onOptionEnable={getMenuOptionEnabled}
					onClose={() => setMenuOpen(false)}
				/>
			)}
		</Box>
	);
};

export default FastTree;
