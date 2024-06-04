import React, { Component } from "react";
import { Paper } from "@mui/material";
import CustomTable from "../controls/CustomTable";
import CodeEditor from "../parts/CodeEditor";
import TabControl from "../controls/TabControl";
import FastTree from "../controls/FastTree";
import MarkdownView from "./MarkdownView";
import JSONView from "./JSONView";

class ObjectPresenter extends Component {
	constructor(props) {
		super(props);
		this.state = {
			selectedId: null,
		};
	}

	// static getDerivedStateFromProps(props, state) {
	// 	if (
	// 		props.object &&
	// 		props.object.presentation &&
	// 		state.pages.length === 1
	// 	) {
	// 		return {
	// 			pages: [
	// 				...state.pages,
	// 				{
	// 					id: "custom",
	// 					label: props.object.presentation.title,
	// 					icon: null,
	// 					component: null,
	// 				},
	// 			],
	// 			selectedId: "custom",
	// 		};
	// 	}
	// 	if (!props.object || !props.object.presentation) {
	// 		return {
	// 			pages: state.pages.slice(0, 1),
	// 			selectedId: "raw",
	// 		};
	// 	}
	// 	return null;
	// }

	pages() {
		const { object, context, onAccept } = this.props;
		if (!object) return [];
		const pages = [
			{
				id: "raw",
				label: "Raw",
				component: (
					<Paper variant="outlined" style={{ height: "100%" }}>
						<CodeEditor
							context={context}
							source={!object ? "" : object.printString}
							onAccept={onAccept}
						/>
					</Paper>
				),
			},
		];
		if (object.hasIndexedSlots && object.slots) {
			pages.push({
				id: "items",
				label: "Items",
				component: (
					<CustomTable
						columns={[
							{
								field: "slot",
								label: "#",
								align: "left",
								link: this.indexedSlotSelected,
							},
							{
								field: "printString",
								label: "Value",
								align: "left",
								link: this.indexedSlotSelected,
							},
						]}
						rows={object.slots}
						rowsPerPage={50}
						usePagination
					/>
				),
			});
		}
		(object.presentations || []).forEach((p, i) => {
			const page = {
				id: p.title + i,
				label: p.title,
				component: (
					<Paper variant="outlined" style={{ height: "100%" }}>
						{p.type === "source" && (
							<CodeEditor source={p.code} showAccept={false} />
						)}
						{p.type === "markdown" && (
							<MarkdownView source={p.code} />
						)}
						{p.type === "json" && (
							<JSONView source={JSON.parse(p.code)} />
						)}
						{p.type === "tree" && (
							<FastTree
								nodes={p.roots}
								selectedNode={p.roots[0]}
								nodeLabel={p.nodeLabel}
								nodeChildren={p.nodeChildren}
							/>
						)}
						{p.type === "table" && p.rows.length > 100 && (
							<CustomTable
								columns={p.columns}
								rows={p.rows}
								rowsPerPage={50}
								usePagination
							/>
						)}
						{p.type === "table" && p.rows.length <= 100 && (
							<CustomTable columns={p.columns} rows={p.rows} />
						)}
						{p.type === "html" && (
							<iframe
								title={p.title}
								srcDoc={p.code}
								height="100%"
								width="100%"
							/>
						)}
					</Paper>
				),
			};
			pages.push(page);
		});
		return pages;
	}

	indexedSlotSelected = (object) => {
		if (this.props.onSlotSelect) this.props.onSlotSelect(object);
	};

	render() {
		const { object, context } = this.props;
		const { selectedId } = this.state;
		const pages = this.pages();
		var selectedPage = selectedId
			? pages.find((p) => p.id === selectedId)
			: pages.length > 0
			? pages[1]
			: pages[0];
		if (!selectedPage) {
			selectedPage = pages[0];
		}
		return (
			<TabControl
				id={object.id || context.object}
				style={{ height: "100%" }}
				selectedPage={selectedPage}
				pages={pages}
				onTabSelect={(p) => this.setState({ selectedId: p.id })}
				showTabClose={false}
			/>
		);
	}
}

export default ObjectPresenter;
