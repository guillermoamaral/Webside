import { Component } from "react";
import { Paper, Box } from "@mui/material";
import CustomTable from "../controls/CustomTable";
import TabControl from "../controls/TabControl";
import CustomTree from "../controls/CustomTree";
import MarkdownView from "./MarkdownView";
import JSONView from "./JSONView";
import CodeEditorBackend from "./CodeEditorBackend";

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
						<CodeEditorBackend
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
		(object.views || []).forEach((v, i) => {
			const component = this.customView(v);
			if (component) {
				const title = v.title || `View-${i}`;
				const page = {
					id: title + i,
					label: title,
					component: component,
				};
				pages.push(page);
			}
		});
		return pages;
	}

	customView(view) {
		let component;
		let code;
		switch (view.type) {
			case "source":
				code = view.code || `"no code provided"`;
				component = (
					<CodeEditorBackend source={code} showAccept={false} />
				);
				break;
			case "markdown":
				code = view.code || `"no markdown provided"`;
				component = <MarkdownView source={code} />;
				break;
			case "json":
				code = view.code || "{}";
				component = <JSONView source={JSON.parse(code)} />;
				break;
			case "tree":
				component = (
					<CustomTree
						nodes={view.roots || []}
						selectedNode={view.roots ? view.roots[0] : []}
						nodeLabel={view.nodeLabel}
						nodeChildren={view.nodeChildren}
					/>
				);
				break;
			case "table":
				const rows = view.rows || [];
				const columns = view.columns || [];
				component =
					rows.length > 100 ? (
						<CustomTable
							columns={columns}
							rows={rows}
							rowsPerPage={50}
							usePagination
						/>
					) : (
						<CustomTable columns={columns} rows={rows} />
					);
				break;
			case "html":
				code = view.code || "";
				component = (
					<iframe
						title={view.title}
						srcDoc={code}
						height="100%"
						width="100%"
					/>
				);
				break;
			default:
		}
		return component;
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
		if (!selectedPage) selectedPage = pages[0];
		return (
			<Box
				sx={{
					width: "100%",
					height: "100%",
					display: "flex",
					flexDirection: "column",
				}}
			>
				<TabControl
					id={object.id || context.object}
					sx={{ height: "100%" }}
					selectedPage={selectedPage}
					pages={pages}
					onTabSelect={(p) => this.setState({ selectedId: p.id })}
					canCloseTabs={false}
					showTabSelector={false}
				/>
			</Box>
		);
	}
}

export default ObjectPresenter;
