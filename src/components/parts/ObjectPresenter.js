import React, { Component } from "react";
import { Paper } from "@mui/material";
import CustomTable from "../controls/CustomTable";
import CodeEditor from "../parts/CodeEditor";
import TabControl from "../controls/TabControl";

class ObjectPresenter extends Component {
	constructor(props) {
		super(props);
		const pages = [
			{
				id: "raw",
				label: "Raw",
				icon: null,
				component: null,
			},
		];
		this.state = {
			pages: pages,
			selectedId: "raw",
		};
	}

	static getDerivedStateFromProps(props, state) {
		if (
			props.object &&
			props.object.presentation &&
			state.pages.length === 1
		) {
			return {
				pages: [
					...state.pages,
					{
						id: "custom",
						label: props.object.presentation.title,
						icon: null,
						component: null,
					},
				],
				selectedId: "custom",
			};
		}
		if (!props.object || !props.object.presentation) {
			return {
				pages: state.pages.slice(0, 1),
				selectedId: "raw",
			};
		}
		return null;
	}

	render() {
		const { object, context, onAccept } = this.props;
		const { selectedId, pages } = this.state;
		const selectedPage = pages.find((p) => p.id === selectedId);
		pages[0].component = (
			<Paper variant="outlined" style={{ height: "100%" }}>
				<CodeEditor
					context={context}
					lineNumbers={false}
					source={!object ? "" : object.printString}
					onAccept={onAccept}
				/>
			</Paper>
		);
		if (object && object.presentation) {
			const presentation = object.presentation;
			const custom = pages.find((p) => p.id === "custom");
			if (custom) {
				custom.component = (
					<Paper variant="outlined" style={{ height: "100%" }}>
						{presentation.type === "table" &&
							presentation.rows.length > 100 && (
								<CustomTable
									columns={presentation.columns}
									rows={presentation.rows}
									rowsPerPage={50}
									usePagination
								/>
							)}
						{presentation.type === "table" &&
							presentation.rows.length <= 100 && (
								<CustomTable
									columns={presentation.columns}
									rows={presentation.rows}
								/>
							)}
						{presentation.type === "html" && (
							<iframe
								title={presentation.title}
								srcdoc={presentation.code}
								height="100%"
								width="100%"
							/>
						)}
					</Paper>
				);
			}
		}
		return (
			<TabControl
				id={object.id}
				style={{ height: "100%" }}
				selectedPage={selectedPage}
				pages={pages}
				onTabSelect={(p) => this.setState({ selectedId: p.id })}
				noClose
			/>
		);
	}
}

export default ObjectPresenter;
