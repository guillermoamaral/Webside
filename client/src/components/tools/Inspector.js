import React, { Component } from "react";
import {
	Grid,
	Box,
	Paper,
	Breadcrumbs,
	Link,
	Typography,
} from "@material-ui/core";
import clsx from "clsx";
import { IDEContext } from "../IDEContext";
import ObjectTree from "../parts/ObjectTree";
import CustomTable from "../controls/CustomTable";
import CodeEditor from "../parts/CodeEditor";

class Inspector extends Component {
	static contextType = IDEContext;

	constructor(props) {
		super(props);
		const root = props.root;
		if (root) {
			root.slot = "self";
			root.path = [];
		}
		this.state = {
			objectTree: !root ? [] : [root],
			selectedObject: !root ? null : root,
		};
	}

	componentDidMount() {
		this.updateSlots(this.props.root);
	}

	async componentDidUpdate(prevProps) {
		const root = this.props.root;
		if (root && (!prevProps.root || root.id !== prevProps.root.id)) {
			root.slot = "self";
			root.path = [];
			await this.updateSlots(root);
			this.setState({ objectTree: [root], selectedObject: root });
		}
	}

	objectUrlPath(object) {
		let path = "";
		object.path.forEach((s) => (path = path + "/" + s));
		return path;
	}

	updateObject = async (object) => {
		if (object && object.dummy) {
			return;
		}
		try {
			const id = this.props.root.id;
			const path = this.objectUrlPath(object);
			const retrieved = await this.context.api.getObjectSlot(id, path);
			Object.assign(object, retrieved);
			await this.updateSlots(object);
		} catch (error) {
			this.context.reportError(error);
		}
	};

	updateSlots = async (object) => {
		if (!object || object.dummy) {
			return;
		}
		const id = this.props.root.id;
		const path = this.objectUrlPath(object);
		var slots;
		try {
			slots = object.indexable
				? await this.context.api.getObjectIndexedSlots(id, path)
				: await this.context.api.getObjectNamedSlots(id, path);
		} catch (error) {
			slots = [];
			this.context.reportError(error);
		}
		slots.forEach((slot) => {
			slot.path = [...object.path, slot.slot];
			// const dummy = {
			// 	...slot,
			// 	dummy: true,
			// 	slot: "self",
			// 	path: [],
			// };
			// slot.slots = [dummy];
		});
		if (slots.length > 0) {
			object.slots = slots;
		}
		this.setState({ objectTree: this.state.objectTree });
	};

	selectSlot = (path) => {
		let object = this.props.root;
		path.forEach(
			(name) => (object = object.slots.find((s) => s.slot === name))
		);
		this.setState({ selectedObject: object });
	};

	slotSelected = async (object) => {
		await this.updateObject(object);
		this.setState({ selectedObject: object });
	};

	slotExpanded = async (object) => {
		if (!object) return;
		await this.updateObject(object);
		this.setState({ objectTree: this.state.objectTree });
	};

	browseClass = (classname) => {
		this.context.browseClass(classname);
	};

	render() {
		const { objectTree, selectedObject } = this.state;
		const { styles, showWorkspace } = this.props;
		const fixedHeightPaper = clsx(styles.paper, styles.fixedHeight);
		const path = selectedObject ? selectedObject.path : [];
		const presentation = selectedObject ? selectedObject.presentation : null;
		return (
			<Grid container spacing={1}>
				<Grid item xs={12} md={12} lg={12}>
					<Box ml={2} display="flex" alignItems="center">
						<Breadcrumbs>
							{path.slice(0, path.length - 1).map((s, i) => {
								const subpath = path.slice(0, i + 1);
								return (
									<Link
										color="inherit"
										key={s}
										onClick={(event) => {
											this.selectSlot(subpath);
										}}
									>
										{s}
									</Link>
								);
							})}
							<Typography color="primary">
								{path[path.length - 1] || "self"}
							</Typography>
						</Breadcrumbs>
						<Box pl={1}>
							{selectedObject && (
								<Link
									color="inherit"
									onClick={(event) => {
										this.browseClass(selectedObject.class);
									}}
								>
									{"(" + selectedObject.class + ")"}
								</Link>
							)}
						</Box>
					</Box>
				</Grid>
				<Grid item xs={12} md={6} lg={6}>
					<Paper className={fixedHeightPaper} variant="outlined">
						<ObjectTree
							roots={objectTree}
							selected={selectedObject}
							onExpand={this.slotExpanded}
							onSelect={this.slotSelected}
						/>
					</Paper>
				</Grid>
				<Grid item xs={12} md={6} lg={6}>
					<Paper variant="outlined" style={{ height: "100%" }}>
						{presentation && presentation.type === "table" && (
							<CustomTable
								styles={styles}
								columns={presentation.columns}
								rows={presentation.rows}
							/>
						)}
						{presentation && presentation.type === "html" && (
							<div
								styles={styles}
								dangerouslySetInnerHTML={{
									__html: presentation.code,
								}}
							/>
						)}
						{!presentation && (
							<CodeEditor
								context={{ object: this.props.id }}
								styles={styles}
								lineNumbers={false}
								source={!selectedObject ? "" : selectedObject.printString}
								onChange={this.props.onChange}
								onAccept={this.props.onAccept}
							/>
						)}
					</Paper>
				</Grid>
				{showWorkspace && (
					<Grid item xs={12} md={12} lg={12}>
						<Paper variant="outlined" style={{ height: "100%" }}>
							<CodeEditor
								context={{ object: this.props.id }}
								styles={styles}
								lineNumbers={false}
							/>
						</Paper>
					</Grid>
				)}
			</Grid>
		);
	}
}

export default Inspector;
