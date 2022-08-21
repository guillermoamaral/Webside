import React, { Component } from "react";
import { Grid, Box, Paper, Breadcrumbs, Link } from "@material-ui/core";
import { IDEContext } from "../IDEContext";
import ObjectTree from "../parts/ObjectTree";
import ObjectPresenter from "../parts/ObjectPresenter";
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

	objectURIPath(object) {
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
			const path = this.objectURIPath(object);
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
		const path = this.objectURIPath(object);
		var slots;
		try {
			slots = object.indexable
				? await this.context.api.getObjectIndexedSlots(id, path)
				: await this.context.api.getObjectNamedSlots(id, path);
		} catch (error) {
			slots = [];
			this.context.reportError(error);
		}
		slots = slots.map((retrieved) => {
			retrieved.path = [...object.path, retrieved.slot];
			// const dummy = {
			// 	...retrieved,
			// 	dummy: true,
			// 	slot: "self",
			// 	path: [],
			// };
			// retrieved.slots = [dummy];
			const existing = (object.slots || []).find(
				(s) => s.slot === retrieved.slot
			);
			return existing ? Object.assign(existing, retrieved) : retrieved;
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

	subpaths(path) {
		const subpaths = [[]];
		for (let l = 1; l <= path.length; l++) {
			subpaths.push(path.slice(0, l));
		}
		return subpaths;
	}

	expressionEvaluated = async () => {
		const selected = this.state.selectedObject;
		await this.updateObject(selected);
		this.setState({ selectedObject: selected });
	};

	evaluationContext() {
		return { object: this.props.root.id };
	}

	render() {
		const { objectTree, selectedObject } = this.state;
		const { root, styles, showWorkspace } = this.props;
		const minHeight = this.props.embedded ? 200 : 400;
		const path = selectedObject ? selectedObject.path : [];
		const subpaths = this.subpaths(path);
		return (
			<Grid container spacing={1}>
				<Grid item xs={12} md={12} lg={12}>
					<Box ml={2} display="flex" alignItems="center">
						<Breadcrumbs>
							{subpaths.map((subpath) => {
								const label =
									subpath.length === 0 ? "self" : subpath[subpath.length - 1];
								const color =
									label === path[path.length - 1] ? "primary" : "inherit";
								return (
									<Link
										style={{
											cursor: "pointer",
										}}
										color={color}
										key={label}
										onClick={(event) => {
											this.selectSlot(subpath);
										}}
									>
										{label}
									</Link>
								);
							})}
						</Breadcrumbs>
						<Box pl={1}>
							{selectedObject && (
								<Link
									style={{
										cursor: "pointer",
									}}
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
				<Grid item xs={12} md={4} lg={4}>
					<Paper
						variant="outlined"
						style={{ height: "100%", minHeight: minHeight }}
					>
						<ObjectTree
							roots={objectTree}
							selected={selectedObject}
							onExpand={this.slotExpanded}
							onSelect={this.slotSelected}
						/>
					</Paper>
				</Grid>
				<Grid item xs={12} md={8} lg={8}>
					<ObjectPresenter
						styles={styles}
						root={root}
						object={selectedObject}
					/>
				</Grid>
				{showWorkspace && (
					<Grid item xs={12} md={12} lg={12}>
						<Paper
							variant="outlined"
							style={{ minHeight: 100, height: "100%" }}
						>
							<CodeEditor
								context={this.evaluationContext()}
								styles={styles}
								lineNumbers={false}
								onEvaluate={this.expressionEvaluated}
							/>
						</Paper>
					</Grid>
				)}
			</Grid>
		);
	}
}

export default Inspector;
