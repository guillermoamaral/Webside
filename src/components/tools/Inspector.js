import React, { Component } from "react";
import { Grid, Box, Paper, Breadcrumbs, Link, Typography } from "@mui/material";
import { ide } from "../IDE";
import ToolContainerContext from "../ToolContainerContext";
import ObjectTree from "../parts/ObjectTree";
import ObjectPresenter from "../parts/ObjectPresenter";
import CodeEditor from "../parts/CodeEditor";

class Inspector extends Component {
	static contextType = ToolContainerContext;

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

	async componentDidMount() {
		const root = this.props.root;
		await this.updatePresentations(root);
		await this.updateSlots(root);
		this.setState({ objectTree: [root], selectedObject: root });
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
		object.path.forEach((s) => {
			if (s !== "yourself") {
				path = path + "/" + s;
			}
		});
		return path;
	}

	updateObject = async (object) => {
		if (object && object.self) {
			return this.updateObject(object.self);
		}
		try {
			const id = this.props.root.id;
			const path = this.objectURIPath(object);
			const retrieved = await ide.api.objectSlot(id, path);
			Object.assign(object, retrieved);
			await this.updatePresentations(object);
			await this.updateSlots(object);
		} catch (error) {
			ide.reportError(error);
		}
	};

	updatePresentations = async (object) => {
		try {
			const id = this.props.root.id;
			const path = this.objectURIPath(object);
			object.presentations = await ide.api.objectPresentations(id, path);
		} catch (error) {
			ide.reportError(error);
		}
	};

	selfSlot(object) {
		return {
			...object,
			self: object,
			slot: "self",
			slots: [],
			path: [...object.path, "yourself"],
		};
	}

	updateSlots = async (object) => {
		if (!object) {
			return;
		}
		const id = this.props.root.id;
		const path = this.objectURIPath(object);
		var slots;
		try {
			slots = object.indexable
				? await ide.api.objectIndexedSlots(id, path)
				: await ide.api.objectNamedSlots(id, path);
		} catch (error) {
			slots = [];
			ide.reportError(error);
		}
		const current = object.slots || [];
		if (slots.length > 0) {
			object.slots = slots.map((s) => {
				s.path = [...object.path, s.slot];
				const existing = current.find((e) => e.slot === s.slot);
				const slot = existing ? Object.assign(existing, s) : s;
				if (!slot.slots || slot.slots.length === 0) {
					s.slots = [this.selfSlot(s)];
				}
				return slot;
			});
		} else {
			const self = this.selfSlot(object);
			const existing = current.find((e) => e.slot === "self");
			if (existing) {
				Object.assign(existing, self);
			} else {
				object.slots = [self];
			}
		}
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
		if (path.length === 0) {
			return subpaths;
		}
		const length =
			path[path.length - 1] === "yourself"
				? path.length - 1
				: path.length;
		for (let l = 1; l <= length; l++) {
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
		const selectedObject = this.state.selectedObject;
		const path = selectedObject ? this.objectURIPath(selectedObject) : "";
		return { object: this.props.root.id + path };
	}

	assignEvaluation = async (expression) => {
		const selectedObject = this.state.selectedObject;
		if (!selectedObject || selectedObject === this.props.root) {
			return;
		}
		const path = this.objectURIPath(selectedObject);
		try {
			await this.context.evaluateExpression(
				expression,
				false,
				false,
				path,
				this.props.root.id + path
			);
		} catch (error) {
			this.setState({ selectedObject: selectedObject });
		}
	};

	render() {
		const { objectTree, selectedObject } = this.state;
		const { showWorkspace } = this.props;
		const minHeight = this.props.embedded ? 200 : 600;
		const path = selectedObject ? selectedObject.path : [];
		const subpaths = this.subpaths(path);
		return (
			<Grid container>
				<Grid item xs={12} md={12} lg={12}>
					<Box ml={2} display="flex" alignItems="center">
						<Breadcrumbs>
							{subpaths.map((subpath) => {
								const label =
									subpath.length === 0
										? "self"
										: subpath[subpath.length - 1];
								const color =
									label === path[path.length - 1]
										? "primary"
										: "inherit";
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
						<Box pl={1}>
							{selectedObject && selectedObject.indexable && (
								<Typography>
									{"[" + selectedObject.size + " elements]"}
								</Typography>
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
							selectedObject={selectedObject}
							onSlotExpand={this.slotExpanded}
							onSlotSelect={this.slotSelected}
						/>
					</Paper>
				</Grid>
				<Grid item xs={12} md={8} lg={8}>
					<Box
						display="flex"
						flexDirection="column"
						justifyContent="center"
						style={{ height: "100%" }}
					>
						<Box flexGrow={1}>
							<ObjectPresenter
								context={this.evaluationContext()}
								object={selectedObject}
								onAccept={this.assignEvaluation}
							/>
						</Box>
						<Box ml={1} mr={1}>
							{showWorkspace && (
								<Paper
									variant="outlined"
									style={{ minHeight: 100, height: "100%" }}
								>
									<CodeEditor
										context={this.evaluationContext()}
										onEvaluate={this.expressionEvaluated}
										onAccept={this.assignEvaluation}
									/>
								</Paper>
							)}
						</Box>
					</Box>
				</Grid>
			</Grid>
		);
	}
}

export default Inspector;
