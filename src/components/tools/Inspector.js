import React from "react";
import Tool from "./Tool";
import { Box, Paper, Breadcrumbs, Link, Typography } from "@mui/material";
import { ide } from "../IDE";
import ObjectTree from "../parts/ObjectTree";
import ObjectPresenter from "../parts/ObjectPresenter";
import CodeEditor from "../parts/CodeEditor";
import CustomSplit from "../controls/CustomSplit";

class Inspector extends Tool {
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
			expandedSlots: root ? [root] : [],
		};
	}

	async aboutToClose() {
		try {
			await ide.backend.unpinObject(this.props.root.id);
		} catch (error) {
			ide.reportError(error);
		}
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
			const retrieved = await ide.backend.objectSlot(id, path);
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
			object.presentations = await ide.backend.objectPresentations(
				id,
				path
			);
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
			slots = [];
			if (object.hasNamedSlots) {
				const named = await ide.backend.objectNamedSlots(id, path);
				slots = slots.concat(named);
			}
			if (object.hasIndexedSlots) {
				const indexed = await ide.backend.objectIndexedSlots(id, path);
				slots = slots.concat(indexed);
			}
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

	selectSlotAtPath = async (path) => {
		let object = this.props.root;
		let expanded = this.state.expandedSlots;
		path.forEach((name) => {
			if (!expanded.includes(object)) expanded.push(object);
			object = object.slots.find((s) => s.slot === name);
		});
		await this.updateObject(object);
		this.setState({ expandedSlots: expanded, selectedObject: object });
	};

	slotSelected = async (object) => {
		await this.updateObject(object);
		this.setState({ selectedObject: object });
	};

	slotExpanded = async (object) => {
		if (!object) return;
		await this.updateObject(object);
		this.setState({ expandedSlots: [...this.state.expandedSlots, object] });
	};

	slotCollapsed = (object) => {
		const expanded = this.state.expandedSlots;
		expanded.splice(expanded.indexOf(object), 1);
		this.setState({ expandedSlots: expanded });
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
		const { objectTree, selectedObject, expandedSlots } = this.state;
		const { showWorkspace } = this.props;
		const minHeight = this.props.embedded ? 200 : 600;
		const path = selectedObject ? selectedObject.path : [];
		const subpaths = this.subpaths(path);
		const background = ide.colorSetting("inspectorColor");
		return (
			<Box
				display="flex"
				flexDirection="column"
				sx={{
					height: "100%",
					background: background,
					padding: 1,
				}}
			>
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
										this.selectSlotAtPath(subpath);
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
						{selectedObject && selectedObject.hasIndexedSlots && (
							<Typography>
								{"[" + selectedObject.size + " elements]"}
							</Typography>
						)}
					</Box>
				</Box>
				<Box flexGrow={1}>
					<CustomSplit>
						<Box sx={{ width: "40%" }}>
							<Paper
								variant="outlined"
								style={{ height: "100%", minHeight: minHeight }}
							>
								<ObjectTree
									roots={objectTree}
									selectedObject={selectedObject}
									onSlotSelect={this.slotSelected}
									onSlotExpand={this.slotExpanded}
									onSlotCollapse={this.slotCollapsed}
									expandedSlots={expandedSlots}
								/>
							</Paper>
						</Box>
						<Box sx={{ width: "60%" }}>
							<CustomSplit mode="vertical">
								<Box sx={{ height: "80%" }}>
									<ObjectPresenter
										context={this.evaluationContext()}
										object={selectedObject}
										onAccept={this.assignEvaluation}
										onSlotSelect={(o) =>
											this.selectSlotAtPath(o.path)
										}
									/>
								</Box>
								<Box sx={{ height: "20%" }}>
									{showWorkspace && (
										<Paper
											variant="outlined"
											sx={{ height: "100%" }}
										>
											<CodeEditor
												context={this.evaluationContext()}
												onEvaluate={
													this.expressionEvaluated
												}
												onAccept={this.assignEvaluation}
											/>
										</Paper>
									)}
								</Box>
							</CustomSplit>
						</Box>
					</CustomSplit>
				</Box>
			</Box>
		);
	}
}

export default Inspector;
