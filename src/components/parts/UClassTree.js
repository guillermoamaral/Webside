import React, { Component } from "react";
import { ide } from "../IDE";
import ToolContainerContext from "../ToolContainerContext";
import FastTree from "../controls/FastTree";
import { Box, IconButton, Tooltip } from "@mui/material";
import SearchList2 from "../controls/SearchList2";
import UpIcon from "@mui/icons-material/ArrowDropUp";
import CustomPaper from "../controls/CustomPaper";

class UClassTree extends Component {
	static contextType = ToolContainerContext;

	constructor(props) {
		super(props);
		this.state = {
			roots: [],
			selectedClass: null,
			expandedClasses: [],
			loading: false,
			preselectedClass: null,
		};
	}

	static getDerivedStateFromProps(props, state) {
		if (props.preselectedClass !== state.preselectedClass) {
			return {
				preselectedClass: props.preselectedClass,
			};
		}
		return null;
	}

	componentDidMount() {
		this.changeRoots(this.props.roots, this.props.package, this.props.preselectedClass);
	}

	componentDidUpdate(prevProps) {
		if (this.props.roots !== prevProps.roots || this.props.package !== prevProps.package) {
			this.changeRoots(this.props.roots, this.props.package, this.state.selectedClass);
		}
	}

	goToRoot = (name) => {
		if (name) {
			this.changeRoots([{ name: name }], null, this.state.selectedClass)
		}
	}

	async refreshEnsuring(species) {
		this.setState({ loading: true });
		let pack = this.props.package;
		let trees = await this.fetchClasses(this.state.roots, pack);
		let selected;
		trees.forEach(root => selected = this.findSubclass(species.name, root));
		if (!selected && !pack) {
			return this.goToRoot(species.name)
		}
		let expanded = [];
		let found;
		this.state.expandedClasses.forEach(c => {
			found = null;
			trees.forEach(root => found = this.findSubclass(c.name, root));
			if (found) { expanded.push(found) };
		});
		found = null;
		trees.forEach(root => found = this.findSubclass(species.superclass, root));
		if (found) { expanded.push(found) };
		this.setState({
			roots: trees,
			expandedClasses: expanded,
			selectedClass: selected,
			loading: false,
		});
	}

	changeRoots = async (classes, pack, selectedClass) => {
		this.setState({ loading: true });
		let trees = await this.fetchClasses(classes, pack);
		let selected;
		if (selectedClass) {
			trees.forEach(root => selected = this.findSubclass(selectedClass.name, root));
		}
		this.setState({
			roots: trees,
			expandedClasses: [...trees],
			selectedClass: selected,
			loading: false,
		});
	};

	async fetchClasses(roots, pack) {
		let trees;
		if (pack) {
			trees = await this.fetchPackageSubtrees(pack);
		} else {
			trees = roots ? await this.fetchSubtrees(roots) : [];
		}
		return trees;
	}

	async fetchPackageSubtrees(pack) {
		let trees = [];
		if (!pack) return trees;
		try {
			trees = await ide.backend.packageClasses(pack.name, true);
		} catch (error) {
			ide.reportError(error);
		};
		return trees;
	}

	async fetchSubtrees(roots) {
		let trees = [];
		try {
			await Promise.all(
				roots.map(async root => {
					let tree = await ide.backend.classTree(root.name, 100, true);
					trees.push(tree);
				}))
		} catch (error) {
			ide.reportError(error);
		};
		return trees;
	}

	findSubclass(name, root) {
		let found;
		if (!root) {
			let roots = this.state.roots;
			for (let i = 0; i < roots.length; i++) {
				found = this.findSubclass(name, roots[i]);
				if (found) return found;
			}
			return null;
		}
		if (root.name === name) return root;
		let subclasses = root.subclasses;
		if (!subclasses) return null;
		for (let i = 0; i < subclasses.length; i++) {
			let c = subclasses[i];
			if (c.name === name) {
				return c;
			}
			found = this.findSubclass(name, c);
			if (found) return found;
		}
		return null;
	}

	async updateClass(species) {
		try {
			let retrieved = await ide.backend.classNamed(species.name);
			Object.assign(species, retrieved);
			species.metaclass = await ide.backend.classNamed(species.class);
		} catch (error) {
			ide.reportError(error);
		}
	}

	classSelected = async (species) => {
		await this.updateClass(species);
		this.setState({ selectedClass: species })
		if (this.props.onClassSelect) { this.props.onClassSelect(species) }
	};


	classExpanded = async (species) => {
		await this.updateClass(species);
		this.setState({ expandedClasses: [...this.state.expandedClasses, species] })
		if (this.props.onClassExpand) { this.props.onClassExpand(species) }
	};

	classCollapsed = (species) => {
		const expanded = this.state.expandedClasses;
		expanded.splice(expanded.indexOf(species), 1);
		this.setState({ expandedClasses: expanded })
		if (this.props.onClassCollapse) { this.props.onClassCollapse(species) }
	};

	newClass = async (superclass) => {
		if (!superclass) {
			return;
		}
		try {
			const name = await ide.prompt({
				title: "New " + superclass.name + " subclass",
				required: true,
			});
			await ide.backend.defineClass(
				name,
				superclass.name,
				superclass.package
			);
			const species = await ide.backend.classNamed(name);
			let expanded = this.state.expandedClasses;
			if (!expanded.find(c => c.name === superclass.name)) { expanded.push(superclass) }
			superclass.subclasses.push(species);
			superclass.subclasses.sort((a, b) => (a.name <= b.name ? -1 : 1));
			this.setState({ expandedClasses: expanded, selectedClass: species })
			if (this.props.onClassDefine) {
				this.props.onClassDefine(species);
			}
		} catch (error) {
			ide.reportError(error);
		}
	};

	renameClass = async (species) => {
		if (!species) {
			return;
		}
		try {
			const newName = await ide.prompt({
				title: "Rename class",
				defaultValue: species.name,
				required: true,
			});
			await ide.waitFor(() => ide.backend.renameClass(species.name, newName));
			species.name = newName;
			this.setState({ selectedClass: species })
			if (this.props.onClassRename) {
				this.props.onClassRename(species);
			}
		} catch (error) {
			ide.reportError(error);
		}
	};

	removeClass = async (species) => {
		if (!species) {
			return;
		}
		try {
			const confirm = await ide.confirm({
				title: "Delete " + species.name + " class?",
				ok: { text: "Delete", color: "secondary", variant: "outlined" },
			});
			if (!confirm) {
				return;
			}
			await ide.backend.removeClass(species.name);
			let expanded = this.state.expandedClasses;
			let index = expanded.indexOf(species);
			if (index > -1) { expanded.splice(index, 1) }
			let superclass = this.findSubclass(species.superclass);
			let selected;
			if (superclass) {
				superclass.subclasses = superclass.subclasses.filter(c => c.name !== species.name);
				selected = superclass;
			}
			this.setState({ roots: this.state.roots, expandedClasses: expanded, selectedClass: selected })
			if (this.props.onClassRemove) { this.props.onClassRemove(species); }
			if (this.props.onClassSelect) { this.props.onClassSelect(selected); }
		} catch (error) {
			ide.reportError(error);
		}
	};

	browseClass = (species) => {
		if (species) {
			this.context.browseClass(species.name);
		}
	};

	browsePackage = (species) => {
		if (species) {
			this.context.browsePackage(species.package);
		}
	};

	browseClassReferences = (species) => {
		if (species) {
			this.context.browseClassReferences(species.name);
		}
	};

	runTests = (species) => {
		if (species) {
			this.context.runTestClass(species.name);
		}
	};

	migrateClass = (species) => {
		if (species) {
			this.context.migrateClass(species.name);
		}
	};

	menuOptions() {
		return [
			{ label: "New", action: this.newClass },
			{ label: "Rename", action: this.renameClass },
			{ label: "Remove", action: this.removeClass },
			null,
			{ label: "Browse", action: this.browseClass },
			{ label: "Browse package", action: this.browsePackage },
			{ label: "Browse references", action: this.browseClassReferences },
			null,
			{ label: "Run tests", action: this.runTests },
			null,
			{ label: "Migrate", action: this.migrateClass },
		];
	}

	render() {
		let { showSearch, labelStyle } = this.props;
		if (showSearch === undefined) showSearch = true;
		let { roots, selectedClass, expandedClasses, loading } = this.state;
		let appearance = ide.settings.section("appearance");
		let mode = appearance.section(appearance.get("mode"));
		let background = mode.section("colors").get("background");
		let root = roots.length === 1 ? roots[0] : null;
		return (
			<Box display="flex" flexDirection="column" height="100%">
				{showSearch && <Box display="flex" flexDirection="row" width="100%">
					<Box mb={1} flexGrow={1}>
						<SearchList2
							value={
								selectedClass
									? selectedClass.name
									: null
							}
							options={async (value) => {
								return ide.backend.searchClassNames(value);
							}}
							backColor={background}
							onChange={this.goToRoot}
						/>
					</Box>
					{root && <Box>
						<Tooltip
							title={root.superclass || ""}
							placement="top"
						>
							<IconButton
								color="inherit"
								size="small"
								onClick={() => this.goToRoot(root.superclass)}
								disabled={!root.superclass}
							>
								<UpIcon />
							</IconButton>
						</Tooltip>
					</Box>}
				</Box>}
				<Box flexGrow={1}>
					<CustomPaper>
						<FastTree
							loading={loading}
							nodes={roots}
							nodeId="name"
							nodeLabel="name"
							nodeStyle={labelStyle}
							nodeChildren="subclasses"
							selectedNode={selectedClass}
							onNodeSelect={this.classSelected}
							expandedNodes={expandedClasses}
							onNodeExpand={this.classExpanded}
							onNodeCollapse={this.classCollapsed}
							menuOptions={this.menuOptions()}
						/>
					</CustomPaper>
				</Box>
			</Box>
		);
	}
}
export default UClassTree;
