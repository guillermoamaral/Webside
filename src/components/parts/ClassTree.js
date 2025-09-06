import { Component } from "react";
import { ide } from "../IDE";
import ToolContainerContext from "../ToolContainerContext";
import CustomTree from "../controls/CustomTree";
import { Box, IconButton, Tooltip } from "@mui/material";
import SearchList2 from "../controls/SearchList2";
import UpIcon from "@mui/icons-material/ArrowDropUp";
import CustomPaper from "../controls/CustomPaper";

class ClassTree extends Component {
	static contextType = ToolContainerContext;

	constructor(props) {
		super(props);
		this.classSearch = null;
		this.state = {
			roots: [],
			selectedClass: null,
			expandedClasses: [],
			loading: false,
			showSearch: true,
			extendedOptions: [],
		};
	}

	async componentDidMount() {
		await this.initializeClassSearch();
		this.updateExtendedOptions();
		const expanded = this.props.selectedClass
			? [this.props.selectedClass]
			: [];
		this.updateClasses(
			this.props.roots,
			this.props.package,
			this.props.category,
			this.props.selectedClass,
			expanded
		);
	}

	async initializeClassSearch() {
		try {
			//This is just a check to detect if there is a /search endpoint to use.
			//In that case, options are dynamically fetch using such endpoint.
			await ide.backend.searchClassNames("Object");
			this.classSearch = this.searchClass;
		} catch (error) {
			//In case there is no /search endpoint, we keep the list of class names
			//in the system (at the moment of initialization).
			try {
				this.classSearch = await ide.backend.classNames();
			} catch (ignored) {
				this.classSearch = [];
			}
		}
	}

	componentDidUpdate(prevProps) {
		let selected = this.props.selectedClass || this.state.selectedClass;
		if (
			this.props.roots !== prevProps.roots ||
			this.props.package !== prevProps.package ||
			this.props.category !== prevProps.category
		) {
			return this.updateClasses(
				this.props.roots,
				this.props.package,
				this.props.category,
				selected,
				this.state.expandedClasses
			);
		}
		if (prevProps.selectedClass !== this.props.selectedClass) {
			this.setState({
				selectedClass: this.props.selectedClass,
			});
		}
	}

	goToRoot = (name) => {
		if (name) {
			const target = { name: name };
			this.updateClasses([target], null, null, target, [
				...this.state.expandedClasses,
				target,
			]);
		}
	};

	async refreshEnsuring(species) {
		this.setState({ loading: true });
		let pack = this.props.package;
		let trees = await this.fetchClasses(
			this.state.roots,
			pack,
			this.props.category
		);
		let selected;
		trees.forEach(
			(root) => (selected = this.findSubclass(species.name, root))
		);
		if (!selected && !pack) {
			return this.goToRoot(species.name);
		}
		let expanded = [];
		let found;
		this.state.expandedClasses.forEach((c) => {
			found = trees.forEach((root) => {
				found = this.findSubclass(c.name, root);
				if (found) expanded.push(found);
			});
		});
		found = null;
		trees.forEach(
			(root) => (found = this.findSubclass(species.superclass, root))
		);
		if (found) {
			expanded.push(found);
		}
		this.setState({
			roots: trees,
			expandedClasses: expanded,
			selectedClass: selected,
			loading: false,
		});
	}

	updateClasses = async (
		roots,
		pack,
		category,
		selectedClass,
		expandedClasses
	) => {
		this.setState({ loading: true });
		let trees = await this.fetchClasses(roots, pack, category);
		let selected;
		if (selectedClass) {
			trees.forEach(
				(root) =>
					(selected = this.findSubclass(selectedClass.name, root))
			);
		}
		if (selected && this.props.onClassSelect)
			this.props.onClassSelect(selected);
		let expanded = [];
		let found;
		expandedClasses.forEach((c) => {
			found = trees.forEach((root) => {
				found = this.findSubclass(c.name, root);
				if (found) expanded.push(found);
			});
		});
		this.setState({
			roots: trees,
			expandedClasses: expanded,
			selectedClass: selected,
			loading: false,
		});
	};

	async fetchClasses(roots, pack, category) {
		let trees = [];
		try {
			if (pack) {
				trees = await this.fetchPackageSubtrees(pack, category);
			} else {
				trees = roots ? await this.fetchSubtrees(roots) : [];
			}
		} catch (error) {
			ide.reportError(error);
		}
		return trees;
	}

	async fetchPackageSubtrees(pack, category) {
		let trees = [];
		if (!pack) return trees;
		try {
			trees = await ide.backend.packageClasses(
				pack.name,
				true,
				category?.name
			);
		} catch (error) {
			ide.reportError(error);
		}
		return trees;
	}

	async fetchSubtrees(roots) {
		let trees = [];
		try {
			await Promise.all(
				roots.map(async (root) => {
					let tree = await ide.backend.classTree(
						root.name,
						100,
						true
					);
					trees.push(tree);
				})
			);
		} catch (error) {
			ide.reportError(error);
		}
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
		if (species.template) return;
		try {
			let retrieved = await ide.backend.classNamed(species.name);
			Object.assign(species, retrieved);
			if (species.subclasses)
				species.subclasses.sort((a, b) => (a.name <= b.name ? -1 : 1));
			species.metaclass = await ide.backend.classNamed(species.class);
		} catch (error) {
			ide.reportError(error);
		}
	}

	async updateExtendedOptions() {
		const options = await ide.fetchExtendedOptions("class");
		this.setState({ extendedOptions: options });
	}

	searchClass = async (text) => {
		let names;
		try {
			names = ide.backend.searchClassNames(text);
		} catch (ignored) {}
		return names;
	};

	classSelected = async (species) => {
		await this.updateClass(species);
		this.setState({ selectedClass: species });
		if (this.props.onClassSelect) this.props.onClassSelect(species);
		this.updateExtendedOptions();
	};

	classExpanded = async (species) => {
		await this.updateClass(species);
		this.setState({
			expandedClasses: [...this.state.expandedClasses, species],
		});
		if (this.props.onClassExpand) this.props.onClassExpand(species);
	};

	classCollapsed = (species) => {
		const expanded = this.state.expandedClasses;
		expanded.splice(expanded.indexOf(species), 1);
		this.setState({ expandedClasses: expanded });
		if (this.props.onClassCollapse) this.props.onClassCollapse(species);
	};

	newClass = async () => {
		const species = this.state.selectedClass;
		const pack = this.props.package?.name || species?.package;
		if (!pack) return;
		const template = await ide.backend.classTemplate(pack);
		if (species) {
			template.superclass = species.name;
		}
		if (this.props.onClassSelect) this.props.onClassSelect(template);
	};

	newSubclass = async (superclass) => {
		if (!superclass) return;
		try {
			const name = await ide.prompt({
				title: "New " + superclass.name + " subclass",
				required: true,
			});
			if (!name) return;
			const packagename = this.props.package
				? this.props.package.name
				: superclass.package;
			await ide.backend.defineClass(name, superclass.name, packagename);
			const species = await ide.backend.classNamed(name);
			let expanded = this.state.expandedClasses;
			if (!expanded.find((c) => c.name === superclass.name))
				expanded.push(superclass);
			if (!superclass.subclasses) superclass.subclasses = [];
			superclass.subclasses.push(species);
			superclass.subclasses.sort((a, b) => (a.name <= b.name ? -1 : 1));
			this.setState({
				expandedClasses: expanded,
				selectedClass: species,
			});
			if (this.props.onClassDefine) this.props.onClassDefine(species);
		} catch (error) {
			ide.reportError(error);
		}
	};

	renameClass = async (species) => {
		if (!species) return;
		try {
			const newName = await ide.prompt({
				title: "Rename class",
				defaultValue: species.name,
				required: true,
			});
			if (!newName) return;
			await ide.waitFor(() =>
				ide.backend.renameClass(species.name, newName)
			);
			const renamed = await ide.backend.classNamed(newName);
			Object.assign(species, renamed);
			this.setState({ selectedClass: species });
			if (this.props.onClassRename) this.props.onClassRename(species);
		} catch (error) {
			ide.reportError(error);
		}
	};

	removeClass = async (species) => {
		if (!species) return;
		try {
			let message;
			const references = await ide.waitFor(() => {
				ide.backend.classReferences(species.name);
			}, "Searching references...");
			if (references.length > 0) {
				message =
					"Class " +
					species.name +
					" has " +
					references.length +
					" references. Remove it anyway?";
			} else {
				message = "Remove " + species.name + " class?";
			}
			const confirm = await ide.confirm({
				title: "Remove class",
				message: message,
				ok: { text: "Delete", color: "secondary", variant: "outlined" },
				cancel: { text: "Cancel" },
			});
			if (!confirm) return;
			await ide.backend.removeClass(species.name);
			let expanded = this.state.expandedClasses;
			let index = expanded.indexOf(species);
			if (index > -1) expanded.splice(index, 1);
			let superclass = this.findSubclass(species.superclass);
			let selected;
			if (superclass) {
				superclass.subclasses = superclass.subclasses.filter(
					(c) => c.name !== species.name
				);
				selected = superclass;
			}
			let roots = this.state.roots;
			index = roots.indexOf(species);
			if (index > -1) roots.splice(index, 1);
			this.setState({
				roots: roots,
				expandedClasses: expanded,
				selectedClass: selected,
			});
			if (this.props.onClassRemove) this.props.onClassRemove(species);
			if (selected && this.props.onClassSelect)
				this.props.onClassSelect(selected);
		} catch (error) {
			ide.reportError(error);
		}
	};

	browseClass = (species) => {
		if (species) this.context.browseClass(species.name);
	};

	browsePackage = (species) => {
		if (species) this.context.browsePackage(species.package);
	};

	browseClassReferences = (species) => {
		if (species) this.context.browseClassReferences(species.name);
	};

	runTests = (species) => {
		if (species) this.context.runTestClass(species.name);
	};

	migrateClass = (species) => {
		if (species) this.context.migrateClass(species.name);
	};

	exportToTonel = (species) => {
		if (species) ide.exportClassToTonel(species.name);
	};

	canAddClass = (species) => {
		return species; //|| this.props.package;
	};

	generateCodePassingTests = async (species) => {
		const methods = await ide.backend.methods(species.name);
		const tests = methods.filter((m) => this.isTestMethod(m));
		let answer;
		await ide.waitFor(async () => {
			answer = await ide.codeAssistant.writeCodeFromTests(tests);
		});
		if (!answer || !answer.parts)
			return ide.reportError("Could not generate code");
		const code = answer.parts.find((p) => p.code)?.code;
		if (!code) return ide.reportError("Could not generate code");
		//const changeset = code.changeset(ide.backend);
		//await changeset.update();
		//ide.browseChanges(changeset);
		ide.updateAssistantChat();
	};

	isTestMethod = (method) => {
		// This is meant to be removed when StMethod is used
		return method && method.selector && method.selector.startsWith("test");
	};

	isTestClass = (species) => {
		// This is meant to be removed when StClass is used
		return species && species.name.endsWith("Test");
	};

	menuOptions() {
		let options = [];
		let pack = this.props.package;
		if (pack) {
			options.push({
				label: "New",
				action: this.newClass,
			});
		}
		options.push(
			...[
				{
					label: "New subclass",
					action: this.newSubclass,
					enabled: this.canAddClass,
				},
				{
					label: "Rename",
					action: this.renameClass,
					enabled: (c) => c != null,
				},
				{
					label: "Remove",
					action: this.removeClass,
					enabled: (c) => c != null,
				},
				null,
				{
					label: "Browse",
					action: this.browseClass,
					enabled: (c) => c != null,
				},
				{
					label: "Browse package",
					action: this.browsePackage,
					enabled: (c) => c != null,
				},
				{
					label: "Browse references",
					action: this.browseClassReferences,
					enabled: (c) => c != null,
				},
				null,
				{
					label: "Run tests",
					action: this.runTests,
					enabled: (c) => c != null,
				},
				null,
				{
					label: "Export to Tonel",
					action: this.exportToTonel,
					enabled: (c) => c != null,
				},
				{
					label: "Migrate",
					action: this.migrateClass,
					enabled: (c) => c != null,
				},
			]
		);
		if (ide.usesCodeAssistant()) {
			options.push({
				label: "AI - Generate code that passes tests",
				action: this.generateCodePassingTests,
				enabled: this.isTestClass,
			});
		}
		const extended = ide.extensionMenuOptions(
			this.state.extendedOptions,
			this.performExtendedOption
		);
		return options.concat(extended);
	}

	performExtendedOption = async (option, species) => {
		if (!species) return;
		await ide.performExtendedOption(option, species);
		this.extendedOptionPerformed();
	};

	extendedOptionPerformed() {
		const handler = this.props.onExtendedOptionPerform;
		handler
			? handler()
			: this.updateClasses(
					this.state.roots,
					this.props.package,
					this.props.category,
					this.state.selectedClass,
					this.state.expandedClasses
			  );
	}

	classLabel = (species) => {
		if (species.template) return "<new>";
		const label = species.name;
		return species.modified === true ? `(${label})` : label;
	};

	classColor = (species) => {
		const pack = this.props.package;
		if (!pack) return;
		if (pack.classes && pack.classes.includes(species.name)) {
			return;
		}
		const appearance = ide.settings.section("appearance");
		const mode = appearance.section(appearance.get("mode"));
		return mode.get("disabledText");
	};

	classIcon = (species) => {
		return ide.objectIcon(species, "class", species.name);
	};

	render() {
		const { roots, selectedClass, expandedClasses, loading, showSearch } =
			this.state;
		const appearance = ide.settings.section("appearance");
		const mode = appearance.section(appearance.get("mode"));
		const background = mode.get("background");
		const root = roots.length === 1 ? roots[0] : null;
		return (
			<Box display="flex" flexDirection="column" height="100%">
				{showSearch && (
					<Box display="flex" flexDirection="row" width="100%">
						<Box mb={1} flexGrow={1}>
							<SearchList2
								value={
									selectedClass ? selectedClass.name : null
								}
								options={this.classSearch}
								backColor={background}
								onChange={this.goToRoot}
							/>
						</Box>
						{root && (
							<Box>
								<Tooltip
									title={root.superclass || ""}
									placement="top"
								>
									<IconButton
										color="inherit"
										size="small"
										onClick={() =>
											this.goToRoot(root.superclass)
										}
										disabled={!root.superclass}
									>
										<UpIcon />
									</IconButton>
								</Tooltip>
							</Box>
						)}
					</Box>
				)}
				<Box flexGrow={1}>
					<CustomPaper>
						<CustomTree
							loading={loading}
							nodes={roots}
							nodeId="name"
							nodeLabel={this.classLabel}
							nodeColor={this.classColor}
							nodeIcon={this.classIcon}
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
export default ClassTree;
