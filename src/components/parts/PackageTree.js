import React, { Component } from "react";
import CustomTree from "../controls/CustomTree";
import CustomPaper from "../controls/CustomPaper";
import { ide } from "../IDE";
import ToolContainerContext from "../ToolContainerContext";
import { Box, TextField } from "@mui/material";

class PackageTree extends Component {
	static contextType = ToolContainerContext;

	constructor(props) {
		super(props);
		this.state = {
			packages: [],
			selectedNode: null,
			expandedNodes: [],
			loading: false,
			extendedOptions: [],
			packageFilter: this.props.selectedPackage
				? this.props.selectedPackage.name
				: "",
		};
	}

	componentDidMount() {
		this.updatePackages(this.props.selectedPackage);
		this.updateExtendedOptions();
	}

	componentDidUpdate(prevProps) {
		if (prevProps.selectedPackage !== this.props.selectedPackage) {
			this.setState({
				selectedNode: this.props.selectedPackage,
			});
		}
	}

	async updatePackages(selectedPackage) {
		this.setState({ loading: true });
		let packages = await this.fetchPackages();
		let selected = selectedPackage
			? packages.find((p) => p.name === selectedPackage.name)
			: null;
		let expanded = [];
		this.state.expandedNodes.forEach((e) => {
			let found = packages.find((p) => p.name === e.name);
			if (found) expanded.push(found);
		});
		this.setState({
			packages: packages,
			selectedNode: selected,
			expandedNodes: expanded,
			loading: false,
		});
	}

	async fetchPackages() {
		let packages = [];
		try {
			packages = await ide.backend.packageTree();
			packages.forEach((p) => {
				p.nodeType = "package";
				if (p.categories) {
					p.categories.forEach((c) => (c.nodeType = "category"));
				}
			});
			packages = packages.sort((a, b) => (a.name <= b.name ? -1 : 1));
		} catch (error) {
			ide.reportError(error);
		}
		return packages;
	}

	async updateExtendedOptions() {
		const options = await ide.fetchExtendedOptions("package");
		this.setState({ extendedOptions: options });
	}

	async updatePackage(pack) {
		try {
			let retrieved = await ide.backend.packageNamed(pack.name);
			Object.assign(pack, retrieved);
			await ide.backend.packageNamed(pack.name);
			pack.nodeType = "package";
			if (pack.categories) {
				pack.categories.forEach((c) => (c.nodeType = "category"));
			}
		} catch (error) {
			ide.reportError(error);
		}
	}

	packageNamed(name) {
		return this.state.packages.find((p) => p.name === name);
	}

	nodeSelected = async (node) => {
		if (node.nodeType === "package") await this.updatePackage(node);
		this.setState({ selectedNode: node });
		if (node.nodeType === "package") {
			if (this.props.onPackageSelect) this.props.onPackageSelect(node);
		} else {
			if (this.props.onCategorySelect) {
				const pack = this.packageNamed(node.package);
				this.props.onCategorySelect(node, pack);
			}
		}
		this.updateExtendedOptions();
	};

	nodeExpanded = async (node) => {
		if (node.nodeType === "package") await this.updatePackage(node);
		this.setState({
			expandedNodes: [...this.state.expandedNodes, node],
		});
		if (node.nodeType === "package" && this.props.onPackageExpand)
			this.props.onPackageExpand(node);
	};

	nodeCollaspsed = (node) => {
		const expanded = this.state.expandedNodes;
		expanded.splice(expanded.indexOf(node), 1);
		this.setState({ expandedNodes: expanded });
		if (node.nodeType === "package" && this.props.onPackageCollapse)
			this.props.onPackageCollapse(node);
	};

	createPackage = async () => {
		try {
			const name = await ide.prompt({
				title: "New package",
				required: true,
			});
			if (!name) return;
			await ide.backend.createPackage(name);
			let pack = { name: name };
			await this.updatePackage(pack);
			let packages = this.state.packages;
			packages.push(pack);
			packages.sort((a, b) => (a.name <= b.name ? -1 : 1));
			this.setState({ packages: packages, selectedNode: pack });
			if (this.props.onPackageCreate) this.props.onPackageCreate(pack);
		} catch (error) {
			ide.reportError(error);
		}
	};

	renamePackage = async (pack) => {
		if (!pack) return;
		try {
			const newName = await ide.prompt({
				title: "Rename package",
				defaultValue: pack.name,
				required: true,
			});
			if (!newName) return;
			await ide.backend.renamePackage(pack.name, newName);
			pack.name = newName;
			let packages = this.state.packages;
			packages.sort((a, b) => (a.name <= b.name ? -1 : 1));
			this.setState({ packages: packages, selectedNode: pack });
			if (this.props.onPackageRename) this.props.onPackageRename(pack);
		} catch (error) {
			ide.reportError(error);
		}
	};

	removePackage = async (pack) => {
		if (!pack) return;
		const confirm = await ide.confirm({
			title: "Delete " + pack.name + " package?",
			ok: { text: "Delete", color: "secondary", variant: "outlined" },
		});
		if (!confirm) return;
		try {
			await ide.backend.removePackage(pack.name);
			let expanded = this.state.expandedNodes;
			let index = expanded.indexOf(pack);
			if (index > -1) expanded.splice(index, 1);
			let packages = this.state.packages;
			packages = packages.filter((p) => p.name !== pack.name);
			this.setState({
				packages: packages,
				expandedNodes: expanded,
				selectedNode: null,
			});
			if (this.props.onPackageRemove) this.props.onPackageRemove(pack);
		} catch (error) {
			ide.reportError(error);
		}
	};

	runTests = (pack) => {
		if (pack) this.context.runTestPackage(pack.name);
	};

	migratePackage = (pack) => {
		if (pack) this.context.migratePackage(pack.name);
	};

	exportToTonel = (pack) => {
		if (pack) ide.exportPackageToTonel(pack.name);
	};

	addCategory = async () => {
		const pack = this.state.selectedNode;
		try {
			const name = await ide.prompt({
				title: "New category",
				required: true,
			});
			if (!name) return;
			await ide.backend.addClassCategory(pack.name, name);
			await this.updatePackage(pack);
			let expanded = this.state.expandedNodes;
			if (!expanded.find((e) => e.name === pack.name)) {
				expanded.push(pack);
			}
			const category = pack.categories.find((c) => c.name === name);
			this.setState({ selectedNode: category });
			if (this.props.onCategoryAdd) this.props.onCategoryAdd(category);
			if (this.props.onCategorySelect)
				this.props.onCategorySelect(category, pack);
		} catch (error) {
			ide.reportError(error);
		}
	};

	renameCategory = async (category) => {
		if (!category) return;
		try {
			const name = await ide.prompt({
				title: "Rename category",
				defaultValue: category.name,
			});
			if (!name) return;
			await ide.backend.renameClassCategory(
				category.package,
				category.name,
				name
			);
			category.name = name;
			this.setState({ selectedNode: category });
			if (this.props.onCategoryRename)
				this.props.onCategoryRename(category);
		} catch (error) {
			ide.reportError(error);
		}
	};

	removeCategory = async (category) => {
		if (!category) return;
		const applied = await ide.performChange((backend) =>
			backend.removeClassCategory(category.package, category.name)
		);
		if (applied) {
			const pack = this.packageNamed(category.package);
			await this.updatePackage(pack);
			this.setState({ selectedNode: pack });
		}
		if (this.props.onCategoryRemove) this.props.onCategoryRemove(category);
	};

	menuOptions() {
		const selected = this.state.selectedNode;
		if (!selected) return [];
		return selected.nodeType === "package"
			? this.packageOptions()
			: this.categoryOptions();
	}

	packageOptions() {
		let options = [
			{ label: "New", action: this.createPackage },
			{ label: "Rename", action: this.renamePackage },
			{ label: "Remove", action: this.removePackage },
			null,
		];
		if (this.state.selectedNode.categories) {
			options.push({ label: "New category", action: this.addCategory });
			options.push(null);
		}
		options = options.concat([
			{ label: "Run tests", action: this.runTests },
			null,
			{ label: "Export to Tonel", action: this.exportToTonel },
			{ label: "Migrate", action: this.migratePackage },
		]);
		const extended = ide.extensionMenuOptions(
			this.state.extendedOptions,
			this.performExtendedOption
		);
		return options.concat(extended);
	}

	categoryOptions() {
		return [
			{ label: "New", action: this.addCategory },
			{ label: "Rename", action: this.renameCategory },
			{ label: "Remove", action: this.removeCategory },
		];
	}

	performExtendedOption = async (option, pack) => {
		if (!pack) return;
		await ide.performExtendedOption(option, pack);
		this.extendedOptionPerformed();
	};

	extendedOptionPerformed() {
		const handler = this.props.onExtendedOptionPerform;
		handler ? handler() : this.updatePackages(this.state.selectedNode);
	}

	clearPackageFilter() {
		this.setState({ packageFilter: "" });
	}

	filterPackages(text) {
		this.setState({ packageFilter: text });
	}

	packageIcon = (pack) => {
		return ide.objectIcon(pack, pack.name);
	};

	render() {
		const {
			selectedNode,
			expandedNodes,
			loading,
			packageFilter,
			packages,
		} = this.state;
		const filteredPackages = packages.filter((p) => {
			return p.name.toLowerCase().startsWith(packageFilter.toLowerCase());
		});
		return (
			<Box
				display="flex"
				flexDirection="column"
				height="100%"
				p={0}
				m={0}
			>
				<Box>
					<TextField
						sx={{ marginTop: 0, marginBottom: 1 }}
						id="filter"
						variant="outlined"
						size="small"
						type="text"
						placeholder="Filter..."
						margin="dense"
						fullWidth
						autoFocus
						name="filter"
						value={packageFilter}
						onKeyDown={(event) => {
							if (event.key === "Escape") {
								this.clearPackageFilter();
							}
						}}
						onChange={(event) =>
							this.filterPackages(event.target.value)
						}
					/>
				</Box>
				<Box flexGrow={1}>
					<CustomPaper>
						<CustomTree
							loading={loading}
							nodes={filteredPackages}
							nodeId="name"
							nodeLabel={"name"}
							nodeChildren="categories"
							nodeIcon={this.packageIcon}
							selectedNode={selectedNode}
							onNodeSelect={this.nodeSelected}
							expandedNodes={expandedNodes}
							onNodeExpand={this.nodeExpanded}
							onNodeCollapse={this.nodeCollaspsed}
							menuOptions={this.menuOptions()}
						/>
					</CustomPaper>
				</Box>
			</Box>
		);
	}
}
export default PackageTree;
