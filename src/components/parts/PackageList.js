import React, { Component } from "react";
import CustomList from "../controls/CustomList";
import CustomPaper from "../controls/CustomPaper";
import { ide } from "../IDE";
import ToolContainerContext from "../ToolContainerContext";

class PackageList extends Component {
	static contextType = ToolContainerContext;

	constructor(props) {
		super(props);
		this.state = {
			packages: [],
			selectedPackage: null,
			extendedOptions: [],
		};
	}

	componentDidMount() {
		this.updatePackages(this.props.selectedPackage);
		this.updateExtendedOptions();
	}

	componentDidUpdate(prevProps) {
		if (prevProps.selectedPackage !== this.props.selectedPackage) {
			this.setState({
				selectedPackage: this.props.selectedPackage,
			});
		}
	}

	async updatePackages(selectedPackage) {
		let packages = await this.fetchPackages();
		let selected = selectedPackage
			? packages.find((p) => p.name === selectedPackage.name)
			: null;
		this.setState({
			packages: packages,
			selectedPackage: selected,
		});
	}

	async fetchPackages() {
		let packages = [];
		try {
			let names = await ide.backend.packageNames();
			packages = names.sort().map((name) => {
				return { name: name };
			});
		} catch (error) {
			ide.reportError(error);
		}
		return packages;
	}

	async updateExtendedOptions() {
		const options = await ide.fetchExtendedOptions("package");
		this.setState({ extendedOptions: options });
	}

	packageSelected = (pack) => {
		this.setState({ selectedPackage: pack });
		if (this.props.onPackageSelect) {
			this.props.onPackageSelect(pack);
		}
		this.updateExtendedOptions();
	};

	createPackage = async () => {
		try {
			const name = await ide.prompt({
				title: "New package",
				required: true,
			});
			if (!name) return;
			await ide.backend.createPackage(name);
			let pack = await ide.backend.packageNamed(name);
			let packages = this.state.packages;
			packages.push(pack);
			packages.sort((a, b) => (a.name <= b.name ? -1 : 1));
			this.setState({ packages: packages, selectedPackage: pack });
			if (this.props.onPackageCreate) {
				this.props.onPackageCreate(pack);
			}
		} catch (error) {
			ide.reportError(error);
		}
	};

	renamePackage = async (pack) => {
		if (!pack) {
			return;
		}
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
			this.setState({ packages: packages, selectedPackage: pack });
			if (this.props.onPackageRename) {
				this.props.onPackageRename(pack);
			}
		} catch (error) {
			ide.reportError(error);
		}
	};

	removePackage = async (pack) => {
		if (!pack) {
			return;
		}
		const confirm = await ide.confirm({
			title: "Delete " + pack.name + " package?",
			ok: { text: "Delete", color: "secondary", variant: "outlined" },
		});
		if (!confirm) {
			return;
		}
		try {
			await ide.backend.removePackage(pack.name);
			let packages = this.state.packages;
			packages = packages.filter((p) => p.name !== pack.name);
			this.setState({ packages: packages });
			if (this.props.onPackageRemove) {
				this.props.onPackageRemove(pack);
			}
		} catch (error) {
			ide.reportError(error);
		}
	};

	runTests = (pack) => {
		if (pack) {
			this.context.runTestPackage(pack.name);
		}
	};

	migratePackage = (pack) => {
		if (pack) {
			this.context.migratePackage(pack.name);
		}
	};

	menuOptions() {
		let options = [
			{ label: "New", action: this.createPackage },
			{ label: "Rename", action: this.renamePackage },
			{ label: "Remove", action: this.removePackage },
			null,
			{ label: "Run tests", action: this.runTests },
			null,
			{ label: "Migrate", action: this.migratePackage },
		];
		const extended = ide.extensionMenuOptions(
			this.state.extendedOptions,
			this.performExtendedOption
		);
		return options.concat(extended);
	}

	performExtendedOption = async (option, pack) => {
		if (!pack) return;
		await ide.performExtendedOption(option, pack);
		this.extendedOptionPerformed();
	};

	extendedOptionPerformed() {
		const handler = this.props.onExtendedOptionPerform;
		handler ? handler() : this.updatePackages(this.state.selectedPackage);
	}

	render() {
		let { packages, selectedPackage } = this.state;
		return (
			<CustomPaper>
				<CustomList
					items={packages}
					itemLabel="name"
					selectedItem={selectedPackage}
					onItemSelect={this.packageSelected}
					menuOptions={this.menuOptions()}
				/>
			</CustomPaper>
		);
	}
}
export default PackageList;
