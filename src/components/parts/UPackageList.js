import React, { Component } from "react";
import CustomList from "../controls/CustomList";
import CustomPaper from "../controls/CustomPaper";
import { ide } from "../IDE";
import ToolContainerContext from "../ToolContainerContext";

class UPackageList extends Component {
	static contextType = ToolContainerContext;

	constructor(props) {
		super(props);
		this.state = {
			packages: [],
			selectedPackage: null,
			preselectedPackage: null,
		};
	}

	static getDerivedStateFromProps(props, state) {
		if (props.preselectedPackage !== state.preselectedPackage) {
			return {
				preselectedPackage: props.preselectedPackage,
			};
		}
		return null;
	}

	componentDidMount() {
		this.updatePackages();
	}

	async updatePackages() {
		let packages = await this.fetchPackages();
		let selected = this.props.preselectedPackage ?
			packages.find(p => p.name === this.props.preselectedPackage.name)
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
			packages = names.sort().map(name => { return { name: name } });
		} catch (error) {
			ide.reportError(error);
		}
		return packages;
	}

	packageSelected = (pack) => {
		this.setState({ selectedPackage: pack })
		if (this.props.onPackageSelect) {
			this.props.onPackageSelect(pack);
		}
	};

	createPackage = async () => {
		try {
			const name = await ide.prompt({
				title: "New package",
				required: true,
			});
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
			packages = packages.filter(p => p.name !== pack.name);
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
		return [
			{ label: "New", action: this.createPackage },
			{ label: "Rename", action: this.renamePackage },
			{ label: "Remove", action: this.removePackage },
			null,
			{ label: "Run tests", action: this.runTests },
			null,
			{ label: "Migrate", action: this.migratePackage },
		];
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
export default UPackageList;
