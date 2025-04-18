import React, { Component } from "react";
import CustomList from "../controls/CustomList";
import CustomPaper from "../controls/CustomPaper";
import { ide } from "../IDE";
import ToolContainerContext from "../ToolContainerContext";

class CategoryList extends Component {
	static contextType = ToolContainerContext;

	constructor(props) {
		super(props);
		this.all = "All selectors";
		this.state = {
			class: null,
			categories: [],
			used: [],
			usual: [],
			selectedCategory: null,
			loading: false,
			extendedOptions: [],
		};
	}

	componentDidMount() {
		this.updateCategories();
		this.updateExtendedOptions();
	}

	componentDidUpdate(prevProps) {
		if (this.props.class !== prevProps.class) {
			this.updateCategories(this.state.selectedCategory);
		}
	}

	async refreshEnsuring(category) {
		this.updateCategories(category);
	}

	async updateCategories(selectedCategory) {
		this.setState({ loading: true });
		const categories = await this.fetchCategories();
		const selected = categories.defined.find((c) => c === selectedCategory);
		this.setState({
			loading: false,
			categories: categories.defined,
			used: categories.used,
			usual: categories.usual,
			selectedCategory: selected,
		});
	}

	async fetchCategories() {
		let categories = { defined: [], used: [], usual: [] };
		let species = this.props.class;
		if (!species) return categories;
		if (species.template) return categories;
		try {
			categories.defined = await ide.backend.categories(species.name);
			categories.defined.sort();
			categories.used = await ide.backend.usedCategories(species.name);
			categories.used.sort();
			categories.usual = await ide.backend.usualCategories(
				species.name.endsWith(" class")
			);
			categories.usual.sort();
		} catch (error) {
			ide.reportError(error);
		}
		return categories;
	}

	async updateExtendedOptions() {
		const options = await ide.fetchExtendedOptions("category");
		this.setState({ extendedOptions: options });
	}

	categorySelected = async (category) => {
		const selected = category === this.all ? null : category;
		this.setState({ selectedCategory: selected });
		if (this.props.onCategorySelect) {
			this.props.onCategorySelect(selected);
		}
		this.updateExtendedOptions();
	};

	addNewCategory = async () => {
		try {
			const category = await ide.prompt({
				title: "New category",
			});
			if (category) this.addCategory(category);
		} catch (error) {}
	};

	addCategory = (category) => {
		let categories = this.state.categories;
		categories.push(category);
		categories.sort();
		this.setState({ categories: categories, selectedCategory: category });
		if (this.props.onCategoryAdd) this.props.onCategoryAdd(category);
	};

	renameCategory = async (category) => {
		if (!category) return;
		try {
			const name = await ide.prompt({
				title: "Rename category",
				defaultValue: category,
			});
			if (!name) return;
			await ide.backend.renameCategory(
				this.props.class.name,
				category,
				name
			);
			let categories = this.state.categories;
			categories.splice(categories.indexOf(category), 1, name);
			categories.sort();
			this.setState({
				categories: categories,
				selectedCategory: name,
			});
			if (this.props.onCategoryRename) this.props.onCategoryRename(name);
		} catch (error) {
			ide.reportError(error);
		}
	};

	removeCategory = async (category) => {
		if (!category) return;
		const applied = await ide.performChange((backend) =>
			backend.removeCategory(this.props.class.name, category)
		);
		if (applied) {
			let categories = await this.fetchCategories();
			this.setState({
				categories: categories.defined,
				selectedCategory: null,
			});
		}
		if (this.props.onCategoryRemove) this.props.onCategoryRemove(category);
	};

	browseMethods = async (category) => {
		let species = this.props.class;
		if (!species) return [];
		let methods = await ide.backend.methodsInCategory(
			species.name,
			category
		);
		this.context.openMethodBrowser(methods);
	};

	menuOptions() {
		let suboptions = [];
		if (this.state.used.length > 0) {
			suboptions.push({
				label: "Used",
				suboptions: this.state.used.map((c) => {
					return { label: c, action: () => this.addCategory(c) };
				}),
			});
		}
		const usual = this.state.usual.map((c) => {
			return { label: c, action: () => this.addCategory(c) };
		});
		if (usual.length > 0) {
			suboptions.push({
				label: "Usual",
				suboptions: usual,
			});
		}
		suboptions.push({
			label: "New...",
			action: this.addNewCategory,
		});
		let options = [
			{ label: "Add", suboptions: suboptions },
			{ label: "Rename", action: this.renameCategory },
			{ label: "Remove", action: this.removeCategory },
			null,
			{ label: "Browse methods", action: this.browseMethods },
		];
		const extended = ide.extensionMenuOptions(
			this.state.extendedOptions,
			this.performExtendedOption
		);
		return options.concat(extended);
	}

	performExtendedOption = async (option, category) => {
		if (!category) return;
		await ide.performExtendedOption(option, category);
		this.extendedOptionPerformed();
	};

	extendedOptionPerformed() {
		const handler = this.props.onExtendedOptionPerform;
		handler
			? handler()
			: this.updateCategories(this.state.selectedCategory);
	}

	render() {
		let { categories, selectedCategory, loading } = this.state;
		const { highlightedCategory } = this.props;
		categories = [this.all, ...categories];
		return (
			<CustomPaper>
				<CustomList
					sx={{ height: "100%" }}
					loading={loading}
					items={categories}
					itemDivider={(item) => item === this.all}
					labelStyle={(item) =>
						item === this.all ? "italic" : "normal"
					}
					labelSize={(item) =>
						item === this.all ? "small" : "normal"
					}
					selectedItem={selectedCategory}
					highlightedItem={highlightedCategory}
					onItemSelect={this.categorySelected}
					menuOptions={this.menuOptions()}
				/>
			</CustomPaper>
		);
	}
}

export default CategoryList;
