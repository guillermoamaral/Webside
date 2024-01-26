import React, { Component } from "react";
import CustomList from "../controls/CustomList";
import CustomPaper from "../controls/CustomPaper";
import { ide } from "../IDE";

class UCategoryList extends Component {

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
		};
	}

	componentDidMount() {
		this.updateCategories();
	}

	componentDidUpdate(prevProps) {
		if (this.props.class !== prevProps.class) {
			this.updateCategories();
		}
	}

	async refreshEnsuring(category) {
		this.updateCategories(category);
	}

	async updateCategories(selectedCategory) {
		this.setState({ loading: true })
		let categories = await this.fetchCategories();
		let selected = categories.defined.find(c => c === selectedCategory);
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
			categories.usual = await ide.backend.usualCategories(species.name.endsWith(" class"));
			categories.usual.sort();
		} catch (error) {
			ide.reportError(error);
		}
		return categories;
	}

	categorySelected = (category) => {
		const selected = category === this.all ? null : category;
		this.setState({ selectedCategory: selected });
		if (this.props.onCategorySelect) {
			this.props.onCategorySelect(selected);
		}
	};

	addNewCategory = async () => {
		try {
			const category = await ide.prompt({
				title: "New category",
			});
			if (category) {
				this.addCategory(category);
			}
		} catch (error) { }
	};

	addCategory = (category) => {
		let categories = this.state.categories;
		categories.push(category);
		categories.sort();
		this.setState({ categories: categories, selectedCategory: category })
		if (this.props.onCategoryAdd) {
			this.props.onCategoryAdd(category);
		}
	};

	renameCategory = async (category) => {
		if (!category) {
			return;
		}
		try {
			const renamed = await ide.prompt({
				title: "Rename category",
				defaultValue: category,
			});
			await ide.backend.renameCategory(
				this.props.class.name,
				category,
				renamed
			);
			let categories = this.state.categories;
			categories.splice(categories.indexOf(category), 1, renamed);
			categories.sort();
			this.setState({ categories: categories, selectedCategory: renamed });
			if (this.props.onCategoryRename) {
				this.props.onCategoryRename(category, renamed);
			}
		} catch (error) {
			ide.reportError(error);
		}
	};

	removeCategory = async (category) => {
		if (!category) {
			return;
		}
		try {
			await ide.backend.removeCategory(this.props.class.name, category);
			let categories = await this.fetchCategories();
			this.setState({ categories: categories.defined, selectedCategory: null });
			if (this.props.onCategoryRemove) {
				this.props.onCategoryRemove(category);
			}
		} catch (error) {
			ide.reportError(error);
		}
	};

	menuOptions() {
		const options = [];
		const used = (this.state.used).map((c) => {
			return { label: c, action: () => this.addCategory(c) };
		});
		if (used.length > 0) {
			options.push({
				label: "Used",
				suboptions: used,
			});
		}
		const usual = (this.state.usual).map((c) => {
			return { label: c, action: () => this.addCategory(c) };
		});
		if (usual.length > 0) {
			options.push({
				label: "Usual",
				suboptions: usual,
			});
		}
		options.push({
			label: "New...",
			action: this.addNewCategory,
		});
		return [
			{ label: "Add", suboptions: options },
			{ label: "Rename", action: this.renameCategory },
			{ label: "Remove", action: this.removeCategory },
		];
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
					labelStyle={(item) => (item === this.all ? "italic" : "normal")}
					labelSize={(item) => (item === this.all ? "small" : "normal")}
					selectedItem={selectedCategory}
					highlightedItem={highlightedCategory}
					onItemSelect={this.categorySelected}
					menuOptions={this.menuOptions()}
				/>
			</CustomPaper>
		);
	}
}

export default UCategoryList;
