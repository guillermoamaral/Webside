import React, { Component } from "react";
import CustomList from "../controls/CustomList";
import { ide } from "../IDE";

class CategoryList extends Component {
	constructor(props) {
		super(props);
		this.all = "All selectors";
	}

	categorySelected = (category) => {
		const selected = category === this.all ? null : category;
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
		} catch (error) {}
	};

	addCategory = (category) => {
		if (this.props.onCategoryAdd) {
			this.props.onCategoryAdd(category);
		}
	};

	renameCategory = async (category) => {
		if (!category) {
			return;
		}
		try {
			const newName = await ide.prompt({
				title: "Rename category",
				defaultValue: category,
			});
			await ide.backend.renameCategory(
				this.props.class.name,
				category,
				newName
			);
			if (this.props.onCategoryRename) {
				this.props.onCategoryRename(category, newName);
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
			if (this.props.onCategoryRemove) {
				this.props.onCategoryRemove(category);
			}
		} catch (error) {
			ide.reportError(error);
		}
	};

	menuOptions() {
		const options = [];
		const used = (this.props.usedCategories || []).map((c) => {
			return { label: c, action: () => this.addCategory(c) };
		});
		if (used.length > 0) {
			options.push({
				label: "Used",
				suboptions: used,
			});
		}
		const usual = (this.props.usualCategories || []).map((c) => {
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
		let categories = this.props.categories;
		const { selectedCategory, highlightedCategory } = this.props;
		if (categories) {
			categories = [...categories];
			categories.unshift(this.all);
		}
		return (
			<CustomList
				items={categories}
				itemDivider={(item) => item === this.all}
				labelStyle={(item) => (item === this.all ? "italic" : "normal")}
				labelSize={(item) => (item === this.all ? "small" : "normal")}
				selectedItem={selectedCategory}
				highlightedItem={highlightedCategory}
				onItemSelect={this.categorySelected}
				menuOptions={this.menuOptions()}
			/>
		);
	}
}

export default CategoryList;
