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

	addCategory = async () => {
		try {
			const category = await ide.prompt({
				title: "New category",
			});
			if (category && this.props.onCategoryAdd) {
				this.props.onCategoryAdd(category);
			}
		} catch (error) {}
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
			await ide.api.renameCategory(
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
			await ide.api.removeCategory(this.props.class.name, category);
			if (this.props.onCategoryRemove) {
				this.props.onCategoryRemove(category);
			}
		} catch (error) {
			ide.reportError(error);
		}
	};

	menuOptions() {
		return [
			{ label: "Add", action: this.addCategory },
			{ label: "Rename", action: this.renameCategory },
			{ label: "Remove", action: this.removeCategory },
		];
	}

	render() {
		let categories = this.props.categories;
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
				selectedItem={this.props.selectedCategory}
				onItemSelect={this.categorySelected}
				menuOptions={this.menuOptions()}
			/>
		);
	}
}

export default CategoryList;
