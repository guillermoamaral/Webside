import React, { Component } from "react";
import OverridenIcon from "@material-ui/icons/ExpandMore";
import OverridingIcon from "@material-ui/icons/ExpandLess";
import OverridingOverridenIcon from "@material-ui/icons/UnfoldMore";
import FastCustomList from "../controls/FastCustomList";
import CustomTable from "../controls/CustomTable";
import { ide } from "../IDE";
import { withDialog } from "../dialogs/index";

class MethodList extends Component {
	renameMethod = async (method) => {
		if (!method) {
			return;
		}
		try {
			const newSelector = await this.props.dialog.prompt({
				title: "Rename selector",
				defaultValue: method.selector,
				required: true,
			});
			await ide.api.renameSelector(
				method.methodClass,
				method.selector,
				newSelector
			);
			method.selector = newSelector;
			if (this.props.onRename) {
				this.props.onRename(method);
			}
		} catch (error) {
			ide.reportError(error);
		}
	};

	removeMethod = async (method) => {
		try {
			await ide.api.removeMethod(method.methodClass, method.selector);
			if (this.props.onRemove) {
				this.props.onRemove(method);
			}
		} catch (error) {
			ide.reportError(error);
		}
	};

	newCategory = async () => {
		try {
			const category = await this.props.dialog.prompt({
				title: "New category",
			});
			if (category && this.props.onCategoryAdd) {
				this.props.onCategoryAdd(category);
			}
			return category;
		} catch (error) {}
	};

	classifyMethod = async (method, category) => {
		var target = category;
		if (!target) {
			target = await this.newCategory();
		}
		try {
			await ide.api.classifyMethod(
				method.methodClass,
				method.selector,
				target
			);
			method.category = target;
			if (this.props.onClassify) {
				this.props.onClassify(method);
			}
		} catch (error) {
			ide.reportError(error);
		}
	};

	browseClass = (method) => {
		if (method) {
			ide.browseClass(method.methodClass);
		}
	};

	browsePackage = (method) => {
		if (method) {
			ide.browsePackage(method.package);
		}
	};

	browseSenders = (method) => {
		if (method) {
			ide.browseSenders(method.selector);
		}
	};

	browseLocalSenders = (method) => {
		if (method) {
			ide.browseLocalSenders(method.selector, method.methodClass);
		}
	};

	browseImplementors = (method) => {
		if (method) {
			ide.browseImplementors(method.selector);
		}
	};

	browseLocalImplementors = (method) => {
		if (method) {
			ide.browseLocalImplementors(method.selector, method.methodClass);
		}
	};

	browseClassReferences = (method) => {
		if (method) {
			ide.browseClassReferences(method.methodClass);
		}
	};

	runTest = (method) => {
		if (method) {
			ide.runTest(method.methodClass, method.selector);
		}
	};

	migrateMethod = (method) => {
		if (method) {
			ide.migrateMethod(method);
		}
	};

	categoryOptions() {
		const categories = this.props.categories || [];
		const options = categories.map((c) => {
			return {
				label: c,
				action: (m) => this.classifyMethod(m, c),
			};
		});
		options.push({
			label: "New..",
			action: (m) => this.classifyMethod(m),
		});
		return options;
	}

	isTest(method) {
		return method && method.selector.startsWith("test");
	}

	menuOptions = () => {
		const options = [];
		if (this.props.showNewOption) {
			options.push({ label: "New", action: this.newMethod });
		}
		options.push(
			...[
				{ label: "Rename", action: this.renameMethod },
				{ label: "Remove", action: this.removeMethod },
			]
		);
		const categories = this.categoryOptions();
		if (categories.length > 0) {
			options.push({
				label: "Classify under...",
				suboptions: categories,
			});
		}
		options.push(
			...[
				null,
				{ label: "Browse class", action: this.browseClass },
				{ label: "Browse senders", action: this.browseSenders },
				{
					label: "Browse local senders",
					action: this.browseLocalSenders,
				},
				{
					label: "Browse implementors",
					action: this.browseImplementors,
				},
				{
					label: "Browse local implementors",
					action: this.browseLocalImplementors,
				},
				{
					label: "Browse class references",
					action: this.browseClassReferences,
				},
				{
					label: "Browse package",
					action: this.browsePackage,
				},
				null,
				{
					label: "Run test",
					action: this.runTest,
					enabled: this.isTest,
				},
				null,
				{ label: "Migrate", action: this.migrateMethod },
			]
		);
		return options;
	};

	methodIcon = (method) => {
		const size = 12;
		if (method.overriding && method.overriden) {
			return (
				<OverridingOverridenIcon
					color="primary"
					style={{ fontSize: size }}
					onClick={(event) => this.browseLocalImplementors(method)}
				/>
			);
		}
		if (method.overriding) {
			return (
				<OverridingIcon
					color="primary"
					style={{ fontSize: size }}
					onClick={(event) => this.browseLocalImplementors(method)}
				/>
			);
		}
		if (method.overriden) {
			return (
				<OverridenIcon
					color="primary"
					style={{ fontSize: size }}
					onClick={(event) => this.browseLocalImplementors(method)}
				/>
			);
		}
		return null;
	};

	methodColumns() {
		return [
			{
				field: "methodClass",
				label: "Class",
				link: (m) => {
					this.browseClass(m);
				},
				align: "left",
				minWidth: 300,
			},
			{
				field: "selector",
				label: "Selector",
				link: (m) => {
					this.browseImplementors(m);
				},
				align: "left",
				minWidth: 300,
			},
			{
				field: "category",
				label: "Category",
				align: "left",
				minWidth: 300,
			},
			{
				field: "package",
				label: "Package",
				link: (m) => {
					this.browsePackage(m);
				},
				align: "left",
				minWidth: 300,
			},
		];
	}

	newMethod = () => {
		const selected = this.props.selected;
		const method = ide.api.methodTemplate();
		method.methodClass = selected ? selected.methodClass : null;
		method.category = selected ? selected.category : null;
		this.props.onSelect(method);
	};

	methodLabel = (method) => {
		return this.props.showClass === true
			? method.methodClass + " >> #" + method.selector
			: method.selector;
	};

	render() {
		const methods = this.props.methods || [];
		const useTable = this.props.useTable;
		if (useTable) {
			return (
				<CustomTable
					styles={this.props.styles}
					columns={this.methodColumns()}
					rows={methods}
					onSelect={this.props.onSelect}
					menuOptions={this.menuOptions()}
					hideRowBorder
					rowsPerPage={50}
					usePagination
					//noHeaders
				/>
			);
		} else {
			return (
				<FastCustomList
					items={methods}
					itemLabel={this.methodLabel}
					itemStyle={this.props.labelStyle}
					itemIcon={this.methodIcon}
					selectedItem={this.props.selected}
					onSelect={this.props.onSelect}
					menuOptions={this.menuOptions()}
				/>
			);
		}
	}
}

export default withDialog()(MethodList);
