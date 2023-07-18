import React, { Component } from "react";
import OverridenIcon from "@mui/icons-material/ExpandMore";
import OverridingIcon from "@mui/icons-material/ExpandLess";
import OverridingOverridenIcon from "@mui/icons-material/UnfoldMore";
import TestIcon from "@mui/icons-material/PlayArrow";
import CustomList from "../controls/CustomList";
import CustomTable from "../controls/CustomTable";
import { ide } from "../IDE";
import ToolContainerContext from "../ToolContainerContext";

class MethodList extends Component {
	static contextType = ToolContainerContext;

	renameMethod = async (method) => {
		if (!method) {
			return;
		}
		try {
			const newSelector = await ide.prompt({
				title: "Rename selector",
				defaultValue: method.selector,
				required: true,
			});
			await ide.waitFor(() =>
				ide.backend.renameSelector(
					method.methodClass,
					method.selector,
					newSelector
				)
			);
			method.selector = newSelector;
			if (this.props.onMethodRename) {
				this.props.onMethodRename(method);
			}
		} catch (error) {
			ide.reportError(error);
		}
	};

	removeMethod = async (method) => {
		try {
			await ide.backend.removeMethod(method.methodClass, method.selector);
			if (this.props.onMethodRemove) {
				this.props.onMethodRemove(method);
			}
		} catch (error) {
			ide.reportError(error);
		}
	};

	newCategory = async () => {
		try {
			const category = await ide.prompt({
				title: "New category",
			});
			if (category && this.props.onCategoryAdd) {
				this.props.onCategoryAdd(category);
			}
			return category;
		} catch (error) {}
	};

	suggestCategory = async (method) => {
		const assistant = ide.codeAssistant();
		const suggested = await assistant.categorizeMethod(method);
		try {
			const category = await ide.prompt({
				title: "Confirm category",
				defaultValue: suggested,
			});
			if (category && this.props.onCategoryAdd) {
				this.props.onCategoryAdd(category);
			}
			this.classifyMethod(method, category);
		} catch (error) {}
	};

	classifyMethod = async (method, category) => {
		var target = category;
		if (!target) {
			target = await this.newCategory();
		}
		try {
			await ide.backend.classifyMethod(
				method.methodClass,
				method.selector,
				target
			);
			method.category = target;
			if (this.props.onMethodClassify) {
				this.props.onMethodClassify(method);
			}
		} catch (error) {
			ide.reportError(error);
		}
	};

	browseClass = (method) => {
		if (method) {
			this.context.browseClass(method.methodClass);
		}
	};

	browsePackage = (method) => {
		if (method) {
			this.context.browsePackage(method.package);
		}
	};

	browseSenders = (method) => {
		if (method) {
			this.context.browseSenders(method.selector);
		}
	};

	browseLocalSenders = (method) => {
		if (method) {
			this.context.browseLocalSenders(
				method.selector,
				method.methodClass
			);
		}
	};

	browseImplementors = (method) => {
		if (method) {
			this.context.browseImplementors(method.selector);
		}
	};

	browseLocalImplementors = (method) => {
		if (method) {
			this.context.browseLocalImplementors(
				method.selector,
				method.methodClass
			);
		}
	};

	browseClassReferences = (method) => {
		if (method) {
			this.context.browseClassReferences(method.methodClass);
		}
	};

	runTest = (method, silently) => {
		if (method) {
			this.context.runTest(method.methodClass, method.selector, silently);
		}
	};

	migrateMethod = (method) => {
		if (method) {
			this.context.migrateMethod(method);
		}
	};

	categoryOptions(categories) {
		const options = (categories || []).map((c) => {
			return {
				label: c,
				action: (m) => this.classifyMethod(m, c),
			};
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
		const current = this.categoryOptions(this.props.categories);
		const used = this.categoryOptions(this.props.usedCategories);
		const usual = this.categoryOptions(this.props.usualCategories);
		const suboptions = ide.usesCodeAssistant()
			? [
					{
						label: "AI suggested..",
						action: (m) => this.suggestCategory(m),
					},
			  ]
			: [];
		if (current.length > 0) {
			suboptions.push({
				label: "Current",
				suboptions: current,
			});
		}
		if (used.length > 0) {
			suboptions.push({
				label: "Used",
				suboptions: used,
			});
		}
		if (usual.length > 0) {
			suboptions.push({
				label: "Usual",
				suboptions: usual,
			});
		}
		suboptions.push({
			label: "New..",
			action: (m) => this.classifyMethod(m),
		});
		options.push(
			...[
				{
					label: "Classify under...",
					suboptions: suboptions,
				},
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
		if (this.isTest(method)) {
			return (
				<TestIcon
					style={{ fontSize: 16 }}
					onClick={(event) => this.runTest(method, true)}
				/>
			);
		}
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
				// link: (m) => {
				// 	this.browseClass(m);
				// },
				align: "left",
				minWidth: 300,
			},
			{
				field: "selector",
				label: "Selector",
				// link: (m) => {
				// 	this.browseImplementors(m);
				// },
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
				// link: (m) => {
				// 	this.browsePackage(m);
				// },
				align: "left",
				minWidth: 300,
			},
		];
	}

	newMethod = () => {
		const selected = this.props.selectedMethod;
		const method = ide.backend.methodTemplate();
		method.methodClass = selected ? selected.methodClass : null;
		method.category = selected ? selected.category : null;
		this.props.onMethodSelect(method);
	};

	methodLabel = (method) => {
		return this.props.showClass === true
			? method.methodClass + " >> #" + method.selector
			: method.selector;
	};

	methodColor = (method) => {
		if (method && method.needsRecompilation) return "red";
	};

	render() {
		const methods = this.props.methods || [];
		const { useTable, selectedMethod, onMethodSelect } = this.props;
		if (useTable) {
			return (
				<CustomTable
					columns={this.methodColumns()}
					rows={methods}
					onRowSelect={onMethodSelect}
					menuOptions={this.menuOptions()}
					hideRowBorder
					rowsPerPage={50}
					usePagination
					selectedRow={selectedMethod}
					//noHeaders
				/>
			);
		} else {
			return (
				<CustomList
					items={methods}
					itemLabel={this.methodLabel}
					itemStyle={this.props.labelStyle}
					itemIcon={this.methodIcon}
					itemColor={this.methodColor}
					selectedItem={selectedMethod}
					onItemSelect={onMethodSelect}
					menuOptions={this.menuOptions()}
				/>
			);
		}
	}
}

export default MethodList;
