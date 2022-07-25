import React, { Component } from "react";
import FastCustomList from "../controls/FastCustomList";
import OverridenIcon from "@material-ui/icons/ExpandMore";
import OverridingIcon from "@material-ui/icons/ExpandLess";
import OverridingOverridenIcon from "@material-ui/icons/UnfoldMore";
import { IDEContext } from "../IDEContext";
import { withDialog } from "../dialogs/index";

class MethodList extends Component {
	static contextType = IDEContext;

	newMethod = () => {
		const selected = this.props.selected;
		const method = this.context.api.methodTemplate();
		method.class = selected ? selected.class : null;
		method.category = selected ? selected.category : null;
		this.props.onSelect(method);
	};

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
			await this.context.api.renameSelector(
				method.class,
				method.selector,
				newSelector
			);
			method.selector = newSelector;
			if (this.props.onRename) {
				this.props.onRename(method);
			}
		} catch (error) {
			this.context.reportError(error);
		}
	};

	removeMethod = async (method) => {
		try {
			await this.context.api.removeMethod(method.class, method.selector);
			if (this.props.onRemove) {
				this.props.onRemove(method);
			}
		} catch (error) {
			this.context.reportError(error);
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
			await this.context.api.classifyMethod(
				method.class,
				method.selector,
				target
			);
			method.category = target;
			if (this.props.onClassify) {
				this.props.onClassify(method);
			}
		} catch (error) {
			this.context.reportError(error);
		}
	};

	browseClass = (method) => {
		if (method) {
			this.context.browseClass(method.class);
		}
	};

	browseSenders = (method) => {
		if (method) {
			this.context.browseSenders(method.selector);
		}
	};

	browseLocalSenders = (method) => {
		if (method) {
			this.context.browseLocalSenders(method.selector, method.class);
		}
	};

	browseImplementors = (method) => {
		if (method) {
			this.context.browseImplementors(method.selector);
		}
	};

	browseLocalImplementors = (method) => {
		if (method) {
			this.context.browseLocalImplementors(method.selector, method.class);
		}
	};

	browseClassReferences = (method) => {
		if (method) {
			this.context.browseClassReferences(method.class);
		}
	};

	runTest = (method) => {
		if (method) {
			this.context.runTest(method.class, method.selector);
		}
	};

	migrateMethod = (method) => {
		if (method) {
			this.context.migrateMethod(method);
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

	menuOptions = () => {
		const options = [
			{ label: "New", action: this.newMethod },
			{ label: "Rename", action: this.renameMethod },
			{ label: "Remove", action: this.removeMethod },
		];
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
				{ label: "Senders", action: this.browseSenders },
				{ label: "Local senders", action: this.browseLocalSenders },
				{ label: "Implementors", action: this.browseImplementors },
				{ label: "Local implementors", action: this.browseLocalImplementors },
				{ label: "Class references", action: this.browseClassReferences },
				null,
				{ label: "Test", action: this.runTest },
				null,
				{ label: "Migrate", action: this.migrateMethod },
			]
		);
		return options;
	};

	methodLabel = (method) => {
		return this.props.showClass === true
			? method.class + ">>#" + method.selector
			: method.selector;
	};

	methodIcon = (method) => {
		const size = 12;
		if (method.overriding && method.overriden) {
			return (
				<OverridingOverridenIcon color="primary" style={{ fontSize: size }} />
			);
		}
		if (method.overriding) {
			return <OverridingIcon color="primary" style={{ fontSize: size }} />;
		}
		if (method.overriden) {
			return <OverridenIcon color="primary" style={{ fontSize: size }} />;
		}
		return null;
	};

	render() {
		const methods = !this.props.methods ? [] : this.props.methods;
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

export default withDialog()(MethodList);
