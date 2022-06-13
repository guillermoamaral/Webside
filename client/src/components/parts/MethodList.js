import React, { Component } from "react";
import FastCustomList from "../controls/FastCustomList";
import OverridenIcon from "@material-ui/icons/ExpandMore";
import OverridingIcon from "@material-ui/icons/ExpandLess";
import OverridingOverridenIcon from "@material-ui/icons/UnfoldMore";
import { IDEContext } from "../IDEContext";
import { withDialog } from "../dialogs/index";
import { optionHandlers } from "codemirror";

class MethodList extends Component {
	static contextType = IDEContext;

	newMethod = () => {
		const method = {
			class: this.props.selectedMethod ? this.props.selectedMethod.class : null,
			category: this.props.selectedMethod
				? this.props.selectedMethod.category
				: null,
			source: 'messagePattern\r\t"comment"\r\t| temporaries |\r\tstatements',
		};
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
			const handler = this.props.onRename;
			if (handler) {
				handler(method);
			}
		} catch (error) {
			this.context.reportError(error);
		}
	};

	removeMethod = async (method) => {
		try {
			await this.context.api.removeMethod(method.class, method.selector);
			const handler = this.props.onRemove;
			if (handler) {
				handler(method);
			}
		} catch (error) {
			this.context.reportError(error);
		}
	};

	classifyMethod = async (method, category) => {
		try {
			await this.context.api.classifyMethod(
				method.class,
				method.selector,
				category
			);
			method.category = category;
			const handler = this.props.onClassify;
			if (handler) {
				handler(method);
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

	menuOptions = () => {
		const options = [
			{ label: "New", action: this.newMethod },
			{ label: "Rename", action: this.renameMethod },
			{ label: "Remove", action: this.removeMethod },
		];
		const categories = this.props.categories || [];
		if (categories.length > 0) {
			options.push({
				label: "Classify under...",
				suboptions: categories.map((c) => {
					return {
						label: c,
						action: (m) => this.classifyMethod(m, c),
					};
				}),
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
				itemIcon={this.methodIcon}
				selectedItem={this.props.selectedMethod}
				onSelect={this.props.onSelect}
				menuOptions={this.menuOptions()}
			/>
		);
	}
}

export default withDialog()(MethodList);
