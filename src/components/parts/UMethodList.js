import React, { Component } from "react";
import OverridenIcon from "@mui/icons-material/ExpandMore";
import OverridingIcon from "@mui/icons-material/ExpandLess";
import OverridingOverridenIcon from "@mui/icons-material/UnfoldMore";
import RunTestIcon from "@mui/icons-material/PlayArrow";
import TestStateIcon from "@mui/icons-material/CircleRounded";
import CustomList from "../controls/CustomList";
import CustomTable from "../controls/CustomTable";
import { ide } from "../IDE";
import ToolContainerContext from "../ToolContainerContext";
import { Typography } from "@mui/material";

class UMethodList extends Component {
	static contextType = ToolContainerContext;

	constructor(props) {
		super(props);
		this.state = {
			methods: [],
			selectedMethod: null,
			class: null,
			category: null,
			access: null,
			variable: null,
			loading: false,
			categories: [],
		};
	}

	async componentDidMount() {
		this.updateMethods();
	}

	async componentDidUpdate(prevProps) {
		if (this.props.methods !== prevProps.methods) {
			return this.setState({ methods: this.props.methods, selectedMethod: null })
		}
		if (this.props.class !== prevProps.class
			|| this.props.category !== prevProps.category
			|| this.props.access !== prevProps.access
			|| this.props.variable !== prevProps.variable) {
			this.updateMethods();
		}
	}

	async updateMethods() {
		this.setState({ loading: true });
		let methods = await this.fetchMethods();
		let categories = await this.fetchCategories();
		let selected = this.state.selectedMethod;
		if (selected) {
			selected = methods.find(m => m.methodClass === selected.methodClass && m.selector === selected.selector);
		}
		let species = this.props.class;
		if (methods.length === 0 && species) {
			const template = ide.backend.methodTemplate();
			template.methodClass = species.name;
			template.category = this.props.category;
			methods.push(template);
		}
		this.setState({
			loading: false,
			methods: methods,
			selectedMethod: selected,
			categories: categories,
		});
	}

	async fetchMethods() {
		let methods = [];
		let species = this.props.class;
		let { category, variable, access } = this.props;
		if (!species) return methods;
		try {
			if (variable && access) {
				methods = await ide.backend.accessors(species.name, variable.name, access, true);
			} else {
				methods = await ide.backend.methods(species.name, true);
			}
		} catch (error) {
			ide.reportError(error);
		}
		if (category) {
			methods = methods.filter(m => m.category === category);
		}
		return methods;
	}

	updateCategories = async () => {
		let categories = await this.fetchCategories();
		this.setState({ categories: categories });
	}

	fetchCategories = async () => {
		let method = this.state.selectedMethod;
		let categories = [];
		let classname = method ? method.methodClass : this.props.class ? this.props.class.name : null;
		if (!classname) return categories;
		try {
			categories = await ide.backend.categories(classname);
			let used = await ide.backend.usedCategories(classname);
			let usual = await ide.backend.usualCategories(classname.endsWith(" class"));
			used.forEach((c) => { if (!categories.includes(c)) categories.push(c) });
			usual.forEach((c) => { if (!categories.includes(c)) categories.push(c) });
			categories.sort();
		}
		catch (error) {
			ide.reportError(error);
		}
		return categories;
	}

	methodSelected = async (method) => {
		await this.updateCategories();
		this.setState({ selectedMethod: method });
		if (this.props.onMethodSelect) {
			this.props.onMethodSelect(method);
		}
	};

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
			this.setState({ selectedMethod: method });
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
			let methods = this.state.methods;
			methods.splice(methods.indexOf(method), 1);
			this.setState({ methods: methods, selectedMethod: null });
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
		} catch (error) { }
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
		} catch (error) { }
	};

	classifyMethod = async (method, category) => {
		var target = category;
		if (!target) {
			target = await this.newCategory();
		}
		if (!target) return;
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

	chooseCategoryForMethod = async (method) => {
		var category;
		try {
			category = await ide.choose({
				title: "Category",
				message: "Select a category",
				items: this.state.categories,
			});
		} catch (error) { }
		if (category) {
			this.classifyMethod(method, category);
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
			var onRun = silently
				? () => {
					this.methodSelected(method);
				}
				: null;
			this.context.runTest(
				method.methodClass,
				method.selector,
				silently,
				onRun
			);
		}
	};

	migrateMethod = (method) => {
		if (method) {
			this.context.migrateMethod(method);
		}
	};

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
		const categories = this.state.categories;
		const suboptions = categories
			.slice(0, Math.min(20, categories.length))
			.map((c) => {
				return {
					label: c,
					action: (m) => this.classifyMethod(m, c),
				};
			});
		if (categories.length > 20) {
			suboptions.push({
				label: "More...",
				action: (m) => this.chooseCategoryForMethod(m),
			});
		}
		if (categories.length > 0) {
			suboptions.push(null);
		}
		suboptions.push({
			label: "New..",
			action: (m) => this.classifyMethod(m),
		});
		if (ide.usesCodeAssistant()) {
			suboptions.push({
				label: "AI suggested..",
				action: (m) => this.suggestCategory(m),
			});
		}
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
		if (this.isTest(method) && method.status) {
			const color =
				method.status === "passed"
					? "green"
					: method.status === "failed"
						? "yellow"
						: method.status === "error"
							? "red"
							: "grey";
			return <TestStateIcon style={{ color: color, fontSize: size }} />;
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

	methodActions = (method) => {
		var actions = [];
		if (this.isTest(method)) {
			actions.push({
				icon: <RunTestIcon color="primary" style={{ fontSize: 16 }} />,
				label: "Run test",
				handler: (m) => this.runTest(m, true),
			});
		}
		actions.push({
			//icon: <ImplementorsIcon style={{ fontSize: 14 }} />,
			icon: <Typography color="primary">i</Typography>,
			label: "Implementors",
			handler: this.browseImplementors,
		});
		actions.push({
			//icon: <SendersIcon style={{ fontSize: 14 }} />,
			icon: <Typography color="primary">s</Typography>,
			label: "Senders",
			handler: this.browseSenders,
		});
		return actions;
	};

	newMethod = () => {
		const selected = this.state.selectedMethod;
		const method = ide.backend.methodTemplate();
		method.methodClass = selected ? selected.methodClass : null;
		method.category = selected ? selected.category : null;
		this.methodSelected(method);
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
		const { methods, selectedMethod, loading } = this.state;
		const { useTable, labelStyle } = this.props;
		if (useTable) {
			return (
				<CustomTable
					columns={this.methodColumns()}
					rows={methods}
					onRowSelect={this.methodSelected}
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
					loading={loading}
					items={methods}
					itemLabel={this.methodLabel}
					itemStyle={labelStyle}
					itemIcon={this.methodIcon}
					itemColor={this.methodColor}
					selectedItem={selectedMethod}
					onItemSelect={this.methodSelected}
					menuOptions={this.menuOptions()}
					itemActions={this.methodActions}
				/>
			);
		}
	}
}

export default UMethodList;
