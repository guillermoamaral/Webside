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
import {
	Typography,
	Box,
	Select,
	OutlinedInput,
	MenuItem,
} from "@mui/material";
import CustomPaper from "../controls/CustomPaper";
import StAST from "../../model/StAST";

class MethodList extends Component {
	static contextType = ToolContainerContext;

	constructor(props) {
		super(props);
		this.state = {
			methods: [],
			selectedMethod: null,
			loading: false,
			categories: [],
			extendedOptions: [],
			scopes: [],
			selectedScope: null,
		};
	}

	async componentDidMount() {
		this.updateMethods();
		this.updateExtendedOptions();
	}

	async componentDidUpdate(prevProps) {
		const methods = this.props.methods;
		let selected = this.props.selectedMethod || this.state.selectedMethod;
		if (
			methods &&
			(methods !== prevProps.methods || methods !== this.state.methods)
		) {
			if (selected) {
				selected = methods.find(
					(m) =>
						m.selector === selected.selector &&
						m.methodClass === selected.methodClass
				);
			}
			return this.setState({
				methods: methods,
				selectedMethod: selected,
			});
		}
		if (
			this.props.class !== prevProps.class ||
			this.props.category !== prevProps.category ||
			this.props.access !== prevProps.access ||
			this.props.variable !== prevProps.variable
		) {
			const scopes = await this.createScopes();
			this.setState(
				{
					scopes: scopes,
					selectedScope: scopes.length > 0 ? scopes[0] : null,
				},
				() => this.updateMethods(selected)
			);
		}
		if (prevProps.selectedMethod !== this.props.selectedMethod) {
			this.setState({
				selectedMethod: this.props.selectedMethod,
			});
		}
	}

	async refreshEnsuring(method) {
		this.updateMethods(method);
	}

	async createScopes() {
		const species = this.props.class;
		let scopes = [];
		if (species) {
			const pack = this.props.package;
			if (pack && species.package !== pack.name) {
				scopes = [{ name: pack.name, type: "package" }, ...scopes];
			}
			scopes.push({ name: species.name, type: "class" });
			let superclasses = await this.fetchSuperclasses();
			superclasses.forEach((c) => {
				scopes.push({ name: c.name, type: "class" });
			});
		}
		return scopes;
	}

	async updateMethods(selectedMethod) {
		this.setState({ loading: true });
		const { scopes, selectedScope } = this.state;
		let scope = scopes.includes(selectedScope)
			? selectedScope
			: scopes.length > 0
			? scopes[0]
			: null;
		let start = scopes.findIndex((s) => s.type === "class");
		let end =
			scope && scope.type === "class" ? scopes.indexOf(scope) : start;
		let classes = start >= 0 ? scopes.slice(start, end + 1) : [];
		let methods = await this.fetchMethods(classes);
		if (scope && scope.type === "package") {
			methods = methods.filter((m) => m.package === scope.name);
		}
		let selected;
		if (selectedMethod) {
			selected = methods.find(
				(m) => m.selector === selectedMethod.selector //&& m.methodClass === selectedMethod.methodClass
			);
		}
		let categories = await this.fetchCategories(selected);
		// let species = this.props.class;
		// if (methods.length === 0 && species && !species.template) {
		// 	const template = await ide.backend.methodTemplate();
		// 	template.methodClass = species.name;
		// 	template.category = this.props.category;
		// 	methods.push(template);
		// }
		this.setState({
			loading: false,
			methods: methods,
			selectedMethod: selected,
			categories: categories,
			selectedScope: scope,
		});
	}

	async fetchMethods(classes) {
		let { category, variable, access } = this.props;
		let grouped = {};
		const settings = ide.settings.section("advance");
		const basic =
			settings && settings.get("useBasicOptionInMethodRequests");
		await Promise.all(
			classes.map(async (c) => {
				let fetched;
				try {
					if (variable && access) {
						fetched = await ide.backend.accessors(
							c.name,
							variable.name,
							access
						);
					} else if (category) {
						fetched = await ide.backend.methodsInCategory(
							c.name,
							category
						);
					} else {
						fetched = await ide.backend.methods(
							c.name,
							false,
							basic
						);
					}
				} catch (error) {
					ide.reportError(error);
				}
				grouped[c.name] = fetched || [];
			})
		);
		let selectors = {};
		let methods = [];
		classes.forEach((c) => {
			grouped[c.name].forEach((m) => {
				if (!selectors[m.selector]) {
					methods.push(m);
					selectors[m.selector] = true;
				}
			});
		});
		if (category) methods = methods.filter((m) => m.category === category);
		return methods.sort((a, b) => (a.selector <= b.selector ? -1 : 1));
	}

	fetchCategories = async (method) => {
		let categories = [];
		let classname = method
			? method.methodClass
			: this.props.class && !this.props.class.template
			? this.props.class.name
			: null;
		if (!classname) return categories;
		try {
			categories = await ide.backend.categories(classname);
			let used = await ide.backend.usedCategories(classname);
			let usual = await ide.backend.usualCategories(
				classname.endsWith(" class")
			);
			used.forEach((c) => {
				if (!categories.includes(c)) categories.push(c);
			});
			usual.forEach((c) => {
				if (!categories.includes(c)) categories.push(c);
			});
			categories.sort();
		} catch (error) {
			ide.reportError(error);
		}
		return categories;
	};

	async fetchSuperclasses() {
		let superclasses = [];
		let species = this.props.class;
		if (!species || species.template) return superclasses;
		try {
			superclasses = await ide.backend.superclasses(species.name);
		} catch (error) {
			ide.reportError(error);
		}
		return superclasses;
	}

	async updateExtendedOptions() {
		const options = await ide.fetchExtendedOptions("method");
		this.setState({ extendedOptions: options });
	}

	scopeSelected(scope) {
		this.setState({ selectedScope: scope }, () =>
			this.updateMethods(this.state.selectedMethod)
		);
	}

	methodSelected = async (method) => {
		let categories = await this.fetchCategories(method);
		this.setState({
			selectedMethod: method,
			categories: categories,
		});
		if (this.props.onMethodSelect) {
			this.props.onMethodSelect(method);
		}
		this.updateExtendedOptions();
	};

	renameMethod = async (method) => {
		if (!method) return;
		try {
			const newSelector = await ide.prompt({
				title: "Rename selector",
				defaultValue: method.selector,
				required: true,
			});
			if (!newSelector) return;
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
			const count = await ide.waitFor(() =>
				ide.backend.sendersCount(method.selector)
			);
			if (count > 0) {
				const confirm = await ide.confirm({
					title: "Remove method",
					message:
						"Method #" +
						method.selector +
						" has " +
						count +
						" senders. Remove anyway?",
					ok: {
						text: "Delete",
						color: "secondary",
						variant: "outlined",
					},
					cancel: {
						text: "Cancel",
					},
				});
				if (!confirm) return;
			}
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
		} catch (error) {}
	};

	suggestCategory = async (method) => {
		const suggested = await ide.codeAssistant.suggestCategoryForMethod(
			method
		);
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
			target = await ide.prompt({
				title: "New category",
			});
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
		const category = await ide.choose({
			title: "Category",
			message: "Select a category",
			items: this.state.categories,
			filter: true,
		});
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

	browseSenders = (selector) => {
		if (selector) this.context.browseSenders(selector);
	};

	browseLocalSenders = (selector, classname) => {
		if (selector && classname)
			this.context.browseLocalSenders(selector, classname);
	};

	browseImplementors = (selector) => {
		if (selector) this.context.browseImplementors(selector);
	};

	browseLocalImplementors = (selector, classname) => {
		console.log(selector, classname);
		if (selector && classname)
			this.context.browseLocalImplementors(selector, classname);
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

	browseHistory = (method) => {
		if (method) {
			this.context.browseMethodHistory(method);
		}
	};

	migrateMethod = (method) => {
		if (method) {
			this.context.migrateMethod(method);
		}
	};

	canRenameMethod = (method) => {
		return method && !this.isMethodTemplate(method);
	};

	canRemoveMethod = (method) => {
		return method && !this.isMethodTemplate(method);
	};

	canClassifyMethod = (method) => {
		return method && !this.isMethodTemplate(method);
	};

	canMigrateMethod = (method) => {
		return method && !this.isMethodTemplate(method);
	};

	canBrowseHistory = (method) => {
		return method && !this.isMethodTemplate(method);
	};

	isMethodTemplate = (method) => {
		return method && method.template;
	};

	isTestMethod = (method) => {
		return method && method.selector && method.selector.startsWith("test");
	};

	canAddMethod = (method) => {
		return method || this.props.class;
	};

	allSelectorsIn(method) {
		if (!method) return [];
		const selectors = [method.selector];
		if (method.ast) {
			const ast = new StAST();
			ast.fromJson(method.ast);
			const sent = ast.selectors().filter((s) => s !== method.selector);
			selectors.push(...sent);
		}
		return selectors;
	}

	menuOptions = (method) => {
		const options = [];
		if (this.props.showNewOption) {
			options.push({
				label: "New",
				action: this.newMethod,
				enabled: this.canAddMethod,
			});
		}
		options.push(
			...[
				{
					label: "Rename",
					action: this.renameMethod,
					enabled: this.canRenameMethod,
				},
				{
					label: "Remove",
					action: this.removeMethod,
					enabled: this.canRemoveMethod,
				},
			]
		);
		const categories = this.state.categories;
		const suboptions = categories
			.slice(0, Math.min(20, categories.length))
			.map((c) => {
				return {
					label: c,
					action: (m) => this.classifyMethod(m, c),
					enabled: this.canClassifyMethod,
				};
			});
		if (categories.length > 20) {
			suboptions.push({
				label: "More...",
				action: (m) => this.chooseCategoryForMethod(m),
				enabled: this.canClassifyMethod,
			});
		}
		if (categories.length > 0) {
			suboptions.push(null);
		}
		suboptions.push({
			label: "New..",
			action: (m) => this.classifyMethod(m),
			enabled: this.canClassifyMethod,
		});
		if (ide.usesCodeAssistant()) {
			suboptions.push({
				label: "AI - Suggest category",
				action: (m) => this.suggestCategory(m),
				enabled: this.canClassifyMethod,
			});
		}
		options.push(
			...[
				{
					label: "Classify under...",
					suboptions: suboptions,
				},
				null,
				{
					label: "Browse class",
					action: this.browseClass,
					enabled: (m) => m != null,
				},
			]
		);
		const selectors = this.allSelectorsIn(method);
		if (selectors.length > 1) {
			options.push(
				...[
					{
						label: "Browse senders...",
						suboptions: selectors.map((s, i) => {
							return {
								label: s,
								action: (m) => this.browseSenders(s),
								enabled: (m) => m != null,
								weight: i === 0 ? "bold" : "normal",
							};
						}),
					},
					{
						label: "Browse local senders...",
						suboptions: selectors.map((s, i) => {
							return {
								label: s,
								action: (m) =>
									this.browseLocalSenders(s, m.methodClass),
								enabled: (m) => m != null,
								weight: i === 0 ? "bold" : "normal",
							};
						}),
					},
					{
						label: "Browse implementors...",
						suboptions: selectors.map((s, i) => {
							return {
								label: s,
								action: (m) => this.browseImplementors(s),
								enabled: (m) => m != null,
								weight: i === 0 ? "bold" : "normal",
							};
						}),
					},
					{
						label: "Browse local implementors...",
						suboptions: selectors.map((s, i) => {
							return {
								label: s,
								action: (m) =>
									this.browseLocalImplementors(
										s,
										m.methodClass
									),
								enabled: (m) => m != null,
								weight: i === 0 ? "bold" : "normal",
							};
						}),
					},
				]
			);
		} else {
			options.push(
				...[
					{
						label: "Browse senders",
						action: (m) => this.browseSenders(m.selector),
						enabled: (m) => m != null,
					},
					{
						label: "Browse local senders",
						action: (m) =>
							this.browseLocalSenders(m.selector, m.methodClass),
						enabled: (m) => m != null,
					},
					{
						label: "Browse implementors",
						action: (m) => this.browseImplementors(m.selector),
						enabled: (m) => m != null,
					},
					{
						label: "Browse local implementors",
						action: (m) =>
							this.browseLocalImplementors(
								m.selector,
								m.methodClass
							),
						enabled: (m) => m != null,
					},
				]
			);
		}
		options.push(
			...[
				{
					label: "Browse class references",
					action: this.browseClassReferences,
					enabled: (m) => m != null,
				},
				{
					label: "Browse package",
					action: this.browsePackage,
					enabled: (m) => m != null,
				},
				null,
				{
					label: "Run test",
					action: this.runTest,
					enabled: this.isTestMethod,
				},
				null,
				{
					label: "History",
					action: this.browseHistory,
					enabled: this.canBrowseHistory,
				},
				null,
				{
					label: "Migrate",
					action: this.migrateMethod,
					enabled: this.canMigrateMethod,
				},
			]
		);
		const extended = ide.extensionMenuOptions(
			this.state.extendedOptions,
			this.performExtendedOption
		);
		return options.concat(extended);
	};

	performExtendedOption = async (option, method) => {
		await ide.performExtendedOption(option, method);
		this.extendedOptionPerformed();
	};

	extendedOptionPerformed() {
		const handler = this.props.onExtendedOptionPerform;
		handler ? handler() : this.updateMethods(this.state.selectedMethod);
	}

	methodIcon = (method) => {
		const size = 12;
		if (this.isTestMethod(method) && method.status) {
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
					onClick={(event) =>
						this.browseLocalImplementors(
							method.selector,
							method.methodClass
						)
					}
				/>
			);
		}
		if (method.overriding) {
			return (
				<OverridingIcon
					color="primary"
					style={{ fontSize: size }}
					onClick={(event) =>
						this.browseLocalImplementors(
							method.selector,
							method.methodClass
						)
					}
				/>
			);
		}
		if (method.overriden) {
			return (
				<OverridenIcon
					color="primary"
					style={{ fontSize: size }}
					onClick={(event) =>
						this.browseLocalImplementors(
							method.selector,
							method.methodClass
						)
					}
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
		if (this.isTestMethod(method)) {
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
			handler: (m) => this.browseImplementors(m.selector),
		});
		actions.push({
			//icon: <SendersIcon style={{ fontSize: 14 }} />,
			icon: <Typography color="primary">s</Typography>,
			label: "Senders",
			handler: (m) => this.browseSenders(m.selector),
		});
		return actions;
	};

	newMethod = async () => {
		const selected = this.state.selectedMethod;
		let template;
		try {
			template = await ide.backend.methodTemplate();
		} catch (ignored) {
			template = {
				selector: "newMethod",
				source: "newMethod",
				template: true,
			};
		}
		template.methodClass = selected
			? selected.methodClass
			: this.props.class
			? this.props.class.name
			: null;
		template.category = selected ? selected.category : null;
		this.methodSelected(template);
	};

	methodLabel = (method) => {
		const selector = method.template ? "<new>" : method.selector;
		return this.props.showClass === true
			? method.methodClass + " >> #" + selector
			: selector;
	};

	methodColor = (method) => {
		if (!method) return;
		if (method.needsRecompilation) return "red";
		const appearance = ide.settings.section("appearance");
		const mode = appearance.section(appearance.get("mode"));
		const disabled = mode.get("disabledText");
		const species = this.props.class;
		if (species && method.methodClass !== species.name) return disabled;
		const pack = this.props.package;
		if (pack && method.package !== pack.name) return disabled;
		return;
		// Not sure whether to make them look differently when we are not in the context of a package
		// const species = this.props.class;
		// if (species && method.package !== species.package) return disabled;
	};

	evaluationContext() {
		const method = this.state.selectedMethod;
		return method
			? {
					class: method.methodClass,
			  }
			: {};
	}

	render() {
		const { methods, selectedMethod, loading, scopes, selectedScope } =
			this.state;
		const { useTable, labelStyle } = this.props;
		let selectedScopeIndex = scopes.indexOf(selectedScope);
		if (selectedScopeIndex < 0) selectedScopeIndex = "";
		if (useTable) {
			return (
				<CustomTable
					columns={this.methodColumns()}
					rows={methods}
					onRowSelect={this.methodSelected}
					menuOptions={this.menuOptions}
					hideRowBorder
					rowsPerPage={50}
					usePagination
					selectedRow={selectedMethod}
					rowColor={this.methodColor}
					//noHeaders
					onRowDoubleClick={this.browseClass}
				/>
			);
		} else {
			return (
				<Box
					display="flex"
					flexDirection="column"
					style={{ height: "100%" }}
				>
					<Select
						size="small"
						value={selectedScopeIndex}
						input={<OutlinedInput margin="dense" fullWidth />}
						onChange={(event) => {
							this.scopeSelected(scopes[event.target.value]);
						}}
					>
						{scopes.map((scope, index) => (
							<MenuItem value={index} key={index}>
								{scope.name +
									(scope.type === "package"
										? " (package)"
										: "")}
							</MenuItem>
						))}
					</Select>
					<Box mt={1} flexGrow={1}>
						<CustomPaper>
							<CustomList
								loading={loading}
								items={methods}
								itemLabel={this.methodLabel}
								itemStyle={labelStyle}
								itemIcon={this.methodIcon}
								itemColor={this.methodColor}
								selectedItem={selectedMethod}
								onItemSelect={this.methodSelected}
								menuOptions={this.menuOptions}
								itemActions={this.methodActions}
							/>
						</CustomPaper>
					</Box>
				</Box>
			);
		}
	}
}

export default MethodList;
