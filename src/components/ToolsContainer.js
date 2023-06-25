import React, { Component } from "react";
import TranscriptIcon from "./icons/TranscriptIcon";
import SearchIcon from "@mui/icons-material/Search";
import MigratorIcon from "@mui/icons-material/Send";
import PackageBrowserIcon from "./icons/PackageBrowserIcon";
import ClassBrowserIcon from "./icons/ClassBrowserIcon";
import MethodBrowserIcon from "./icons/MethodBrowserIcon";
import WorkspaceIcon from "./icons/WorkspaceIcon";
import InspectorIcon from "./icons/InspectorIcon";
import ChangesBrowserIcon from "./icons/ChangesBrowserIcon";
import DebuggerIcon from "./icons/DebuggerIcon";
import TestRunnerIcon from "./icons/TestRunnerIcon";
import ChatIcon from "./icons/ChatIcon";
import SettingsIcon from "@mui/icons-material/Settings";
import TabControl from "./controls/TabControl";
import Transcript from "./tools/Transcript";
import Search from "./tools/Search";
import PackageBrowser from "./tools/PackageBrowser";
import ClassBrowser from "./tools/ClassBrowser";
import MethodBrowser from "./tools/MethodBrowser";
import Inspector from "./tools/Inspector";
import Workspace from "./tools/Workspace";
import ChangesBrowser from "./tools/ChangesBrowser";
import Debugger from "./tools/Debugger";
import TestRunner from "./tools/TestRunner";
import Profiler from "./tools/Profiler";
import NativeDebugger from "./tools/NativeDebugger";
import Chat from "./tools/Chat";
import CodeDifferences from "./tools/CodeDifferences";
import SettingsEditor from "./SettingsEditor";
import ResourceBrowser from "./tools/ResourceBrowser";
import CoderLikeBrowser from "./tools/CoderLikeBrowser";
import CodeMigrator from "./tools/CodeMigrator";
import Changeset from "../model/StChangeset";
import { ide } from "./IDE";

var container = null;

class ToolsContainer extends Component {
	constructor(props) {
		super(props);
		container = this;
		this.state = {
			selectedPageId: props.pages ? props.pages[0].id : null,
			pages: props.pages || [],
		};
	}

	newPageId() {
		const pages = this.state.pages;
		if (pages.length === 0) {
			return 0;
		}
		const sorted = pages.map((p) => p.id).sort();
		const maxId = sorted[sorted.length - 1];
		if (1 == 1) {
			//We don't recycle ids for the moment;
			return maxId + 1;
		}
		const used = new Array(maxId);
		sorted.forEach((id) => (used[id] = true));
		const unused = used.findIndex((id) => id !== true);
		return unused === -1 ? maxId + 1 : unused;
	}

	createPage(label, icon, component, id, onClose) {
		const pages = this.state.pages;
		const labelRef = React.createRef();
		const page = {
			id: id || this.newPageId(),
			label: label,
			icon: icon,
			component: component,
			labelRef: labelRef,
			onClose: onClose,
		};
		pages.push(page);
		const state = { pages: pages, selectedPageId: page.id };
		if (page.label === "Transcript") {
			ide.resetUnredErrorCount();
		}
		this.setState(state);
	}

	addPage(page) {
		const pages = this.state.pages;
		page.id = this.newPageId();
		pages.push(page);
		this.setState({ pages: pages, selectedPageId: page.id });
	}

	pages() {
		return this.state.pages;
	}

	selectPage = (page) => {
		const state = { selectedPageId: page.id };
		if (page.label === "Transcript") {
			ide.resetUnredErrorCount();
			if (
				page.component &&
				page.component.ref &&
				page.component.ref.current
			) {
				page.component.ref.current.forceUpdate();
			}
		}
		this.setState(state);
	};

	selectPageAtOffset(offset) {
		const pages = this.state.pages;
		var page = this.pageWithId(this.state.selectedPageId);
		var index = pages.indexOf(page);
		if (index >= 0) {
			index = index + offset;
			if (index < 0) {
				index = pages.length;
			} else if (index >= pages.length) {
				index = 0;
			}
			this.selectPage(pages[index]);
		}
	}

	updatePageLabel = (id, label) => {
		const page = this.pageWithId(id);
		if (page && page.labelRef && page.labelRef.current) {
			page.label = label || page.label;
			page.labelRef.current.changeLabel(page.label);
		}
	};

	removePageWithId(id) {
		const page = this.pageWithId(id);
		if (page) {
			this.removePage(page);
		}
	}

	removePage = (page) => {
		this.removePages([page]);
	};

	removePages = (pages) => {
		if (pages.length === 0) return;
		const selectedPageId = this.state.selectedPageId;
		const currentPages = this.state.pages;
		const ids = pages.map((p) => p.id);
		const indexes = pages.map((p) => currentPages.indexOf(p)).sort();
		const i0 = indexes[0];
		const ik = indexes[indexes.length - 1];
		const selectedId =
			currentPages.length === pages.length
				? null
				: !ids.includes(selectedPageId)
				? selectedPageId
				: i0 > 0
				? currentPages[i0 - 1].id
				: ik < currentPages.length - 1
				? currentPages[ik + 1].id
				: null;
		const filtered = currentPages.filter((p) => !ids.includes(p.id));
		if (this.props.onPagesRemove) {
			this.props.onPagesRemove(this);
		}
		this.setState(
			{
				selectedPageId: selectedId,
				pages: filtered,
			},
			() => {
				if (this.props.onPagesRemove) {
					this.props.onPagesRemove(this);
				}
			}
		);
	};

	removeAllPages = () => {
		this.setState({ pages: [] });
	};

	closePage = (page) => {
		this.removePage(page);
	};

	closePages = (pages) => {
		pages.forEach((p) => {
			if (p.onClose) {
				p.onClose();
			}
		});
		this.removePages(pages);
	};

	splitPage = (page) => {
		this.props.onPageSplit(this, page);
	};

	pageWithId(id) {
		return this.state.pages.find((p) => p.id === id);
	}

	pageLabeled(label) {
		return this.state.pages.find((p) => p.label === label);
	}

	reportError = (error) => {
		ide.reportError(error);
	};

	openTranscript = () => {
		const page = this.pageLabeled("Transcript");
		if (page) {
			this.selectPage(page);
		} else {
			const ref = React.createRef();
			const transcript = (
				<Transcript
					ref={ref}
					text={ide.transcriptText()}
					onChange={this.transcriptChanged}
				/>
			);
			this.createPage("Transcript", <TranscriptIcon />, transcript);
		}
	};

	openSearch = () => {
		const search = <Search />;
		this.createPage("Search", <SearchIcon />, search);
	};

	migratePackage = (packagename) => {
		const migrator = <CodeMigrator package={packagename} />;
		this.createPage("Migrate: " + packagename, <MigratorIcon />, migrator);
	};

	migrateClass = (classname) => {
		const migrator = <CodeMigrator class={classname} />;
		this.createPage("Migrate: " + classname, <MigratorIcon />, migrator);
	};

	migrateMethod = (method) => {
		const migrator = <CodeMigrator method={method} />;
		this.createPage(
			"Migrate: " + method.methodClass + ">>" + method.selector,
			<MigratorIcon />,
			migrator
		);
	};

	openResources = async () => {
		const page = this.pageLabeled("Resources");
		if (page) {
			this.selectPage(page);
		} else {
			this.openResourceBrowser("Resources");
		}
	};

	openPackageBrowser = (packagename) => {
		const browser = <PackageBrowser selectedPackage={packagename} />;
		this.createPage("Package Browser", <PackageBrowserIcon />, browser);
	};

	browsePackage(name) {
		this.openPackageBrowser(name);
	}

	browseClass = async (classname) => {
		try {
			var name = classname;
			var side = "instance";
			if (name.endsWith(" class")) {
				name = name.slice(0, name.length - 6);
				side = "class";
			}
			await ide.api.classNamed(name);
			this.openClassBrowser(name, side);
		} catch (error) {
			ide.inform("There is no class named " + name);
		}
	};

	browseMethod = (method) => {
		this.openMethodBrowser([method]);
	};

	openClassBrowser = (classname, side, selector) => {
		const id = this.newPageId();
		const browser = (
			<ClassBrowser
				root={classname}
				side={side}
				selectedSelector={selector}
				id={id}
			/>
		);
		this.createPage(
			classname || "Class Browser",
			<ClassBrowserIcon />,
			browser,
			id
		);
	};

	openMethodBrowser = (
		methods,
		title = "Methods",
		selectedSelector,
		selectedIdentifier,
		sortedBy
	) => {
		const sorted = sortedBy
			? methods.sort((a, b) => (a[sortedBy] <= b[sortedBy] ? -1 : 1))
			: methods;
		const browser = (
			<MethodBrowser
				methods={sorted}
				selectedSelector={selectedSelector}
				selectedIdentifier={selectedIdentifier}
			/>
		);
		this.createPage(
			title + " (" + methods.length + ")",
			<MethodBrowserIcon />,
			browser
		);
	};

	newWorkspace = async () => {
		try {
			const workspace = await ide.api.createWorkspace();
			this.openWorkspace(workspace.id);
		} catch (error) {
			this.reportError(error);
		}
	};

	openWorkspace = async (id) => {
		const existing = this.state.pages.find((p) => {
			return (
				p.component.type === Workspace && p.component.props.id === id
			);
		});
		if (existing) {
			this.selectPage(existing);
			return;
		}
		var source = "";
		if (id) {
			const workspace = await ide.api.workspace(id);
			source = workspace.source;
		}
		const component = <Workspace key={id} id={id} source={source} />;
		this.createPage(
			"Workspace",
			<WorkspaceIcon />,
			component,
			null,
			async () => {
				try {
					await ide.api.deleteWorkspace(id);
				} catch (error) {
					this.reportError(error);
				}
			}
		);
	};

	openDebugger = (id, title = "Debugger", onResume, onTerminate) => {
		const existing = this.state.pages.find((p) => {
			return p.component.type === Debugger && p.component.props.id === id;
		});
		if (existing) {
			this.selectPage(existing);
			return;
		}
		const pageId = this.newPageId();
		const tool = (
			<Debugger
				key={id}
				id={id}
				title={title}
				onResume={() => {
					this.removePageWithId(pageId);
					if (onResume) {
						onResume();
					}
				}}
				onTerminate={() => {
					this.removePageWithId(pageId);
					if (onTerminate) {
						onTerminate();
					}
				}}
			/>
		);
		this.createPage(title, <DebuggerIcon />, tool, pageId, () => {
			try {
				ide.api.deleteDebugger(id);
			} catch (error) {
				this.reportError(error);
			}
			if (onTerminate) {
				onTerminate();
			}
		});
	};

	openInspector = (object) => {
		const existing = this.state.pages.find((p) => {
			return (
				p.component.type === Inspector &&
				p.component.props.id === object.id
			);
		});
		if (existing) {
			this.selectPage(existing);
			return;
		}
		const inspector = (
			<Inspector
				key={object.id}
				id={object.id}
				root={object}
				showWorkspace={true}
			/>
		);
		this.createPage(
			"Inspecting: " + object.class,
			<InspectorIcon />,
			inspector,
			null,
			() => {
				try {
					ide.api.unpinObject(object.id);
				} catch (error) {
					this.reportError(error);
				}
			}
		);
	};

	browseChanges = (changeset, title = "Changes") => {
		const selected = changeset.size() > 0 ? changeset.changes[0] : null;
		const browser = (
			<ChangesBrowser changeset={changeset} selectedChange={selected} />
		);
		this.createPage(
			title + " (" + changeset.size() + ")",
			<ChangesBrowserIcon />,
			browser
		);
	};

	openResourceBrowser = (title = "Objects") => {
		const browser = <ResourceBrowser />;
		this.createPage(title, <InspectorIcon />, browser);
	};

	openTestRunner = (id, title = "Test Runner") => {
		const existing = this.state.pages.find((p) => {
			return (
				p.component.type === TestRunner && p.component.props.id === id
			);
		});
		if (existing) {
			this.selectPage(existing);
			return;
		}
		const tool = <TestRunner key={id} id={id} />;
		this.createPage(title, <TestRunnerIcon />, tool, null, () => {
			try {
				ide.api.deleteTestRun(id);
			} catch (error) {
				this.reportError(error);
			}
		});
	};

	openProfiler = (id, title = "Profiler") => {
		const tool = <Profiler key={id} id={id} />;
		this.createPage(title, <TestRunnerIcon />, tool);
	};

	openNativeDebugger = (id, title = "Native Debugger") => {
		const tool = <NativeDebugger key={id} id={id} />;
		this.createPage(title, <DebuggerIcon />, tool);
	};

	openChat = (peername) => {
		const channel = ide.messageChannel;
		if (!channel) {
			ide.reportError("There is no channel for chatting");
			return;
		}
		if (peername === this.developer) return;
		const peer = channel.peerNamed(peername);
		if (peername && !peer) return;
		const page = this.pageLabeled("Chat");
		if (page) {
			this.selectPage(page);
		} else {
			const tool = <Chat channel={channel} initialPeer={peer} />;
			this.createPage("Chat", <ChatIcon />, tool);
		}
	};

	openSettings = () => {
		const page = this.pageLabeled("Settings");
		if (page) {
			this.selectPage(page);
		} else {
			const settings = ide.settings;
			const page = (
				<SettingsEditor
					settings={settings}
					onApply={(settings) => {
						ide.applySettings(settings);
					}}
					onResetSection={(name) => {
						ide.resetSettingsSection(name);
					}}
				/>
			);
			this.createPage("Settings", <SettingsIcon />, page);
		}
	};

	openMethodDifferences = (
		leftMethod,
		rightMethod,
		title = "Differences"
	) => {
		const browser = (
			<CodeDifferences
				leftMethod={leftMethod.source}
				rightMethod={rightMethod.source}
			/>
		);
		this.createPage(title, <MethodBrowserIcon />, browser);
	};

	openCoderLikeBrowser = (classname) => {
		const browser = <CoderLikeBrowser root={classname} />;
		this.createPage(
			browser.props.root || "Class Browser",
			<ClassBrowserIcon />,
			browser
		);
	};

	browseSenders = async (selector) => {
		const senders = await ide.searchMethods(() => {
			return ide.api.senders(selector);
		}, "senders of " + selector);
		if (senders.length > 0) {
			this.openMethodBrowser(
				senders,
				"Senders of " + selector,
				selector,
				null,
				"methodClass"
			);
		}
	};

	browseLocalSenders = async (selector, classname) => {
		const senders = await ide.searchMethods(() => {
			return ide.api.localSenders(selector, classname);
		}, "local senders of " + selector);
		if (senders.length > 0) {
			this.openMethodBrowser(
				senders,
				"Local senders of " + selector,
				selector,
				null,
				"selector"
			);
		}
	};

	browseImplementors = async (selector) => {
		const implementors = await ide.searchMethods(() => {
			return ide.api.implementors(selector);
		}, "implementors of " + selector);
		if (implementors.length > 0) {
			this.openMethodBrowser(
				implementors,
				"Implementors of " + selector,
				null,
				null,
				"methodClass"
			);
		}
	};

	browseLocalImplementors = async (selector, classname) => {
		const implementors = await ide.searchMethods(() => {
			return ide.api.localImplementors(selector, classname);
		}, "local implementors of " + selector);
		if (implementors.length > 0) {
			this.openMethodBrowser(
				implementors,
				"Local implementors of " + selector
			);
		}
	};

	browseClassReferences = async (classname) => {
		const references = await ide.searchMethods(() => {
			return ide.api.classReferences(classname);
		}, "references to " + classname);
		if (references.length > 0) {
			this.openMethodBrowser(
				references,
				"References to " + classname,
				null,
				classname,
				"methodClass"
			);
		}
	};

	browseStringReferences = async (string) => {
		const references = await ide.searchMethods(() => {
			return ide.api.stringReferences(string);
		}, "references to " + string);
		if (references.length > 0) {
			this.openMethodBrowser(
				references,
				"References to '" + string + "'",
				null,
				string,
				"methodClass"
			);
		}
	};

	browseMethodsMatching = async (pattern) => {
		const matching = await ide.searchMethods(() => {
			return ide.api.methodsMatching(pattern);
		}, "methods with selector matching " + pattern);
		if (matching.length > 0) {
			this.openMethodBrowser(
				matching,
				"Methods with selector matching " + pattern,
				null,
				null,
				"methocClass"
			);
		}
	};

	browseLastChanges = async () => {
		const page = this.pageLabeled("Last changes");
		if (page) {
			this.selectPage(page);
		} else {
			try {
				const changes = await ide.api.lastChanges();
				const changeset = Changeset.fromJson(changes);
				changeset.on(ide.api);
				this.browseChanges(changeset, "Last changes");
			} catch (error) {
				this.reportError(error);
			}
		}
	};

	browseChangesFromFile = async () => {
		var input = document.createElement("input");
		input.type = "file";
		input.onchange = (e) => {
			var file = e.target.files[0];
			if (file) {
				var reader = new FileReader();
				reader.onload = async () => {
					const ch = reader.result;
					try {
						const changes = await ide.api.uploadChangeset(ch);
						const changeset = Changeset.fromJson(changes);
						changeset.on(ide.api);
						this.browseChanges(changeset, file.name);
					} catch (error) {
						this.reportError(error);
					}
				};
				reader.readAsText(file, "UTF-8");
			}
		};
		input.click();
	};

	debugExpression = async (expression, context) => {
		try {
			const id = await ide.api.debugExpression(expression, context);
			this.openDebugger(id, "Debugging expression");
		} catch (error) {
			this.reportError(error);
		}
	};

	async canDebugError(error) {
		if (!error.data || !error.data.evaluation) {
			return false;
		}
		// Don't ask for confirmation by the moment
		const confirms = false;
		if (!confirms) {
			return true;
		}
		return await ide.confirm({
			title: error.data.description,
			message:
				"Stack trace:\r" +
				error.data.stack +
				"\r\rDo you want to debug it?",
			ok: { text: "Debug", variant: "outlined" },
		});
	}

	evaluateExpression = async (expression, sync, pin, context, assignee) => {
		const evaluation = {
			expression: expression,
			context: context,
			sync: sync,
			pin: pin,
			assignee: assignee,
		};
		var result;
		try {
			result = await ide.api.issueEvaluation(evaluation);
			if (sync) {
				return result;
			}
		} catch (error) {
			evaluation.id = error.evaluation;
			return this.handleEvaluationError(error, evaluation);
		}
		evaluation.id = result.id;
		const object = await this.getEvaluationResult(evaluation);
		if (!pin && !sync) {
			try {
				await ide.api.unpinObject(object.id);
			} catch (ignored) {}
		}
		return object;
	};

	async getEvaluationResult(evaluation) {
		var object;
		try {
			object = await ide.api.objectWithId(evaluation.id);
		} catch (error) {
			return await this.handleEvaluationError(error, evaluation);
		}
		return object;
	}

	handleEvaluationError = async (error, evaluation) => {
		const data = error.data;
		if (data && data.suggestions && data.suggestions.length > 0) {
			const chosen = await ide.choose({
				title: data.description,
				message: "What do you want to do?",
				items: data.suggestions.map((s) => s.description),
				defaultValue: data.suggestions[0].description,
			});
			const suggestion = chosen
				? data.suggestions.find((s) => s.description === chosen)
				: null;
			if (chosen) {
				try {
					for (const change of suggestion.changes) {
						await ide.api.postChange(change);
					}
					await ide.api.cancelEvaluation(evaluation.id);
				} catch (inner) {
					this.reportError(inner.description);
				}
				return await this.evaluateExpression(
					suggestion.expression,
					evaluation.sync,
					evaluation.pin,
					evaluation.context,
					evaluation.assignee
				);
			}
		} else {
			if (await this.canDebugError(error)) {
				var object;
				await this.debugEvaluationError(error, evaluation).then(
					async () => {
						object = await this.getEvaluationResult(evaluation);
					},
					() => {
						console.log("nothing should happen from here");
					}
				);
				return object;
			} else {
				this.reportError(error.description || error.toString());
			}
		}
	};

	async debugEvaluationError(error, evaluation) {
		const d = await ide.api.createDebugger(evaluation.id);
		return new Promise((resolve, reject) => {
			this.openDebugger(d.id, d.description, resolve, reject);
		});
	}

	runTest = async (classname, selector, silently) => {
		try {
			const status = await ide.api.runTest(classname, selector);
			silently
				? ide.followTestRun(status.id, true)
				: this.openTestRunner(status.id, "Test " + selector);
		} catch (error) {
			this.reportError(error);
		}
	};

	runTestClass = async (classname, silently) => {
		try {
			const status = await ide.api.runTestClass(classname);
			silently
				? ide.followTestRun(status.id)
				: this.openTestRunner(status.id, "Test " + classname);
		} catch (error) {
			this.reportError(error);
		}
	};

	runTestPackage = async (packagename, silently) => {
		try {
			const status = await ide.api.runTestPackage(packagename);
			silently
				? ide.followTestRun(status.id)
				: this.openTestRunner(status.id, "Test " + packagename);
		} catch (error) {
			this.reportError(error);
		}
	};

	profileExpression = async (expression, context) => {
		try {
			const id = await ide.api.profileExpression(expression, context);
			this.openProfiler(id);
		} catch (error) {
			this.reportError(error);
		}
	};

	transcriptChanged = (text) => {
		ide.transcriptChanged(text);
	};

	addPackageBrowserClicked = () => {
		this.openPackageBrowser();
	};

	addClassBrowserClicked = () => {
		this.openClassBrowser();
	};

	addWorkspaceClicked = async () => {
		this.newWorkspace();
	};

	addChangesBrowserClicked = async () => {
		try {
			this.browseChangesFromFile();
		} catch (error) {
			this.reportError(error);
		}
	};

	addPageOptions() {
		return [
			{
				label: "Workspace",
				icon: <WorkspaceIcon />,
				handler: this.addWorkspaceClicked,
			},
			{
				label: "Class Browser",
				icon: <ClassBrowserIcon />,
				handler: this.addClassBrowserClicked,
			},
			{
				label: "Package Browser",
				icon: <PackageBrowserIcon />,
				handler: this.addPackageBrowserClicked,
			},
			{
				label: "Changes Browser",
				icon: <ChangesBrowserIcon />,
				handler: this.addChangesBrowserClicked,
			},
		];
	}

	render() {
		const { selectedPageId, pages } = this.state;
		const selectedPage = this.pageWithId(selectedPageId);
		return (
			<TabControl
				id={this.props.id}
				style={{ width: "100%", height: "100%" }}
				selectedPage={selectedPage}
				pages={pages}
				onTabSelect={this.selectPage}
				onTabsClose={this.closePages}
				addOptions={this.addPageOptions()}
				onTabSplit={this.splitPage}
			/>
		);
	}
}

export default ToolsContainer;

export { container };
