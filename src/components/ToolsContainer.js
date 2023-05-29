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
			selectedPageId: null,
			pages: [],
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

	addPage(label, icon, component, id, onClose) {
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
		const { pages, selectedPageId } = this.state;
		var index = pages.indexOf(page);
		const selectedId =
			pages.length === 1
				? null
				: page.id !== selectedPageId
				? selectedPageId
				: index > 0
				? pages[index - 1].id
				: pages[index + 1].id;
		const filtered = pages.filter((p) => p.id !== page.id);
		if (this.props.onPageRemove) {
			this.props.onPageRemove(this);
		}
		this.setState(
			{
				selectedPageId: selectedId,
				pages: filtered,
			},
			() => {
				if (this.props.onPageRemove) {
					this.props.onPageRemove(this);
				}
			}
		);
	};

	removeAllPages = () => {
		this.setState({ pages: [] });
	};

	closePage = (page) => {
		if (page.onClose) {
			page.onClose();
		}
		this.removePage(page);
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
					styles={this.props.styles}
					text={ide.transcriptText()}
					onChange={this.transcriptChanged}
				/>
			);
			this.addPage("Transcript", <TranscriptIcon />, transcript);
		}
	};

	openSearch = () => {
		const search = <Search styles={this.props.styles} />;
		this.addPage("Search", <SearchIcon />, search);
	};

	migratePackage = (packagename) => {
		const migrator = (
			<CodeMigrator styles={this.props.styles} package={packagename} />
		);
		this.addPage("Migrate: " + packagename, <MigratorIcon />, migrator);
	};

	migrateClass = (classname) => {
		const migrator = (
			<CodeMigrator styles={this.props.styles} class={classname} />
		);
		this.addPage("Migrate: " + classname, <MigratorIcon />, migrator);
	};

	migrateMethod = (method) => {
		const migrator = (
			<CodeMigrator styles={this.props.styles} method={method} />
		);
		this.addPage(
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
		const browser = (
			<PackageBrowser
				styles={this.props.styles}
				selectedPackage={packagename}
			/>
		);
		this.addPage("Package Browser", <PackageBrowserIcon />, browser);
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
				styles={this.props.styles}
				root={classname}
				side={side}
				selectedSelector={selector}
				id={id}
			/>
		);
		this.addPage(
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
				styles={this.props.styles}
				methods={sorted}
				selectedSelector={selectedSelector}
				selectedIdentifier={selectedIdentifier}
			/>
		);
		this.addPage(
			title + " (" + methods.length + ")",
			<MethodBrowserIcon />,
			browser
		);
	};

	newWorkspace = async () => {
		try {
			const id = await ide.api.createWorkspace();
			this.openWorkspace(id);
		} catch (error) {
			this.reportError(error);
		}
	};

	openWorkspace = (id) => {
		const existing = this.state.pages.find((p) => {
			return (
				p.component.type === Workspace && p.component.props.id === id
			);
		});
		if (existing) {
			this.selectPage(existing);
			return;
		}
		const workspace = (
			<Workspace styles={this.props.styles} key={id} id={id} />
		);
		this.addPage(
			"Workspace",
			<WorkspaceIcon />,
			workspace,
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
				styles={this.props.styles}
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
		this.addPage(title, <DebuggerIcon />, tool, pageId, () => {
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
				styles={this.props.styles}
				key={object.id}
				id={object.id}
				root={object}
				showWorkspace={true}
			/>
		);
		this.addPage(
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
			<ChangesBrowser
				styles={this.props.styles}
				changeset={changeset}
				selectedChange={selected}
			/>
		);
		this.addPage(
			title + " (" + changeset.size() + ")",
			<ChangesBrowserIcon />,
			browser
		);
	};

	openResourceBrowser = (title = "Objects") => {
		const browser = <ResourceBrowser styles={this.props.styles} />;
		this.addPage(title, <InspectorIcon />, browser);
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
		const tool = <TestRunner styles={this.props.styles} key={id} id={id} />;
		this.addPage(
			title,
			<TestRunnerIcon className={this.props.styles.testRunnerIcon} />,
			tool,
			null,
			() => {
				try {
					ide.api.deleteTestRun(id);
				} catch (error) {
					this.reportError(error);
				}
			}
		);
	};

	openProfiler = (id, title = "Profiler") => {
		const tool = <Profiler styles={this.props.styles} key={id} id={id} />;
		this.addPage(
			title,
			<TestRunnerIcon className={this.props.styles.testRunnerIcon} />,
			tool
		);
	};

	openNativeDebugger = (id, title = "Native Debugger") => {
		const tool = (
			<NativeDebugger styles={this.props.styles} key={id} id={id} />
		);
		this.addPage(title, <DebuggerIcon />, tool);
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
			const tool = (
				<Chat
					styles={this.props.styles}
					channel={channel}
					initialPeer={peer}
				/>
			);
			this.addPage("Chat", <ChatIcon />, tool);
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
				/>
			);
			this.addPage("Settings", <SettingsIcon />, page);
		}
	};

	openMethodDifferences = (
		leftMethod,
		rightMethod,
		title = "Differences"
	) => {
		const browser = (
			<CodeDifferences
				styles={this.props.styles}
				leftMethod={leftMethod.source}
				rightMethod={rightMethod.source}
			/>
		);
		this.addPage(title, <MethodBrowserIcon />, browser);
	};

	openCoderLikeBrowser = (classname) => {
		const browser = (
			<CoderLikeBrowser styles={this.props.styles} root={classname} />
		);
		this.addPage(
			browser.props.root || "Class Browser",
			<ClassBrowserIcon />,
			browser
		);
	};

	browseSenders = async (selector) => {
		try {
			const senders = await ide.waitFor(() => {
				return ide.api.senders(selector);
			});
			if (senders.length === 0) {
				return ide.inform("There is no senders of " + selector);
			}
			this.openMethodBrowser(
				senders,
				"Senders of " + selector,
				selector,
				null,
				"methodClass"
			);
		} catch (error) {
			this.reportError(error);
		}
	};

	browseLocalSenders = async (selector, classname) => {
		try {
			const senders = await ide.api.localSenders(selector, classname);
			if (senders.length === 0) {
				return ide.inform("There is no local senders of " + selector);
			}
			this.openMethodBrowser(
				senders,
				"Local senders of " + selector,
				selector,
				null,
				"selector"
			);
		} catch (error) {
			this.reportError(error);
		}
	};

	browseImplementors = async (selector) => {
		try {
			const implementors = await ide.api.implementors(selector);
			if (implementors.length === 0) {
				return ide.inform("There is no implementors of " + selector);
			}
			this.openMethodBrowser(
				implementors,
				"Implementors of " + selector,
				null,
				null,
				"methodClass"
			);
		} catch (error) {
			this.reportError(error);
		}
	};

	browseLocalImplementors = async (selector, classname) => {
		try {
			const implementors = await ide.api.localImplementors(
				selector,
				classname
			);
			if (implementors.length === 0) {
				return ide.inform(
					"There is no local implementors of " + selector
				);
			}
			this.openMethodBrowser(
				implementors,
				"Local implementors of " + selector
			);
		} catch (error) {
			this.reportError(error);
		}
	};

	browseClassReferences = async (classname) => {
		try {
			const references = await ide.api.classReferences(classname);
			if (references.length === 0) {
				return ide.inform("There is no references of " + classname);
			}
			this.openMethodBrowser(
				references,
				"References to " + classname,
				null,
				classname,
				"methodClass"
			);
		} catch (error) {
			this.reportError(error);
		}
	};

	browseStringReferences = async (string) => {
		try {
			const references = await ide.api.stringReferences(string);
			if (references.length === 0) {
				return ide.inform("There is no refernces of " + string);
			}
			this.openMethodBrowser(
				references,
				"References to '" + string + "'",
				null,
				string,
				"methodClass"
			);
		} catch (error) {
			this.reportError(error);
		}
	};

	browseMethodsMatching = async (pattern) => {
		try {
			const matching = await ide.api.methodsMatching(pattern);
			if (matching.length === 0) {
				return ide.inform("There is no method matching " + pattern);
			}
			this.openMethodBrowser(
				matching,
				"Methods with selector matching " + pattern,
				null,
				null,
				"methocClass"
			);
		} catch (error) {
			this.reportError(error);
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
				this.reportError(error.description);
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
		const styles = this.props.styles;
		const { selectedPageId, pages } = this.state;
		const selectedPage = this.pageWithId(selectedPageId);
		return (
			<TabControl
				id={this.props.id}
				style={{ height: "100%" }}
				styles={styles}
				selectedPage={selectedPage}
				pages={pages}
				onTabSelect={this.selectPage}
				onClose={this.closePage}
				addOptions={this.addPageOptions()}
			/>
		);
	}
}

export default ToolsContainer;

export { container };
