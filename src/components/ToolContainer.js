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
import SettingsEditor from "./tools/SettingsEditor";
import ResourceBrowser from "./tools/ResourceBrowser";
import CodeMigrator from "./tools/CodeMigrator";
import Changeset from "../model/StChangeset";
import { ide } from "./IDE";
import ToolContainerContext from "./ToolContainerContext";
import ResourcesIcon from "./icons/ResourcesIcon";
import POC from "./tools/POC";
import SystemBrowser from "./tools/SystemBrowser";
import { v4 as uuidv4 } from "uuid";
import MethodHistoryBrowser from "./tools/MethodHistoryBrowser";
import BackendTester from "./tools/BackendTester";
import CompilationResult from "../model/CompilationResult";
import ReleaseNotes from "./tools/ReleaseNotes";
import ReleaseNotesIcon from "@mui/icons-material/NewReleases";

class ToolContainer extends Component {
	constructor(props) {
		super(props);
		this.state = {
			selectedPageId:
				props.pages && props.pages.length > 0
					? props.pages[0].id
					: null,
			pages: props.pages || [],
		};
	}

	newPageId() {
		return uuidv4();
	}

	createPage(label, icon, component, id, ref, nextToSelected) {
		const { pages, selectedPageId } = this.state;
		const labelRef = React.createRef();
		const page = {
			id: id || this.newPageId(),
			label: label,
			icon: icon,
			component: component,
			ref: ref,
			labelRef: labelRef,
		};
		if (nextToSelected) {
			const selectedPage = this.pageWithId(selectedPageId);
			const index = pages.indexOf(selectedPage) + 1;
			pages.splice(index, 0, page);
		} else {
			pages.push(page);
		}
		this.setState({ pages: pages, selectedPageId: page.id });
	}

	addPage(page) {
		const pages = this.state.pages;
		pages.push(page);
		this.setState({ pages: pages, selectedPageId: page.id });
	}

	pages() {
		return this.state.pages;
	}

	selectPage = (page) => {
		const previous = this.pageWithId(this.state.selectedPageId);
		if (previous) this.aboutToDeselectPage(previous);
		const state = { selectedPageId: page.id };
		this.aboutToSelectPage(page);
		this.setState(state);
		if (this.props.onPageSelect) this.props.onPageSelect(this, page);
	};

	pageFocused = (page) => {
		if (this.props.onPageFocus) this.props.onPageFocus(this, page);
	};

	aboutToDeselectPage(page) {
		if (page && page.ref && page.ref.current)
			page.ref.current.aboutToDeselect();
	}

	aboutToSelectPage(page) {
		if (page && page.ref && page.ref.current)
			page.ref.current.aboutToSelect();
	}

	selectPageAtOffset(offset) {
		const { pages, selectedPageId } = this.state;
		var page = this.pageWithId(selectedPageId);
		var index = pages.indexOf(page);
		if (index >= 0) {
			index = index + offset;
			if (index < 0) {
				index = pages.length - 1;
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
		//if (pages.length === 0) return;
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
		if (selectedPageId !== selectedId) {
			this.aboutToSelectPage(this.pageWithId(selectedId));
		}
		this.setState(
			{
				selectedPageId: selectedId,
				pages: filtered,
			},
			() => {
				if (this.props.onPagesRemove) this.props.onPagesRemove(this);
			}
		);
	};

	reorderPages = (from, to) => {
		if (from === to) return;
		const pages = [...this.state.pages];
		const moved = pages.splice(from, 1)[0];
		pages.splice(to, 0, moved);
		this.setState({ pages: pages });
	};

	removeAllPages = () => {
		this.setState({ pages: [] });
	};

	closePage = (page) => {
		this.removePage(page);
	};

	async aboutToClosePage(page) {
		if (page.ref && page.ref.current) {
			await page.ref.current.aboutToClose();
		}
	}

	closePages = async (pages) => {
		await Promise.all(
			pages.map(async (page) => {
				await this.aboutToClosePage(page);
			})
		);
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
			ide.resetUnredErrorCount();
			const ref = React.createRef();
			const transcript = (
				<Transcript
					ref={ref}
					text={ide.transcriptText()}
					onChange={this.transcriptChanged}
				/>
			);
			this.createPage(
				"Transcript",
				<TranscriptIcon />,
				transcript,
				null,
				ref,
				false
			);
		}
	};

	openPOC = () => {
		this.createPage("POC", <TranscriptIcon />, <POC />);
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

	openPackageBrowser = async (packagename) => {
		const pageId = this.newPageId();
		let pack;
		if (packagename) {
			try {
				pack = await ide.backend.packageNamed(packagename);
			} catch (error) {}
		}
		const ref = React.createRef();
		const browser = (
			<SystemBrowser
				ref={ref}
				showPackages={true}
				preselectedPackage={pack}
				id={pageId}
			/>
		);
		this.createPage(
			"Package Browser",
			<PackageBrowserIcon />,
			browser,
			pageId,
			ref
		);
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
			await ide.backend.classNamed(name);
			this.openClassBrowser(name, side, true);
		} catch (error) {
			ide.inform("There is no class named " + name);
		}
	};

	browseMethod = (method) => {
		this.openMethodBrowser([method]);
	};

	openClassBrowser = async (classname, side, nextToSelected) => {
		const pageId = this.newPageId();
		let browser, species;
		if (classname) {
			try {
				species = await ide.backend.classNamed(classname);
				species.metaclass = await ide.backend.classNamed(species.class);
			} catch (error) {
				ide.reportError(error);
			}
		}
		const ref = React.createRef();
		browser = (
			<SystemBrowser
				ref={ref}
				preselectedClass={species}
				side={side}
				id={pageId}
			/>
		);
		this.createPage(
			classname || "Class Browser",
			<ClassBrowserIcon />,
			browser,
			pageId,
			ref,
			nextToSelected
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
		const pageId = this.newPageId();
		const ref = React.createRef();
		const browser = (
			<MethodBrowser
				ref={ref}
				methods={sorted}
				selectedSelector={selectedSelector}
				selectedIdentifier={selectedIdentifier}
				id={pageId}
				title={title}
			/>
		);
		this.createPage(
			title + " (" + methods.length + ")",
			<MethodBrowserIcon />,
			browser,
			null,
			ref,
			true
		);
	};

	newWorkspace = async () => {
		try {
			const workspace = await ide.backend.createWorkspace();
			this.openWorkspace(workspace.id);
		} catch (error) {
			this.reportError(error);
		}
	};

	openWorkspace = async (id, nextToSelected) => {
		const existing = this.state.pages.find((p) => {
			return (
				p.component.type === Workspace && p.component.props.id === id
			);
		});
		if (existing) {
			this.selectPage(existing);
			return;
		}
		let source = "";
		let name;
		if (id) {
			try {
				const workspace = await ide.backend.workspace(id);
				source = workspace.source;
				name = workspace.name;
			} catch (error) {
				this.reportError(error);
			}
		}
		const ref = React.createRef();
		const component = (
			<Workspace ref={ref} key={id} id={id} name={name} source={source} />
		);
		this.createPage(
			name || "Workspace",
			<WorkspaceIcon />,
			component,
			id,
			ref,
			nextToSelected
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
		const ref = React.createRef();
		const pageId = this.newPageId();
		const tool = (
			<Debugger
				ref={ref}
				key={id}
				id={id}
				title={title}
				onResume={() => {
					this.removePageWithId(pageId);
					if (onResume) onResume();
				}}
				onTerminate={() => {
					this.removePageWithId(pageId);
					if (onTerminate) onTerminate();
				}}
			/>
		);
		this.createPage(title, <DebuggerIcon />, tool, pageId, ref, true);
	};

	openInspector = (object, preserve = false) => {
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
		const ref = React.createRef();
		const inspector = (
			<Inspector
				ref={ref}
				key={object.id}
				id={object.id}
				root={object}
				showWorkspace={true}
				preserveObject={preserve}
			/>
		);
		this.createPage(
			"Inspecting: " + object.class,
			<InspectorIcon />,
			inspector,
			null,
			ref,
			true
		);
	};

	browseChanges = (changeset, title = "Changes") => {
		const selected = changeset.size() > 0 ? changeset.changes[0] : null;
		const pageId = this.newPageId();
		const browser = (
			<ChangesBrowser
				changeset={changeset}
				title={title}
				selectedChange={selected}
				id={pageId}
			/>
		);
		this.createPage(
			title + " (" + changeset.size() + ")",
			<ChangesBrowserIcon />,
			browser,
			pageId
		);
	};

	openResourceBrowser = (title = "Objects") => {
		const ref = React.createRef();
		const browser = <ResourceBrowser ref={ref} />;
		this.createPage(title, <ResourcesIcon />, browser, null, ref, false);
	};

	openTestRunner = (id, title = "Test Runner") => {
		const ref = React.createRef();
		const existing = this.state.pages.find((p) => {
			return (
				p.component.type === TestRunner && p.component.props.id === id
			);
		});
		if (existing) {
			this.selectPage(existing);
			return;
		}
		const tool = <TestRunner ref={ref} key={id} id={id} />;
		this.createPage(title, <TestRunnerIcon />, tool, null, ref, false);
	};

	openProfiler = (expression, context, title = "Profiler") => {
		const tool = <Profiler expression={expression} context={context} />;
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
			const settings = ide.settings.copy();
			const page = (
				<SettingsEditor
					settings={settings}
					onApply={(settings) => {
						ide.applySettings(settings);
					}}
					onResetSection={(path) => {
						ide.resetSettingsSection(path);
					}}
				/>
			);
			this.createPage("Settings", <SettingsIcon />, page);
		}
	};

	openReleaseNotes = () => {
		const page = this.pageLabeled("Release Notes");
		if (page) {
			this.selectPage(page);
		} else {
			const page = <ReleaseNotes />;
			this.createPage("Release Notes", <ReleaseNotesIcon />, page);
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

	browseSenders = async (selector) => {
		const senders = await ide.searchMethods(() => {
			return ide.backend.senders(selector);
		}, "senders of " + selector);
		if (senders && senders.length > 0) {
			this.openMethodBrowser(
				senders,
				"Senders of " + selector,
				selector,
				null,
				"methodClass"
			);
		}
	};

	browseSendersInList = async (selector, methods) => {
		let senders = await ide.searchMethods(() => {
			return ide.backend.senders(selector);
		}, "senders of " + selector + " in list");
		senders = senders.filter((s) =>
			methods.find(
				(m) =>
					m.methodClass === s.methodClass && m.selector === s.selector
			)
		);
		if (senders && senders.length > 0) {
			this.openMethodBrowser(
				senders,
				"Senders of " + selector + " in list",
				selector,
				null,
				"methodClass"
			);
		}
	};

	browseLocalSenders = async (selector, classname) => {
		const senders = await ide.searchMethods(() => {
			return ide.backend.localSenders(selector, classname);
		}, "local senders of " + selector);
		if (senders && senders.length > 0) {
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
			return ide.backend.implementors(selector);
		}, "implementors of " + selector);
		if (implementors && implementors.length > 0) {
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
			return ide.backend.localImplementors(selector, classname);
		}, "local implementors of " + selector);
		if (implementors && implementors.length > 0) {
			this.openMethodBrowser(
				implementors,
				"Local implementors of " + selector
			);
		}
	};

	browseClassReferences = async (classname) => {
		const references = await ide.searchMethods(() => {
			return ide.backend.classReferences(classname);
		}, "references to " + classname);
		if (references && references.length > 0) {
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
			return ide.backend.stringReferences(string);
		}, "references to " + string);
		if (references && references.length > 0) {
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
			return ide.backend.methodsMatching(pattern);
		}, "methods with selector matching " + pattern);
		if (matching && matching.length > 0) {
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
				const changes = await ide.backend.lastChanges();
				const changeset = new Changeset(ide.backend);
				changeset.fromJson(changes);
				this.browseChanges(changeset, "Last changes");
			} catch (error) {
				this.reportError(error);
			}
		}
	};

	browseChangesFromFile = async () => {
		var input = document.createElement("input");
		input.type = "file";
		input.onchange = async (e) => {
			var file = e.target.files[0];
			if (file) {
				const changeset = await ide.waitFor(
					() => this.loadChangesetFromFile(file),
					"Loading changeset..."
				);
				if (changeset) this.browseChanges(changeset, file.name);
			}
		};
		input.click();
	};

	loadChangesetFromFile = async (file) => {
		return new Promise((resolve, reject) => {
			const reader = new FileReader();
			reader.onload = async () => {
				try {
					const changes = await ide.backend.uploadChangeset(
						reader.result
					);
					const changeset = new Changeset(ide.backend);
					changeset.fromJson(changes);
					resolve(changeset);
				} catch (error) {
					this.reportError(error);
					reject(error);
				}
			};
			reader.onerror = (e) => reject(e);
			reader.readAsArrayBuffer(file);
		});
	};

	browseMethodHistory(method) {
		this.createPage(
			"History of " + method.methodClass + ">>" + method.selector,
			<ChangesBrowserIcon />,
			<MethodHistoryBrowser method={method} />,
			null,
			null,
			true
		);
	}

	debugExpression = async (expression, context) => {
		try {
			const id = await ide.backend.debugExpression(expression, context);
			this.openDebugger(id, "Debugging expression");
		} catch (error) {
			this.reportError(error);
		}
	};

	async debugsError(error) {
		// Don't ask for confirmation by the moment...
		const confirms = false;
		if (!confirms) return true;
		return await ide.confirm({
			title: error.description,
			message:
				"Stack trace:\r" + error.stack + "\r\rDo you want to debug it?",
			ok: { text: "Debug", variant: "outlined" },
		});
	}

	compileMethod = async (classname, packagename, category, source) => {
		let method;
		try {
			const change = await ide.backend.compileMethod(
				classname,
				packagename,
				category,
				source
			);
			method = await ide.backend.method(classname, change.selector);
		} catch (error) {
			return this.handleCompilationError(error, classname);
		}
		return new CompilationResult(method);
	};

	async handleCompilationError(error, classname) {
		const data = error.data;
		if (data && data.suggestions && data.suggestions.length > 0) {
			const suggestions = data.suggestions;
			let suggestion;
			if (suggestions.length === 1) {
				let confirm = await ide.confirm({
					title: data.description,
					message: suggestions[0].description,
				});
				if (confirm) suggestion = suggestions[0];
			} else {
				let description = await ide.choose({
					title: data.description,
					message: "What do you want to do?",
					items: suggestions.map((s) => s.description),
					defaultValue: suggestions[0].description,
				});
				if (description) {
					suggestion = suggestions.find(
						(s) => s.description === description
					);
				}
			}
			if (suggestion) {
				try {
					let applied, method;
					for (const change of suggestion.changes) {
						applied = await ide.backend.postChange(change);
					}
					if (applied && applied.type === "AddMethod") {
						try {
							method = await ide.backend.method(
								classname,
								applied.selector
							);
						} catch (ignored) {}
					}
					return new CompilationResult(method);
				} catch (inner) {
					return this.handleCompilationError(inner, classname);
				}
			}
		} else {
			return new CompilationResult(null, error);
		}
	}

	evaluateExpression = async (
		expression,
		sync,
		pin,
		context,
		assignee,
		silent = false
	) => {
		const evaluation = {
			expression: expression,
			sync: sync,
			pin: pin,
			context: context,
			assignee: assignee,
		};
		var result;
		try {
			result = await ide.backend.issueEvaluation(evaluation);
			if (sync) return result;
		} catch (error) {
			if (!silent && error.data && error.data.evaluation) {
				evaluation.id = error.evaluation;
				evaluation.error = error.data;
				return this.handleEvaluationError(evaluation);
			} else {
				return this.reportError(error);
			}
		}
		evaluation.id = result.id;
		const object = await this.waitForEvaluationResult(evaluation, silent);
		if (!pin && !sync) {
			try {
				await ide.backend.unpinObject(object.id);
			} catch (ignored) {}
		}
		return object;
	};

	waitForEvaluationFinalization = async (evaluation) => {
		let delay = ide.settings
			.section("advanced")
			.get("evaluationPollingFrequency");
		let finalization;
		const promise = new Promise((resolve, _) => {
			finalization = resolve;
		});
		const interval = setInterval(async () => {
			try {
				const retrieved = await ide.backend.evaluation(evaluation.id);
				Object.assign(evaluation, retrieved);
				if (
					["finished", "cancelled", "failed"].includes(
						evaluation.state
					)
				)
					finalization(evaluation);
			} catch (error) {
				if (error.status === 404) {
					// Cancelling could result into this...
					return finalization({ ...evaluation, state: "cancelled" });
				}
				this.reportError(error);
			}
		}, delay);
		const final = await promise;
		clearInterval(interval);
		return final;
	};

	waitForEvaluationResult = async (evaluation, silent) => {
		const final = await this.waitForEvaluationFinalization(evaluation);
		if (final.state === "cancelled") return null;
		if (final.state === "failed") {
			return silent
				? this.reportError(
						evaluation.error.description ||
							evaluation.error.toString()
				  )
				: this.handleEvaluationError(final);
		}
		try {
			return await ide.backend.objectWithId(evaluation.id);
		} catch (error) {
			this.reportError(error);
		}
	};

	handleEvaluationError = async (evaluation) => {
		const error = evaluation.error;
		if (error.suggestions && error.suggestions.length > 0) {
			const chosen = await ide.choose({
				title: error.description,
				message: "What do you want to do?",
				items: error.suggestions.map((s) => s.description),
				defaultValue: error.suggestions[0].description,
			});
			const suggestion = chosen
				? error.suggestions.find((s) => s.description === chosen)
				: null;
			if (chosen) {
				try {
					for (const change of suggestion.changes) {
						await ide.backend.postChange(change);
					}
					await ide.backend.cancelEvaluation(evaluation.id);
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
			if (await this.debugsError(error)) {
				var object;
				await this.debugEvaluation(evaluation).then(
					async () => {
						object = await this.waitForEvaluationResult(evaluation);
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

	async debugEvaluation(evaluation) {
		const d = await ide.backend.createDebugger(evaluation.id);
		return new Promise((resolve, reject) => {
			this.openDebugger(d.id, d.description, resolve, reject);
		});
	}

	runTest = async (classname, selector, silently, onRun) => {
		try {
			const status = await ide.backend.runTest(classname, selector);
			silently
				? ide.followTestRun(status.id, true, onRun)
				: this.openTestRunner(status.id, "Test " + selector);
		} catch (error) {
			this.reportError(error);
		}
	};

	runTestClass = async (classname, silently, onRun) => {
		try {
			const status = await ide.backend.runTestClass(classname);
			silently
				? ide.followTestRun(status.id, onRun)
				: this.openTestRunner(status.id, "Test " + classname);
		} catch (error) {
			this.reportError(error);
		}
	};

	runTestPackage = async (packagename, silently, onRun) => {
		try {
			const status = await ide.backend.runTestPackage(packagename);
			silently
				? ide.followTestRun(status.id, onRun)
				: this.openTestRunner(status.id, "Test " + packagename);
		} catch (error) {
			this.reportError(error);
		}
	};

	profileExpression = async (expression, context) => {
		this.openProfiler(expression, context);
	};

	transcriptChanged = (text) => {
		ide.transcriptChanged(text);
	};

	addChangesBrowserClicked = async () => {
		try {
			this.browseChangesFromFile();
		} catch (error) {
			this.reportError(error);
		}
	};

	addPageOptions() {
		const shortcuts = ide.settings.section("shortcuts");
		return [
			{
				label: "Workspace",
				shortcut: shortcuts.get("newWorkspace"),
				icon: <WorkspaceIcon />,
				handler: this.newWorkspace,
			},
			{
				label: "System Browser",
				shortcut: shortcuts.get("openSystemBrowser"),
				icon: <PackageBrowserIcon />,
				handler: this.openPackageBrowser,
			},
			{
				label: "Class Browser",
				shortcut: shortcuts.get("openClassBrowser"),
				icon: <ClassBrowserIcon />,
				handler: this.openClassBrowser,
			},
			{
				label: "Changes Browser",
				icon: <ChangesBrowserIcon />,
				handler: this.addChangesBrowserClicked,
			},
		];
	}

	openBackendTester = (url) => {
		const page = <BackendTester url={url} />;
		this.createPage("Backend Tester", <SettingsIcon />, page);
	};

	render() {
		const showClose = this.props.showClose;
		const { selectedPageId, pages } = this.state;
		const selectedPage = this.pageWithId(selectedPageId);
		return (
			<ToolContainerContext.Provider value={this}>
				<TabControl
					id={this.props.id}
					style={{ width: "100%", height: "100%" }}
					selectedPage={selectedPage}
					pages={pages}
					onTabSelect={this.selectPage}
					onTabsClose={this.closePages}
					addOptions={this.addPageOptions()}
					onTabSplit={this.splitPage}
					onTabFocus={this.pageFocused}
					showClose={showClose}
					onTabsReorder={this.reorderPages}
				/>
			</ToolContainerContext.Provider>
		);
	}
}

export default ToolContainer;
