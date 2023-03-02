import React, { Component } from "react";
import {
	Container,
	createMuiTheme,
	Grid,
	IconButton,
	Menu,
	MenuItem,
	Drawer,
	Box,
	Backdrop,
	CircularProgress,
} from "@material-ui/core";
import { ThemeProvider } from "@material-ui/styles";
import { withCookies } from "react-cookie";
import { withRouter } from "react-router-dom";
import { withDialog } from "./dialogs/index";
import { amber, blue, green } from "@material-ui/core/colors";
import AddIcon from "@material-ui/icons/AddCircle";
import KeyboardArrowDown from "@material-ui/icons/KeyboardArrowDown";
import API from "./API";
import { DialogProvider } from "./dialogs/index";
import CustomSnacks from "./controls/CustomSnacks";
import TranscriptIcon from "./icons/TranscriptIcon";
import SearchIcon from "@material-ui/icons/Search";
import MigratorIcon from "@material-ui/icons/Send";
import PackageBrowserIcon from "./icons/PackageBrowserIcon";
import ClassBrowserIcon from "./icons/ClassBrowserIcon";
import MethodBrowserIcon from "./icons/MethodBrowserIcon";
import WorkspaceIcon from "./icons/WorkspaceIcon";
import InspectorIcon from "./icons/InspectorIcon";
import ChangesBrowserIcon from "./icons/ChangesBrowserIcon";
import DebuggerIcon from "./icons/DebuggerIcon";
import TestRunnerIcon from "./icons/TestRunnerIcon";
import ChatIcon from "./icons/ChatIcon";
import SettingsIcon from "@material-ui/icons/Settings";
import Titlebar from "./layout/Titlebar";
import Sidebar from "./layout/Sidebar";
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
import MessageChannel from "./MessageChannel";
import Chat from "./tools/Chat";
import CodeDifferences from "./tools/CodeDifferences";
import Settings from "./Settings";
import ResourceBrowser from "./tools/ResourceBrowser";
import CoderLikeBrowser from "./tools/CoderLikeBrowser";
import CodeMigrator from "./tools/CodeMigrator";
import Hotkeys from "react-hot-keys";

var ide = null;

class IDE extends Component {
	constructor(props) {
		super(props);
		ide = this;
		this.initializeSettings();
		this.state = {
			dialect: null,
			sidebarExpanded: false,
			addPageMenuOpen: false,
			selectedPage: null,
			transcriptOpen: false,
			lastError: null,
			unreadErrorsCount: 0,
			unreadMessages: 0,
			transcriptText: this.welcomeMessage(),
			pages: [],
		};
	}

	componentDidMount() {
		//this.openTranscript();
		const classname = this.props.match.params.classname;
		if (classname) {
			this.openClassBrowser(classname);
		}
		const id = this.props.match.params.debuggerid;
		if (id) {
			this.openDebugger(id);
		}
		//this.openNativeDebugger('{B3AE5087-3EBC-43E2-B4A5-95DD37D802FE}')
	}

	usesEmergentTranscript() {
		return false;
	}

	queryOptions() {
		const query = new URLSearchParams(this.props.location.search);
		const options = {};
		for (let option of query.entries()) {
			options[option[0]] = option[1];
		}
		return options;
	}

	async updateSettings(baseUri, dialect, developer, messageChannelUrl) {
		this.baseUri = baseUri;
		this.developer = developer;
		this.dialect = dialect;
		document.title = dialect;
		this.initializeAPI(baseUri, developer);
		this.updateTheme(dialect);
		this.initializeMessageChannel(messageChannelUrl);
	}

	welcomeMessage() {
		const backend =
			this.dialect !== "undefined"
				? this.dialect
				: "It looks like the Smalltalk system could not be determined";
		return (
			'"Welcome to Webside ' +
			this.developer +
			"!\rA Smalltalk IDE for the web.\r\r" +
			"Backend: " +
			backend +
			"\r" +
			"URL: " +
			this.baseUri +
			'"'
		);
	}

	transcriptText() {
		return this.state.transcriptText;
	}

	initializeSettings = async () => {
		const options = this.queryOptions();
		this.updateSettings(
			options.baseUri,
			options.dialect,
			options.developer
		);
	};

	initializeAPI(baseUri, developer) {
		this.api = new API(
			baseUri,
			developer,
			this.reportError,
			this.reportChange
		);
	}

	initializeMessageChannel(url) {
		if (!url) {
			return;
		}
		this.messageChannel = new MessageChannel();
		this.messageChannel.login(url, this.developer);
		this.messageChannel.onEvent(
			"onMessageReceived",
			this.messageReceived,
			this
		);
		this.messageChannel.onEvent(
			"onMessagesSeen",
			this.messageReceived,
			this
		);
	}

	saveImage = async () => {
		try {
			await this.api.saveImage();
		} catch (error) {
			this.reportError(error);
		}
	};

	messageReceived = (message) => {
		if (message.type === "text") {
			this.setState({
				unreadMessages: this.messageChannel.unseenMessages(),
			});
		}
	};

	updateTheme(dialect) {
		var mainPrimaryColor;
		var mainSecondaryColor;
		switch (dialect) {
			case "Bee":
				mainPrimaryColor = amber[300];
				mainSecondaryColor = amber[800];
				break;
			case "Pharo":
				mainPrimaryColor = blue[300];
				mainSecondaryColor = blue[800];
				break;
			case "Dolphin":
				mainPrimaryColor = "#1d7bb9";
				mainSecondaryColor = blue[800];
				break;
			case "VAST":
				mainPrimaryColor = "#28AAE1";
				mainSecondaryColor = "#003865";
				break;
			case "Python":
				mainPrimaryColor = "#2b5b84";
				mainSecondaryColor = "#1e415e";
				break;
			case "Powerlang":
				mainPrimaryColor = green[300];
				mainSecondaryColor = green[800];
				break;
			default:
				mainPrimaryColor = "#00000";
				mainSecondaryColor = "#00000";
		}
		this.theme = createMuiTheme({
			typography: {
				//fontFamily: '"Segoe UI"',
				fontSize: 13,
				button: {
					textTransform: "none",
				},
			},
			palette: {
				type: "dark",
				primary: {
					main: mainPrimaryColor,
				},
				secondary: {
					main: mainSecondaryColor,
				},
				text: {
					primary: "#aaaaaa",
					secondary: "#00000",
				},
				background: {
					main: "#303030",
					paper: "#303030",
				},
			},
		});
	}

	newPageId() {
		const pages = this.state.pages;
		return pages.length > 0
			? pages.sort((a, b) => a.id > b.id)[0].id + 1
			: 1;
	}

	addPage(label, icon, component, id) {
		const pages = this.state.pages;
		const page = {
			id: id || this.newPageId(),
			label: label,
			icon: icon,
			component: component,
		};
		pages.push(page);
		const state = { pages: pages, selectedPage: page };
		if (page.label === "Transcript") {
			state.unreadErrorsCount = 0;
		}
		this.setState(state);
	}

	selectPage = (page) => {
		const state = { selectedPage: page };
		if (page.label === "Transcript") {
			state.unreadErrorsCount = 0;
			if (
				page.component &&
				page.component.ref &&
				page.component.current
			) {
				page.component.current.forceUpdate();
			}
		}
		this.setState(state);
	};

	selectPageAtOffset(offset) {
		const pages = this.state.pages;
		var page = this.state.selectedPage;
		var index = pages.indexOf(page);
		if (index >= 0) {
			index = index + offset;
			console.log(index);
			if (index < 0) {
				index = pages.length;
			} else if (index >= pages.length) {
				index = 0;
			}
			this.selectPage(pages[index]);
		}
	}

	updatePageLabel = (id, label) => {
		const page = this.state.pages.find((p) => p.id === id);
		if (page) {
			page.label = label;
		}
	};

	preRemovePage = async (page) => {
		try {
			if (page.component.type.name === "Inspector") {
				await this.api.unpinObject(page.component.props.id);
			}
			if (page.component.type.name === "Debugger") {
				await this.api.deleteDebugger(page.component.props.id);
			}
			if (page.component.type.name === "TestRunner") {
				await this.api.deleteTestRun(page.component.props.id);
			}
			if (page.component.type.name === "Workspace") {
				await this.api.deleteWorkspace(page.component.props.id);
			}
		} catch (error) {
			this.reportError(error);
		}
	};

	removePage = async (page) => {
		await this.preRemovePage(page);
		const { pages, selectedPage } = this.state;
		let i = pages.indexOf(page);
		const selected =
			pages.length === 1
				? null
				: page !== selectedPage
				? selectedPage
				: i > 0
				? pages[i - 1]
				: pages[i + 1];
		this.setState({
			pages: pages.filter((p) => p !== page),
			selectedPage: selected,
		});
	};

	removeAllPages = async () => {
		await Promise.all(
			this.state.pages.map(async (p) => await this.preRemovePage(p))
		);
		this.setState({ pages: [] });
	};

	pageLabeled(label) {
		return this.state.pages.find((p) => p.label === label);
	}

	openTranscript = () => {
		if (this.usesEmergentTranscript()) {
			this.toggleShowTranscript();
		} else {
			const page = this.pageLabeled("Transcript");
			if (page) {
				this.selectPage(page);
			} else {
				const ref = React.createRef();
				const transcript = (
					<Transcript
						ref={ref}
						styles={this.props.styles}
						text={this.state.transcriptText}
						onChange={this.transcriptChanged}
					/>
				);
				this.addPage(
					"Transcript",
					<TranscriptIcon />,
					transcript,
					true
				);
			}
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

	toggleShowTranscript = () => {
		this.setState({
			transcriptOpen: !this.state.transcriptOpen,
			unreadErrorsCount: 0,
		});
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
			await this.api.classNamed(name);
			this.openClassBrowser(name, side);
		} catch (error) {
			this.props.dialog.alert("There is no class named " + name);
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
		selectedWord,
		sortedBy
	) => {
		const sorted = sortedBy
			? methods.sort((a, b) => (a[sortedBy] <= b[sortedBy] ? -1 : 1))
			: methods;
		const browser = (
			<MethodBrowser
				styles={this.props.styles}
				methods={sorted}
				selectedWord={selectedWord}
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
			const id = await this.api.createWorkspace();
			this.openWorkspace(id);
		} catch (error) {
			this.reportError(error);
		}
	};

	openWorkspace = (id) => {
		const workspace = (
			<Workspace styles={this.props.styles} key={id} id={id} />
		);
		this.addPage("Workspace", <WorkspaceIcon />, workspace);
	};

	openDebugger = (id, title = "Debugger") => {
		const tool = (
			<Debugger
				styles={this.props.styles}
				key={id}
				id={id}
				title={title}
			/>
		);
		this.addPage(title, <DebuggerIcon />, tool);
	};

	closeDebugger = (id) => {
		const page = this.state.pages.find(
			(p) =>
				p.component.type.name === "Debugger" &&
				p.component.props.id === id
		);
		if (page) {
			this.removePage(page);
		}
	};

	openInspector = (object) => {
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
			inspector
		);
	};

	browseChanges = (changes, title = "Changes") => {
		const browser = (
			<ChangesBrowser styles={this.props.styles} changes={changes} />
		);
		this.addPage(
			title + " (" + changes.length + ")",
			<ChangesBrowserIcon />,
			browser
		);
	};

	openResourceBrowser = (title = "Objects") => {
		const browser = <ResourceBrowser styles={this.props.styles} />;
		this.addPage(title, <InspectorIcon />, browser);
	};

	openTestRunner = (id, title = "Test Runner") => {
		const tool = <TestRunner styles={this.props.styles} key={id} id={id} />;
		this.addPage(
			title,
			<TestRunnerIcon className={this.props.styles.testRunnerIcon} />,
			tool
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
		if (!this.messageChannel) {
			this.props.dialog.alert("There is no channel for chatting");
			return;
		}
		if (peername === this.developer) return;
		const peer = this.messageChannel.peerNamed(peername);
		if (peername && !peer) return;
		const page = this.pageLabeled("Chat");
		if (page) {
			this.selectPage(page);
		} else {
			const tool = (
				<Chat
					styles={this.props.styles}
					channel={this.messageChannel}
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
			const settings = (
				<Settings
					styles={this.props.styles}
					baseUri={this.baseUri}
					developer={this.developer}
					acceptLabel="Save"
					onAccept={(baseUri, dialect, developer) => {
						this.removeAllPages();
						this.props.history.push(
							"/ide?baseUri=" +
								baseUri +
								"&dialect=" +
								dialect +
								"&developer=" +
								developer
						);
						this.updateSettings(baseUri, dialect, developer);
					}}
				/>
			);
			this.addPage("Settings", <SettingsIcon />, settings);
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
			const senders = await this.api.senders(selector);
			this.openMethodBrowser(
				senders,
				"Senders of " + selector,
				selector,
				"methodClass"
			);
		} catch (error) {
			this.reportError(error);
		}
	};

	browseLocalSenders = async (selector, classname) => {
		try {
			const senders = await this.api.localSenders(selector, classname);
			this.openMethodBrowser(
				senders,
				"Local senders of " + selector,
				selector,
				"selector"
			);
		} catch (error) {
			this.reportError(error);
		}
	};

	browseImplementors = async (selector) => {
		try {
			const implementors = await this.api.implementors(selector);
			this.openMethodBrowser(
				implementors,
				"Implementors of " + selector,
				null,
				"methodClass"
			);
		} catch (error) {
			this.reportError(error);
		}
	};

	browseLocalImplementors = async (selector, classname) => {
		try {
			const implementors = await this.api.localImplementors(
				selector,
				classname
			);
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
			const references = await this.api.classReferences(classname);
			this.openMethodBrowser(
				references,
				"References to " + classname,
				classname,
				"methodClass"
			);
		} catch (error) {
			this.reportError(error);
		}
	};

	browseStringReferences = async (string) => {
		try {
			const references = await this.api.stringReferences(string);
			this.openMethodBrowser(
				references,
				"References to '" + string + "'",
				string,
				"methodClass"
			);
		} catch (error) {
			this.reportError(error);
		}
	};

	browseMethodsMatching = async (pattern) => {
		try {
			const matching = await this.api.methodsMatching(pattern);
			this.openMethodBrowser(
				matching,
				"Methods with selector matching " + pattern,
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
				const changes = await this.api.lastChanges();
				this.browseChanges(changes, "Last changes");
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
						const changes = await this.api.uploadChangeset(ch);
						this.browseChanges(changes);
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
			const id = await this.api.debugExpression(expression, context);
			this.openDebugger(id, "Debugging expression");
		} catch (error) {
			this.reportError(error);
		}
	};

	evaluateExpression = async (expression, sync, pin, context) => {
		try {
			const result = await this.api.evaluateExpression(
				expression,
				sync,
				pin,
				context
			);
			if (sync) {
				return result;
			}
			const object = await this.api.objectWithId(result.id);
			if (!pin && !sync) {
				await this.api.unpinObject(object.id);
			}
			return object;
		} catch (error) {
			if (error.data && error.data.evaluation) {
				// const debug = await this.confirm(error.description, 'Stack tracke:\r' + error.stack + '\r\rDo you want to debug it?');
				// (debug)? this.openDebugger(error.debugger) : this.reportError(error.description);
				const d = await this.api.createDebugger(error.data.evaluation);
				this.openDebugger(d.id, d.description);
			}
		}
	};

	runTest = async (classname, selector) => {
		try {
			const status = await this.api.runTest(classname, selector);
			this.openTestRunner(status.id, "Test " + selector);
		} catch (error) {
			this.reportError(error);
		}
	};

	runTestClass = async (classname) => {
		try {
			const status = await this.api.runTestClass(classname);
			this.openTestRunner(status.id, "Test " + classname);
		} catch (error) {
			this.reportError(error);
		}
	};

	runTestPackage = async (packagename) => {
		try {
			const status = await this.api.runTestPackage(packagename);
			this.openTestRunner(status.id, "Test " + packagename);
		} catch (error) {
			this.reportError(error);
		}
	};

	profileExpression = async (expression, context) => {
		try {
			const id = await this.api.profileExpression(expression, context);
			this.openProfiler(id);
		} catch (error) {
			this.reportError(error);
		}
	};

	expandSidebar = () => {
		this.setState({ sidebarExpanded: true });
	};

	collapseSidebar = () => {
		this.setState({ sidebarExpanded: false });
	};

	reportError = (text) => {
		if (!text) {
			return;
		}
		this.setState({
			lastError: text.toString(),
			transcriptText: this.state.transcriptText + "\r" + text,
			unreadErrorsCount: this.state.unreadErrorsCount + 1,
		});
	};

	transcriptChanged = (text) => {
		this.setState({ transcriptText: text });
	};

	reportChange = async (change) => {
		//this triggers unnecessary renders!!!
		// const changes = await this.api.lastChanges();
		// this.setState({changesCount: changes.length})
	};

	addPackageBrowserClicked = () => {
		this.setState({ addPageMenuOpen: false });
		this.openPackageBrowser();
	};

	addClassBrowserClicked = () => {
		this.setState({ addPageMenuOpen: false });
		this.openClassBrowser();
		//this.openCoderLikeBrowser();
	};

	addWorkspaceClicked = async () => {
		this.setState({ addPageMenuOpen: false });
		this.newWorkspace();
	};

	addChangesBrowserClicked = async () => {
		this.setState({ addPageMenuOpen: false });
		try {
			this.browseChangesFromFile();
		} catch (error) {
			this.reportError(error);
		}
	};

	hotkeyPressed = async (hotkey) => {
		switch (hotkey) {
			case "ctrl+b":
				this.openClassBrowser();
				break;
			case "ctrl+alt+w":
				this.newWorkspace();
				break;
			case "ctrl+alt+left":
				this.selectPageAtOffset(-1);
				break;
			case "ctrl+alt+right":
				this.selectPageAtOffset(1);
				break;
			default:
		}
	};

	render() {
		console.log("rendering IDE");
		const styles = this.props.styles;
		return (
			<Hotkeys
				keyName="ctrl+b, ctrl+alt+w, ctrl+alt+left, ctrl+alt+right"
				filter={(event) => {
					return true;
				}}
				allowRepeat={false}
				onKeyDown={(hotkey, e, handle) => this.hotkeyPressed(hotkey)}
			>
				<ThemeProvider theme={this.theme}>
					<DialogProvider>
						<div className={styles.root}>
							<Titlebar
								developer={this.developer}
								dialect={this.dialect}
								styles={styles}
								sidebarExpanded={this.state.sidebarExpanded}
								expandSidebar={this.expandSidebar}
								searchOptions={[]}
								onAvatarClicked={this.openSettings}
							/>
							<Sidebar
								styles={styles}
								expanded={this.state.sidebarExpanded}
								unreadErrorsCount={this.state.unreadErrorsCount}
								unreadMessages={this.state.unreadMessages}
								onSaveImageClicked={this.saveImage}
								onTranscriptClicked={this.openTranscript}
								onSearchClicked={this.openSearch}
								onChangesClicked={this.browseLastChanges}
								onResourcesClicked={this.openResources}
								onPeersClicked={this.openChat}
								onClose={this.collapseSidebar}
							/>
							<main className={styles.content}>
								<div className={styles.appBarSpacer} />
								<Container className={styles.container}>
									<Grid
										container
										spacing={1}
										style={{ height: "100%" }}
									>
										<Grid item xs={11} md={11} lg={11}>
											<TabControl
												style={{ height: "100%" }}
												styles={styles}
												selectedPage={
													this.state.selectedPage
												}
												pages={this.state.pages}
												onSelect={this.selectPage}
												onClose={this.removePage}
											/>
										</Grid>
										<Grid item xs={1} md={1} lg={1}>
											<IconButton
												id="addPageButton"
												color="primary"
												onClick={() => {
													this.setState({
														addPageMenuOpen: true,
													});
												}}
											>
												<AddIcon
													style={{ fontSize: 40 }}
												/>
											</IconButton>
											<Menu
												id="addPageMenu"
												anchorEl={document.getElementById(
													"addPageButton"
												)}
												keepMounted
												open={
													this.state.addPageMenuOpen
												}
												onClose={() => {
													this.setState({
														addPageMenuOpen: false,
													});
												}}
											>
												<MenuItem
													onClick={
														this.addWorkspaceClicked
													}
												>
													<Box
														display="flex"
														flexWrap="nowrap"
														alignItems="center"
														justifyContent="center"
													>
														<Box pt={1} pr={1}>
															<WorkspaceIcon />
														</Box>
														<Box>Workspace</Box>
													</Box>
												</MenuItem>
												<MenuItem
													onClick={
														this
															.addClassBrowserClicked
													}
												>
													<Box
														display="flex"
														flexWrap="nowrap"
														alignItems="center"
														justifyContent="center"
													>
														<Box pt={1} pr={1}>
															<ClassBrowserIcon />
														</Box>
														<Box>Class Browser</Box>
													</Box>
												</MenuItem>
												<MenuItem
													onClick={
														this
															.addPackageBrowserClicked
													}
												>
													<Box
														display="flex"
														flexWrap="nowrap"
														alignItems="center"
														justifyContent="center"
													>
														<Box pt={1} pr={1}>
															<PackageBrowserIcon />
														</Box>
														<Box>
															Package Browser
														</Box>
													</Box>
												</MenuItem>
												<MenuItem
													onClick={
														this
															.addChangesBrowserClicked
													}
												>
													<Box
														display="flex"
														flexWrap="nowrap"
														alignItems="center"
														justifyContent="center"
													>
														<Box pt={1} pr={1}>
															<ChangesBrowserIcon />
														</Box>
														<Box>
															Changes Browser
														</Box>
													</Box>
												</MenuItem>
											</Menu>
										</Grid>
										<React.Fragment key="bottom">
											<Drawer
												anchor="bottom"
												variant="persistent"
												open={this.state.transcriptOpen}
											>
												<Grid container spacing={0}>
													<Grid
														item
														xs={11}
														md={11}
														lg={11}
													>
														{this.state
															.transcriptOpen && (
															<Transcript
																styles={styles}
																text={
																	this.state
																		.transcriptText
																}
																onChange={
																	this
																		.transcriptChanged
																}
															/>
														)}
													</Grid>
													<Grid
														item
														xs={1}
														md={1}
														lg={1}
													>
														<Box
															display="flex"
															justifyContent="center"
														>
															<IconButton
																onClick={() =>
																	this.setState(
																		{
																			transcriptOpen: false,
																		}
																	)
																}
															>
																<KeyboardArrowDown />
															</IconButton>
														</Box>
													</Grid>
												</Grid>
											</Drawer>
										</React.Fragment>
									</Grid>
								</Container>
							</main>
						</div>
						<CustomSnacks
							open={this.state.lastError !== null}
							onClose={() => this.setState({ lastError: null })}
							text={this.state.lastError}
							severity="error"
						/>
					</DialogProvider>
				</ThemeProvider>
			</Hotkeys>
		);
	}
}

export default withDialog()(withRouter(withCookies(IDE)));

export { ide };
