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
} from "@material-ui/core";
import { ThemeProvider } from "@material-ui/styles";
import { withCookies } from "react-cookie";
import { withRouter } from "react-router-dom";
import { withDialog } from "./dialogs/index";
import { amber, blue } from "@material-ui/core/colors";
import AddIcon from "@material-ui/icons/AddCircle";
import KeyboardArrowDown from "@material-ui/icons/KeyboardArrowDown";
import API from "./API";
import { IDEContext } from "./IDEContext";
import { DialogProvider } from "./dialogs/index";
import TranscriptIcon from "./icons/TranscriptIcon";
import SystemBrowserIcon from "./icons/SystemBrowserIcon";
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
import SystemBrowser from "./tools/SystemBrowser";
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
import MethodDifferences from "./tools/MethodDifferences";
import Settings from "./Settings";
import ResourceBrowser from "./tools/ResourceBrowser";
import CoderLikeBrowser from "./tools/CoderLikeBrowser";

class IDE extends Component {
	constructor(props) {
		super(props);
		this.updateSettings();
		this.updateTheme();
		this.initializeAPI();
		this.initializeMessageChannel();
		this.state = {
			sidebarExpanded: false,
			addPageMenuOpen: false,
			selectedPage: null,
			transcriptOpen: false,
			unreadErrorsCount: 0,
			unreadMessages: 0,
			transcriptText: this.welcomeMessage(),
			pages: [],
		};
	}

	componentDidMount() {
		this.cacheNames();
		this.openTranscript();
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

	testMethodDifferences = async () => {
		const m1 = {
			class: "Number",
			selector: "roundTo:",
			source:
				'roundTo: quantum \r\t"Answer the nearest number that is a multiple of quantum."\r\r\t^(self / quantum) rounded * quantum',
		};
		const m2 = {
			class: "Number",
			selector: "roundTo:",
			source:
				"roundTo: aNumber\r\t^self < 0\r\t\tifTrue: [self - (aNumber / 2) truncateTo: aNumber]\r\t\tifFalse: [self + (aNumber / 2) truncateTo: aNumber]",
		};
		this.openMethodDifferences(m1, m2, "Method Diff");
	};

	updateSettings() {
		const cookies = this.props.cookies;
		this.dialect = cookies.get("dialect");
		this.baseUri = cookies.get("baseUri");
		this.developer = cookies.get("developer");
		this.messageChannelUrl = "http://localhost:4200";
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

	initializeAPI() {
		this.api = new API(
			this.baseUri,
			this.developer,
			this.reportError,
			this.reportChange
		);
	}

	initializeMessageChannel() {
		this.messageChannel = new MessageChannel();
		this.messageChannel.login(this.messageChannelUrl, this.developer);
		this.messageChannel.onEvent(
			"onMessageReceived",
			this.messageReceived,
			this
		);
		this.messageChannel.onEvent("onMessagesSeen", this.messageReceived, this);
	}

	messageReceived = (message) => {
		if (message.type === "text") {
			this.setState({ unreadMessages: this.messageChannel.unseenMessages() });
		}
	};

	updateTheme() {
		var mainPrimaryColor;
		var mainSecondaryColor;
		switch (this.dialect) {
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
			case "VA Smalltalk":
				mainPrimaryColor = "#9d2f69";
				mainSecondaryColor = "#3f010c";
				break;
			case "Python":
				mainPrimaryColor = "#2b5b84";
				mainSecondaryColor = "#1e415e";
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
					paper: "#303030",
				},
			},
		});
	}

	cacheNames = async () => {
		try {
			this.projectNames = await this.api.getProjectNames();
		} catch (error) {
			this.projectNames = [];
			this.reportError(error);
		}
		try {
			this.classNames = await this.api.getClassNames();
		} catch (error) {
			this.classNames = [];
			this.reportError(error);
		}
	};

	newPageId() {
		const pages = this.state.pages;
		return pages.length > 0 ? pages.sort((a, b) => a.id > b.id)[0].id + 1 : 1;
	}

	addPage(label, icon, component, id) {
		const pages = this.state.pages;
		const page = {
			id: id || this.newPageId(),
			label: label,
			icon: icon,
			component: component,
		};
		console.log(page);
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
		}
		this.setState(state);
	};

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
				page.text = this.state.transcriptText;
				this.selectPage(page);
			} else {
				const transcript = (
					<Transcript
						styles={this.props.styles}
						text={this.state.transcriptText}
					/>
				);
				this.addPage("Transcript", <TranscriptIcon />, transcript);
			}
		}
	};

	toggleShowTranscript = () => {
		this.setState({
			transcriptOpen: !this.state.transcriptOpen,
			unreadErrorsCount: 0,
		});
	};

	openInspectors = async () => {
		try {
			const objects = await this.api.getObjects();
			objects.forEach((o) => this.openInspector(o));
		} catch (error) {
			this.reportError(error);
		}
	};

	openResources = async () => {
		const page = this.state.pages.find((p) => p.label.startsWith("Resources"));
		if (page) {
			this.selectPage(page);
		} else {
			this.openResourceBrowser("Resources");
		}
	};

	openSystemBrowser = (projectname) => {
		const browser = (
			<SystemBrowser styles={this.props.styles} root={projectname} />
		);
		this.addPage(
			projectname || "System Browser",
			<SystemBrowserIcon />,
			browser
		);
	};

	browseClass = async (classname) => {
		try {
			await this.api.getClass(classname);
			this.openClassBrowser(classname);
		} catch (error) {
			this.props.dialog.alert("There is no class named " + classname);
		}
	};

	openClassBrowser = (classname, selector) => {
		const id = this.newPageId();
		const browser = (
			<ClassBrowser
				styles={this.props.styles}
				root={classname}
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

	openMethodBrowser = (methods, title = "Methods", selectedWord) => {
		const browser = (
			<MethodBrowser
				styles={this.props.styles}
				methods={methods}
				selectedWord={selectedWord}
			/>
		);
		this.addPage(
			title + " (" + methods.length + ")",
			<MethodBrowserIcon />,
			browser
		);
	};

	openWorkspace = (id) => {
		const workspace = <Workspace styles={this.props.styles} key={id} id={id} />;
		this.addPage("Workspace", <WorkspaceIcon />, workspace);
	};

	openDebugger = (id, title = "Debugger") => {
		const tool = <Debugger styles={this.props.styles} key={id} id={id} />;
		this.addPage(title, <DebuggerIcon />, tool);
	};

	closeDebugger = (id) => {
		const page = this.state.pages.find(
			(p) => p.component.type.name === "Debugger" && p.component.props.id === id
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
				root={object}
				id={object.id}
				showWorkspace
			/>
		);
		this.addPage("Inspecting: " + object.class, <InspectorIcon />, inspector);
	};

	openChangesBrowser = (changes, title = "Changes") => {
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
		const tool = <NativeDebugger styles={this.props.styles} key={id} id={id} />;
		this.addPage(title, <DebuggerIcon />, tool);
	};

	openChat = (peername) => {
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
					onSave={() => {
						this.removeAllPages();
						this.updateSettings();
						this.updateTheme();
						this.initializeAPI();
						this.initializeMessageChannel();
						this.cacheNames();
					}}
				/>
			);
			this.addPage("Settings", <SettingsIcon />, settings);
		}
	};

	openMethodDifferences = (leftMethod, rightMethod, title = "Differences") => {
		const browser = (
			<MethodDifferences
				styles={this.props.styles}
				leftMethod={leftMethod}
				rightMethod={rightMethod}
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
			const senders = await this.api.getSenders(selector);
			this.openMethodBrowser(senders, "Senders of " + selector, selector);
		} catch (error) {
			this.reportError(error);
		}
	};

	browseLocalSenders = async (selector, classname) => {
		try {
			const senders = await this.api.getLocalSenders(selector, classname);
			this.openMethodBrowser(senders, "Local senders of " + selector, selector);
		} catch (error) {
			this.reportError(error);
		}
	};

	browseImplementors = async (selector) => {
		try {
			const implementors = await this.api.getImplementors(selector);
			this.openMethodBrowser(implementors, "Implementors of " + selector);
		} catch (error) {
			this.reportError(error);
		}
	};

	browseLocalImplementors = async (selector, classname) => {
		try {
			const implementors = await this.api.getLocalImplementors(
				selector,
				classname
			);
			this.openMethodBrowser(implementors, "Local implementors of " + selector);
		} catch (error) {
			this.reportError(error);
		}
	};

	browseReferences = async (classname) => {
		try {
			const references = await this.api.getReferences(classname);
			this.openMethodBrowser(
				references,
				"References to " + classname,
				classname
			);
		} catch (error) {
			this.reportError(error);
		}
	};

	browseLastChanges = async () => {
		try {
			const changes = await this.api.getChanges();
			this.openChangesBrowser(changes, "Last changes");
		} catch (error) {
			this.reportError(error);
		}
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
			const object = await this.api.getObject(result.id);
			if (!pin && !sync) {
				await this.api.unpinObject(object.id);
			}
			return object;
		} catch (error) {
			if (error.data && error.data.evaluation) {
				// const debug = await this.confirm(error.description, 'Stack tracke:\r' + error.stack + '\r\rDo you want to debug it?');
				// (debug)? this.openDebugger(error.debugger) : this.reportError(error.description);
				const d = await this.api.createDebugger(error.data.evaluation);
				this.openDebugger(d.id);
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

	runTestProject = async (projectname) => {
		try {
			const status = await this.api.runTestProject(projectname);
			this.openTestRunner(status.id, "Test " + projectname);
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
			transcriptText: this.state.transcriptText + "\r" + text,
			unreadErrorsCount: this.state.unreadErrorsCount + 1,
		});
	};

	transcriptChanged = (text) => {
		this.setState({ transcriptText: text });
	};

	reportChange = async (change) => {
		//this triggers unnecessary renders!!!
		// const changes = await this.api.getChanges();
		// this.setState({changesCount: changes.length})
	};

	addSystemBrowserClicked = () => {
		this.setState({ addPageMenuOpen: false });
		this.openSystemBrowser();
	};

	addClassBrowserClicked = () => {
		this.setState({ addPageMenuOpen: false });
		this.openClassBrowser();
		//this.openCoderLikeBrowser();
	};

	addWorkspaceClicked = async () => {
		this.setState({ addPageMenuOpen: false });
		try {
			const id = await this.api.createWorkspace();
			this.openWorkspace(id);
		} catch (error) {
			this.reportError(error);
		}
	};

	render() {
		console.log("rendering IDE");
		const context = {
			api: this.api,
			dialect: this.dialect,
			messageChannel: this.messageChannel,
			projectNames: this.projectNames,
			classNames: this.classNames,
			openChat: this.openChat,
			openWorkspace: this.openWorkspace,
			browseProject: this.openSystemBrowser,
			browseClass: this.browseClass,
			browseSenders: this.browseSenders,
			browseLocalSenders: this.browseLocalSenders,
			browseImplementors: this.browseImplementors,
			browseLocalImplementors: this.browseLocalImplementors,
			browseReferences: this.browseReferences,
			evaluateExpression: this.evaluateExpression,
			debugExpression: this.debugExpression,
			profileExpression: this.profileExpression,
			runTest: this.runTest,
			runTestClass: this.runTestClass,
			runTestProject: this.runTestProject,
			openDebugger: this.openDebugger,
			closeDebugger: this.closeDebugger,
			inspectObject: this.openInspector,
			reportError: this.reportError,
			updatePageLabel: this.updatePageLabel,
		};
		const styles = this.props.styles;
		return (
			<IDEContext.Provider value={context}>
				<ThemeProvider theme={this.theme}>
					<DialogProvider>
						<div className={styles.root}>
							<Titlebar
								developer={this.developer}
								dialect={this.dialect}
								styles={styles}
								sidebarExpanded={this.state.sidebarExpanded}
								expandSidebar={this.expandSidebar}
								searchOptions={this.classNames || []}
								onAvatarClicked={this.openSettings}
							/>
							<Sidebar
								styles={styles}
								expanded={this.state.sidebarExpanded}
								unreadErrorsCount={this.state.unreadErrorsCount}
								unreadMessages={this.state.unreadMessages}
								onTranscriptClicked={this.openTranscript}
								onChangesClicked={this.browseLastChanges}
								onResourcesClicked={this.openResources}
								onPeersClicked={this.openChat}
								onSettingsClicked={this.openSettings}
								onClose={this.collapseSidebar}
							/>
							<main className={styles.content}>
								<div className={styles.appBarSpacer} />
								<Container className={styles.container}>
									<Grid container spacing={1} style={{ height: "100%" }}>
										<Grid item xs={11} md={11} lg={11}>
											<TabControl
												style={{ height: "100%" }}
												styles={styles}
												selectedPage={this.state.selectedPage}
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
													this.setState({ addPageMenuOpen: true });
												}}
											>
												<AddIcon style={{ fontSize: 40 }} />
											</IconButton>
											<Menu
												id="addPageMenu"
												anchorEl={document.getElementById("addPageButton")}
												keepMounted
												open={this.state.addPageMenuOpen}
												onClose={() => {
													this.setState({ addPageMenuOpen: false });
												}}
											>
												<MenuItem onClick={this.addWorkspaceClicked}>
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
												<MenuItem onClick={this.addClassBrowserClicked}>
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
												<MenuItem onClick={this.addSystemBrowserClicked}>
													<Box
														display="flex"
														flexWrap="nowrap"
														alignItems="center"
														justifyContent="center"
													>
														<Box pt={1} pr={1}>
															<SystemBrowserIcon />
														</Box>
														<Box>System Browser</Box>
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
													<Grid item xs={11} md={11} lg={11}>
														{this.state.transcriptOpen && (
															<Transcript
																styles={styles}
																text={this.state.transcriptText}
																onChange={this.transcriptChanged}
															/>
														)}
													</Grid>
													<Grid item xs={1} md={1} lg={1}>
														<Box display="flex" justifyContent="center">
															<IconButton
																onClick={() =>
																	this.setState({ transcriptOpen: false })
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
					</DialogProvider>
				</ThemeProvider>
			</IDEContext.Provider>
		);
	}
}

export default withDialog()(withRouter(withCookies(IDE)));
