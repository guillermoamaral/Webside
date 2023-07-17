import React, { Component } from "react";
import {
	Container,
	Grid,
	IconButton,
	Drawer,
	Box,
	Backdrop,
	CircularProgress,
} from "@mui/material";
import ToolContainer from "./ToolContainer";
import { withCookies } from "react-cookie";
import { withNavigation } from "./withNavigation";
import { withDialog } from "./dialogs/index";
import { amber, blue, green } from "@mui/material/colors";
import KeyboardArrowDown from "@mui/icons-material/KeyboardArrowDown";
import Backend from "./Backend";
import { DialogProvider } from "./dialogs/index";
import CustomSnacks from "./controls/CustomSnacks";
import Titlebar from "./layout/Titlebar";
import Sidebar from "./layout/Sidebar";
import Transcript from "./tools/Transcript";
import MessageChannel from "./MessageChannel";
import Hotkeys from "react-hot-keys";
//import SplitIcon from "@mui/icons-material/VerticalSplit";
import DrawerHeader from "./layout/DrawerHeader";
import { Settings } from "../model/Settings";
import { app as mainApp } from "../App";
import { Setting } from "../model/Settings";
import CodeAssistant from "./CodeAssistant";

var ide = null;

class IDE extends Component {
	constructor(props) {
		super(props);
		ide = this;
		this.initializeSettings();
		this.mainContainerRef = React.createRef();
		this.state = {
			sidebarExpanded: false,
			transcriptOpen: false,
			lastMessage: null,
			unreadErrorsCount: 0,
			unreadMessages: 0,
			transcriptText: this.welcomeMessage(),
			extraContainers: [],
			waiting: false,
		};
	}

	componentDidMount() {
		const container = this.mainContainer();
		container.openTranscript();
		//container.openPOC();
		const options = this.queryOptions();
		if (options.classname) {
			container.openClassBrowser(options.classname);
		}
		if (options.debugger) {
			container.openDebugger(options.debugger);
		}
	}

	// Settings
	initializeSettings = () => {
		this.settings = this.defaultSettings();
		this.loadSettingsFromCookie();
		this.updateConnectionSettings();
		this.updateSettings();
	};

	updateConnectionSettings() {
		const options = this.queryOptions();
		const connection = this.settings.section("connection");
		connection.set("backend", options.backend);
		connection.set("developer", options.developer);
	}

	defaultSettings() {
		const settings = new Settings("settings");
		const connection = settings.addSection("connection");
		connection.addUrl("backend");
		connection.addText("developer");
		connection.addText("dialect").readOnly();

		const appearance = settings.addSection("appearance");
		appearance.addOptions(
			"fontSize",
			[8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24],
			14
		);
		appearance.addOptions("mode", ["dark", "light"]);

		const light = appearance.addSection("light", "Light Settings");
		const lightColors = light.addSection("colors", "Dialect Colors");
		lightColors.addColor("primaryColor", "#cccccc").readOnly();
		lightColors.addColor("secondaryColor", "#cccccc").readOnly();
		lightColors.addColor("background", "#ffffff").readOnly();
		lightColors.addColor("primaryText", "#000000").readOnly();
		lightColors.addColor("secondaryText", "#808080").readOnly();
		lightColors.addColor("appliedChange", "green");
		lightColors.addColor("unappliedChange", "black");
		const lightCode = light.addSection("code", "Code Colors");
		lightCode.addColor("selector", "black");
		lightCode.addColor("symbol", "#2aa9b2");
		lightCode.addColor("argument", "#f06520");
		lightCode.addColor("temporary", "#28739f");
		lightCode.addColor("assignment", "#000000");
		lightCode.addColor("string", "#00b32d");
		lightCode.addColor("variable", "#268bd2");
		lightCode.addColor("meta", "#ffcb6b");
		lightCode.addColor("bracket", "#9b9b9b");
		lightCode.addColor("reserved", "#c792ea");
		lightCode.addColor("return", "#72bb19");
		lightCode.addColor("global", "#a22598");
		lightCode.addColor("number", "#65a14e");
		lightCode.addColor("comment", "#586e75");
		lightCode.addColor("separator", "#b3bab6");

		const dark = appearance.addSection("dark", "Dark Settings");
		const darkColors = dark.addSection("colors", "Dialect Colors");
		darkColors.addColor("primaryColor", "#ffffff").readOnly();
		darkColors.addColor("secondaryColor", "#cccccc").readOnly();
		darkColors.addColor("background", "#303030").readOnly();
		darkColors.addColor("primaryText", "#aaaaaa").readOnly();
		darkColors.addColor("secondaryText", "#00000").readOnly();
		darkColors.addColor("appliedChange", "#c0ff61");
		darkColors.addColor("unappliedChange", "#ffffff");
		const darkCode = dark.addSection("code", "Code Colors");
		darkCode.addColor("selector", "#d3dddd");
		darkCode.addColor("symbol", "#3cd2dd");
		darkCode.addColor("argument", "#f06520");
		darkCode.addColor("temporary", "#81c9f3");
		darkCode.addColor("assignment", "#ffffff");
		darkCode.addColor("string", "#c3e88d");
		darkCode.addColor("variable", "#268bd2");
		darkCode.addColor("meta", "#ffcb6b");
		darkCode.addColor("bracket", "#9b9b9b");
		darkCode.addColor("reserved", "#c792ea");
		darkCode.addColor("return", "#72bb19");
		darkCode.addColor("global", "#bb73b5");
		darkCode.addColor("number", "#65a14e");
		darkCode.addColor("comment", "#586e75");
		darkCode.addColor("separator", "#b3bab6");

		const shortcuts = settings.addSection("shortcuts");
		shortcuts.addText("evaluateExpression", "Ctrl+d");
		shortcuts.addText("inspectEvaluation", "Ctrl+i");
		shortcuts.addText("showEvaluation", "Ctrl+p");
		shortcuts.addText("debugExpression", "Ctrl+u");
		shortcuts.addText("acceptCode", "Ctrl+s");
		shortcuts.addText("browseClass", "Ctrl+b");
		shortcuts.addText("browseSenders", "Alt+n");
		shortcuts.addText("browseImplementors", "Alt+m");
		shortcuts.addText("browseClassReferences", "Alt+r");
		shortcuts.readOnly();

		const openAI = settings.addSection("openAI", "OpenAI API");
		openAI.addText("apiKey");
		return settings;
	}

	applySettings(settings) {
		const hard =
			this.settings.section("connection").get("backend") !==
				settings.section("connection").get("backend") ||
			this.settings.section("connection").get("developer") !==
				settings.section("connection").get("developer");
		this.settings = settings;
		this.storeSettingsIntoCookie();
		const connection = this.settings.section("connection");
		if (hard) {
			this.props.navigate(
				"/ide?backend=" +
					connection.get("backend") +
					"&developer=" +
					connection.get("developer")
			);
			this.removeExtraContainers();
		}
		this.updateSettings();
	}

	resetSettingsSection(name) {
		const section = this.defaultSettings().section(name);
		this.settings.setSection(name, section);
		this.applySettings(this.settings);
	}

	toggleColorMode = () => {
		const appearance = this.settings.section("appearance");
		appearance.set(
			"mode",
			appearance.get("mode") === "dark" ? "light" : "dark"
		);
		this.storeSettingsIntoCookie();
		this.updateTheme();
		this.forceUpdate();
	};

	settingsCookieName() {
		const options = this.queryOptions();
		return (
			"webside-settings-for-backend-" +
			options.backend +
			"-developer-" +
			options.developer
		);
	}

	loadSettingsFromCookie() {
		const raw = this.props.cookies.get(this.settingsCookieName());
		if (raw) {
			this.settings.fromJson(JSON.parse(raw));
		}
	}

	storeSettingsIntoCookie() {
		this.props.cookies.set(
			this.settingsCookieName(),
			JSON.stringify(JSON.stringify(this.settings.toJson()))
		);
	}

	async updateSettings() {
		this.initializeBackend();
		const dialect = await this.backend.dialect();
		document.title = dialect;
		this.settings.section("connection").set("dialect", dialect);
		this.updateDialectColorsSettings();
		this.updateTheme();
		this.initializeMessageChannel();
	}

	updateDialectColorsSettings() {
		var primary;
		var secondary;
		const dialect = this.settings.section("connection").get("dialect");
		switch (dialect) {
			case "Bee":
				primary = "#eebd00";
				secondary = amber[800];
				break;
			case "Pharo":
				primary = "#3297d4";
				secondary = blue[800];
				break;
			case "Dolphin":
				primary = "#1d7bb9";
				secondary = blue[800];
				break;
			case "VAST":
				primary = "#28AAE1";
				secondary = "#003865";
				break;
			case "Python":
				primary = "#2b5b84";
				secondary = "#1e415e";
				break;
			case "Powerlang":
				primary = green[300];
				secondary = green[800];
				break;
			default:
				primary = "#00000";
				secondary = "#00000";
		}
		const appearance = this.settings.section("appearance");
		const dark = appearance.section("dark").section("colors");
		dark.set("primaryColor", primary);
		dark.set("secondaryColor", secondary);
		dark.set("appliedChange", primary);
		const light = appearance.section("light").section("colors");
		light.set("primaryColor", Setting.adjustColor(primary, -40));
		light.set("secondaryColor", secondary);
		light.set("appliedChange", Setting.adjustColor(primary, -60));
	}

	updateTheme() {
		mainApp.updateTheme(this.settings);
	}

	usesCodeAssistant() {
		const key = ide.settings.section("openAI").get("apiKey");
		return key !== undefined && key !== "";
	}

	codeAssistant() {
		return new CodeAssistant();
	}

	initializeBackend() {
		const connection = this.settings.section("connection");
		this.backend = new Backend(
			connection.get("backend"),
			connection.get("developer"),
			this.reportError,
			this.reportChange
		);
	}

	initializeMessageChannel() {
		const url = this.settings
			.section("connection")
			.get("messageChannelUrl");
		if (!url) {
			return;
		}
		this.messageChannel = new MessageChannel();
		this.messageChannel.login(url, this.settings.connection.developer);
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

	welcomeMessage() {
		const connection = this.settings.section("connection");
		const backend =
			connection.get("dialect") !== "undefined"
				? connection.get("dialect")
				: "It looks like the Smalltalk system could not be determined";
		return (
			'"Welcome to Webside ' +
			connection.get("developer") +
			"!\rA Smalltalk IDE for the web.\r\r" +
			"Backend: " +
			backend +
			"\r" +
			"URL: " +
			connection.get("backend") +
			'"'
		);
	}

	transcriptText() {
		return this.state.transcriptText;
	}

	mainContainer() {
		return this.mainContainerRef.current;
	}

	// Containers...

	newContainerId() {
		const containers = this.state.extraContainers;
		if (containers.length === 0) {
			return 0;
		}
		const sorted = containers.map((p) => p.id).sort();
		const maxId = sorted[sorted.length - 1];
		return maxId + 1;
		//We don't recycle ids for the moment"
		// const used = new Array(maxId);
		// sorted.forEach((id) => (used[id] = true));
		// const unused = used.findIndex((id) => id !== true);
		// return unused === -1 ? maxId + 1 : unused;
	}

	addContainer = (pages) => {
		const containers = this.state.extraContainers;
		const id = this.newContainerId();
		const ref = React.createRef();
		const container = {
			id: id,
			component: (
				<ToolContainer
					id={id}
					ref={ref}
					onPagesRemove={(c) => {
						if (c.pages().length === 0) {
							this.removeContainer(c);
						}
					}}
					onPageSplit={this.splitPage}
					pages={pages}
				/>
			),
		};
		containers.push(container);
		this.setState({
			extraContainers: containers,
		});
	};

	removeContainer(container) {
		const containers = this.state.extraContainers.filter((c) => {
			return c.id !== container.props.id;
		});
		this.setState({
			extraContainers: containers,
		});
	}

	removeExtraContainers() {
		this.setState({
			extralContainers: [],
		});
	}

	saveImage = async () => {
		try {
			await this.backend.saveImage();
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

	toggleShowTranscript = () => {
		this.setState({
			transcriptOpen: !this.state.transcriptOpen,
			unreadErrorsCount: 0,
		});
	};

	openSettings = () => {
		this.mainContainer().openSettings();
	};

	openTranscript = () => {
		if (this.usesEmergentTranscript()) {
			this.toggleShowTranscript();
		} else {
			this.mainContainer().openTranscript();
		}
	};

	openSearch = () => {
		this.mainContainer().openSearch();
	};

	browseLastChanges = () => {
		this.mainContainer().browseLastChanges();
	};

	browseClass = (classname) => {
		this.mainContainer().browseClass(classname);
	};

	openResources = () => {
		this.mainContainer().openResources();
	};

	openChat = () => {
		this.mainContainer().openChat();
	};

	async followTestRun(id, debug) {
		try {
			const status = await this.backend.testRunStatus(id);
			if (status.running) {
				setTimeout(async () => {
					await this.followTestRun(id);
				}, 1000);
			}
			if (!status.running) {
				const results = await this.backend.testRunResults(id);
				const passed = results.passed.length;
				const failed = results.failed.length;
				const errors = results.errors.length;
				if (debug && (failed > 0 || errors > 0)) {
					const test =
						failed > 0 ? results.failed[0] : results.errors[0];
					const d = await this.backend.debugTest(
						id,
						test.class,
						test.selector
					);
					this.openDebugger(
						d.id,
						d.description || "Debugging test " + test.selector
					);
				} else {
					await this.backend.deleteTestRun(id);
				}
				var text = "";
				var first = true;
				[
					"passed",
					"failed",
					"errors",
					"skipped",
					"knownIssues",
				].forEach((type) => {
					text +=
						(first ? "" : ", ") + results[type].length + " " + type;
					first = false;
				});
				const type =
					passed > 0 && failed === 0 && errors === 0
						? "success"
						: failed > 0 && errors === 0
						? "warning"
						: errors > 0
						? "error"
						: "info";
				this.setState({
					lastMessage: { type: type, text: text },
				});
			}
		} catch (error) {
			this.reportError(error);
		}
	}

	expandSidebar = () => {
		this.setState({ sidebarExpanded: true });
	};

	collapseSidebar = () => {
		this.setState({ sidebarExpanded: false });
	};

	splitPage = (container, page) => {
		const containers = this.state.extraContainers;
		const index =
			containers.findIndex((c) => c.component.ref.current === container) +
			1;
		if (index > containers.length - 1) {
			this.addContainer([page]);
		} else {
			const target = containers[index];
			target.component.ref.current.addPage(page);
		}
		container.removePage(page);
	};

	//Services...

	reportError = (error) => {
		if (!error) {
			return;
		}
		const description =
			(error.data ? error.data.description : null) || error.toString();
		this.setState({
			lastMessage: { type: "error", text: description },
			transcriptText: this.state.transcriptText + "\r" + description,
			unreadErrorsCount: this.state.unreadErrorsCount + 1,
		});
	};

	warn(text) {
		this.setState({
			lastMessage: { type: "warning", text: text },
		});
	}

	inform(text) {
		this.setState({
			lastMessage: { type: "info", text: text },
		});
	}

	transcriptChanged = (text) => {
		this.state.transcriptText = text;
	};

	reportChange = async (change) => {
		//this triggers unnecessary renders!!!
		// const changes = await this.backend.lastChanges();
		// this.setState({changesCount: changes.length})
	};

	hotkeyPressed = async (hotkey) => {
		switch (hotkey) {
			case "Ctrl+Alt+W":
				this.mainContainer().newWorkspace();
				break;
			case "Ctrl+Alt+Left":
				this.mainContainer().selectPageAtOffset(-1);
				break;
			case "Ctrl+Alt+Right":
				this.mainContainer().selectPageAtOffset(1);
				break;
			default:
		}
	};

	resetUnredErrorCount() {
		this.setState({ unreadErrorsCount: 0 });
	}

	async choose(list) {
		return await this.props.dialog.list(list);
	}

	async confirm(question) {
		return await this.props.dialog.confirm(question);
	}

	async prompt(data) {
		return await this.props.dialog.prompt(data);
	}

	async alert(info) {
		return await this.props.dialog.alert(info);
	}

	waitFor = async (evaluation) => {
		this.setState({ waiting: true });
		const result = await evaluation();
		this.setState({ waiting: false });
		return result;
	};

	searchMethods = async (search, description) => {
		try {
			const methods = await this.waitFor(() => {
				return search();
			});
			if (methods.length === 0) {
				this.inform("There are no " + description);
				return [];
			}
			return methods;
		} catch (error) {
			this.reportError(error);
		}
	};

	render() {
		console.log("rendering IDE");
		const {
			sidebarExpanded,
			unreadErrorsCount,
			unreadMessages,
			transcriptOpen,
			transcriptText,
			lastMessage,
			extraContainers,
			waiting,
		} = this.state;
		const totalWidth = false; //extraContainers.length > 0 ? false : "lg";
		const containerWidth = Math.max(12 / (1 + extraContainers.length), 1);
		return (
			<Hotkeys
				keyName="Ctrl+Alt+W, Ctrl+Alt+Left, Ctrl+Alt+Right"
				filter={(event) => {
					return true;
				}}
				allowRepeat={false}
				onKeyDown={(hotkey, e, handle) => this.hotkeyPressed(hotkey)}
			>
				<DialogProvider>
					<div
						sx={{
							display: "flex",
						}}
					>
						<Titlebar
							developer={this.settings
								.section("connection")
								.get("developer")}
							dialect={this.settings
								.section("connection")
								.get("dialect")}
							sidebarExpanded={sidebarExpanded}
							onSidebarExpand={this.expandSidebar}
							searchOptions={[]}
							onUserClick={this.openSettings}
							colorMode={this.settings
								.section("appearance")
								.get("mode")}
							onColorModeToggle={this.toggleColorMode}
						/>
						<Sidebar
							expanded={sidebarExpanded}
							unreadErrorsCount={unreadErrorsCount}
							unreadMessages={unreadMessages}
							onSaveImageClick={this.saveImage}
							onTranscriptClick={this.openTranscript}
							onSearchClick={this.openSearch}
							onChangesClick={this.browseLastChanges}
							onResourcesClick={this.openResources}
							onPeersClick={this.openChat}
							onSettingsClick={this.openSettings}
							onCollapse={this.collapseSidebar}
						/>
						<Box
							component="main"
							sx={{ flexGrow: 1, ml: 6, mr: 2, p: 3 }}
						>
							<DrawerHeader />
							<Container maxWidth={totalWidth} disableGutters>
								<Grid container spacing={0}>
									<Grid
										item
										xs={containerWidth}
										md={containerWidth}
										lg={containerWidth}
									>
										<ToolContainer
											id={99999}
											key="mainContainer"
											ref={this.mainContainerRef}
											onPageSplit={this.splitPage}
										/>
									</Grid>
									{extraContainers.map((container) => (
										<Grid
											item
											xs={containerWidth}
											md={containerWidth}
											lg={containerWidth}
											key={"container" + container.id}
										>
											{container.component}
										</Grid>
									))}
								</Grid>
							</Container>
						</Box>
						<React.Fragment key="bottom">
							<Drawer
								anchor="bottom"
								variant="persistent"
								open={transcriptOpen}
							>
								<Grid container spacing={0}>
									<Grid item xs={11} md={11} lg={11}>
										{transcriptOpen && (
											<Transcript
												text={transcriptText}
												onChange={
													this.transcriptChanged
												}
											/>
										)}
									</Grid>
									<Grid item xs={1} md={1} lg={1}>
										<Box
											display="flex"
											justifyContent="center"
										>
											<IconButton
												onClick={() =>
													this.setState({
														transcriptOpen: false,
													})
												}
											>
												<KeyboardArrowDown />
											</IconButton>
										</Box>
									</Grid>
								</Grid>
							</Drawer>
						</React.Fragment>
					</div>
					<Backdrop
						//sx={{ zIndex: (theme) => theme.zIndex.drawer + 1 }}
						open={waiting}
					>
						<CircularProgress color="inherit" />
					</Backdrop>
					<CustomSnacks
						open={lastMessage !== null}
						onClose={() => this.setState({ lastMessage: null })}
						text={lastMessage ? lastMessage.text : ""}
						severity={lastMessage ? lastMessage.type : ""}
					/>
				</DialogProvider>
			</Hotkeys>
		);
	}
}

export default withDialog()(withNavigation(withCookies(IDE)));

export { ide };
