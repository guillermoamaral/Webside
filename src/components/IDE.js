import React, { Component } from "react";
import {
	Container,
	Grid,
	IconButton,
	Drawer,
	Box,
	Fab,
	Backdrop,
	CircularProgress,
} from "@mui/material";
import ToolsContainer from "./ToolsContainer";
import { withCookies } from "react-cookie";
import { withNavigation } from "./withNavigation";
import { withDialog } from "./dialogs/index";
import { amber, blue, green } from "@mui/material/colors";
import KeyboardArrowDown from "@mui/icons-material/KeyboardArrowDown";
import API from "./API";
import { DialogProvider } from "./dialogs/index";
import CustomSnacks from "./controls/CustomSnacks";
import Titlebar from "./layout/Titlebar";
import Sidebar from "./layout/Sidebar";
import Transcript from "./tools/Transcript";
import MessageChannel from "./MessageChannel";
import Hotkeys from "react-hot-keys";
import SplitIcon from "@mui/icons-material/VerticalSplit";
import DrawerHeader from "./layout/DrawerHeader";
import { Setting, Settings } from "../model/Settings";
import { app as mainApp } from "../App";

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
		const settings = this.defaultSettings();
		const options = this.queryOptions();
		const connection = settings.section("connection");
		connection.set("baseUri", options.baseUri);
		connection.set("developer", options.developer);
		connection.set("dialect", options.dialect);
		this.updateSettings(settings);
		new Settings().fromJson(settings.toJson());
	};

	defaultSettings() {
		const settings = new Settings("settings");
		const connection = new Settings("connection");
		connection.add(Setting.url("baseUri"));
		connection.add(Setting.text("developer"));
		const dialect = Setting.text("dialect");
		dialect.readOnly = true;
		connection.add(dialect);
		settings.add(connection);

		const appearance = new Settings("appearance");
		appearance.add(Setting.options("mode", ["dark", "light"]));
		settings.add(appearance);

		const light = new Settings("light");
		light.add(Setting.color("primaryColor", "#cccccc"));
		light.add(Setting.color("secondaryColor", "#cccccc"));
		light.add(Setting.color("background", "#ffffff"));
		light.add(Setting.color("primaryText", "#000000"));
		light.add(Setting.color("secondaryText", "#808080"));
		appearance.add(light);

		const dark = new Settings("dark");
		dark.add(Setting.color("primaryColor", "#ffffff"));
		dark.add(Setting.color("secondaryColor", "#cccccc"));
		dark.add(Setting.color("background", "#303030"));
		dark.add(Setting.color("primaryText", "#aaaaaa"));
		dark.add(Setting.color("secondaryText", "#00000"));
		appearance.add(dark);

		const shortcuts = new Settings("shortcuts");
		shortcuts.add(
			Setting.text("evaluateExpression", "Ctrl+d", "Evaluate expression")
		);
		shortcuts.add(
			Setting.text("inspectEvaluation", "Ctrl+i", "Inspect evaluation")
		);
		shortcuts.add(
			Setting.text("showEvaluation", "Ctrl+p", "Show evaluation")
		);
		shortcuts.add(
			Setting.text("debugExpression", "Ctrl+u", "Debug expression")
		);
		shortcuts.add(Setting.text("acceptCode", "Ctrl+s", "Accept code"));
		shortcuts.add(Setting.text("browseClass", "Ctrl+b", "Browse class"));
		shortcuts.add(Setting.text("browseSenders", "Alt+n", "Browse senders"));
		shortcuts.add(
			Setting.text("browseImplementors", "Alt+m", "Browse implementors")
		);
		shortcuts.add(
			Setting.text(
				"browseClassReferences",
				"Alt+r",
				"Browse class references"
			)
		);
		shortcuts.readOnly();
		settings.add(shortcuts);

		return settings;
	}

	applySettings(settings) {
		this.mainContainer().removeAllPages();
		const connection = this.settings.section("connection");
		this.props.navigate(
			"/ide?baseUri=" +
				connection.get("baseUri") +
				"&dialect=" +
				connection.get("dialect") +
				"&developer=" +
				connection.get("developer")
		);
		this.updateSettings(settings);
		this.removeExtraContainers();
	}

	toggleColorMode = () => {
		const appearance = this.settings.section("appearance");
		appearance.set(
			"mode",
			appearance.get("mode") === "dark" ? "light" : "dark"
		);
		this.updateTheme();
		this.forceUpdate();
	};

	async updateSettings(settings) {
		this.settings = settings;
		this.initializeAPI();
		const dialect = await this.api.dialect();
		document.title = dialect;
		settings.section("connection").set("dialect", dialect);
		this.updateDialectColors();
		this.updateTheme();
		this.initializeMessageChannel();
	}

	updateDialectColors() {
		var primary;
		var secondary;
		const dialect = this.settings.section("connection").get("dialect");
		switch (dialect) {
			case "Bee":
				primary = amber[300];
				secondary = amber[800];
				break;
			case "Pharo":
				primary = blue[300];
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
		const dark = appearance.section("dark");
		dark.set("primaryColor", primary);
		dark.set("secondaryColor", secondary);
		const light = appearance.section("light");
		light.set("primaryColor", primary);
		light.set("secondaryColor", secondary);
	}

	updateTheme() {
		mainApp.updateTheme(this.settings);
	}

	initializeAPI() {
		const connection = this.settings.section("connection");
		this.api = new API(
			connection.get("baseUri"),
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
			connection.dialect !== "undefined"
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
			connection.get("baseUri") +
			'"'
		);
	}

	transcriptText() {
		return this.state.transcriptText;
	}

	mainContainer() {
		return this.mainContainerRef.current;
	}

	newContainerId() {
		const containers = this.state.extraContainers;
		if (containers.length === 0) {
			return 0;
		}
		const sorted = containers.map((p) => p.id).sort();
		const maxId = sorted[sorted.length - 1];
		if (1 == 1) {
			//We don't recycle ids for the moment"
			return maxId + 1;
		}
		const used = new Array(maxId);
		sorted.forEach((id) => (used[id] = true));
		const unused = used.findIndex((id) => id !== true);
		return unused === -1 ? maxId + 1 : unused;
	}

	addContainer = () => {
		const containers = this.state.extraContainers;
		const id = this.newContainerId();
		const container = {
			id: id,
			component: (
				<ToolsContainer
					id={id}
					styles={this.props.styles}
					onPageRemove={(c) => {
						if (c.pages().length === 0) {
							this.removeContainer(c);
						}
					}}
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
			const status = await this.api.testRunStatus(id);
			if (status.running) {
				setTimeout(async () => {
					await this.followTestRun(id);
				}, 1000);
			}
			if (!status.running) {
				const results = await this.api.testRunResults(id);
				const passed = results.passed.length;
				const failed = results.failed.length;
				const errors = results.errors.length;
				if (debug && (failed > 0 || errors > 0)) {
					const test =
						failed > 0 ? results.failed[0] : results.errors[0];
					const d = await this.api.debugTest(
						id,
						test.class,
						test.selector
					);
					this.openDebugger(
						d.id,
						d.description || "Debugging test " + test.selector
					);
				} else {
					await this.api.deleteTestRun(id);
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
		// const changes = await this.api.lastChanges();
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

	waitFor = async (something) => {
		this.setState({ waiting: true });
		const result = await something();
		this.setState({ waiting: false });
		return result;
	};

	render() {
		console.log("rendering IDE");
		const styles = this.props.styles;
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
		const totalWidth = extraContainers.length > 0 ? false : "lg";
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
							styles={styles}
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
							styles={styles}
							expanded={sidebarExpanded}
							unreadErrorsCount={unreadErrorsCount}
							unreadMessages={unreadMessages}
							onSaveImageClicked={this.saveImage}
							onTranscriptClicked={this.openTranscript}
							onSearchClicked={this.openSearch}
							onChangesClicked={this.browseLastChanges}
							onResourcesClicked={this.openResources}
							onPeersClicked={this.openChat}
							onCollapse={this.collapseSidebar}
						/>
						<Box component="main" sx={{ flexGrow: 1, p: 3 }}>
							<DrawerHeader />
							<Container maxWidth={totalWidth} disableGutters>
								<Box display="flex" flexDirection="row">
									<Box flexGrow={1}>
										<Grid container spacing={0}>
											<Grid
												item
												xs={containerWidth}
												md={containerWidth}
												lg={containerWidth}
											>
												<ToolsContainer
													key="mainContainer"
													ref={this.mainContainerRef}
													styles={styles}
												/>
											</Grid>
											{extraContainers.map(
												(container) => (
													<Grid
														item
														xs={containerWidth}
														md={containerWidth}
														lg={containerWidth}
														key={
															"container" +
															container.id
														}
													>
														{container.component}
													</Grid>
												)
											)}
										</Grid>
									</Box>
									<Box>
										<Fab
											color="primary"
											variant="round"
											onClick={this.addContainer}
											size="medium"
										>
											<SplitIcon />
										</Fab>
									</Box>
								</Box>
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
												styles={styles}
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
