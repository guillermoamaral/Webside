import React, { Component } from "react";
import {
	createMuiTheme,
	Container,
	Grid,
	IconButton,
	Drawer,
	Box,
	Fab,
} from "@material-ui/core";
import ToolsContainer from "./ToolsContainer";
import { ThemeProvider } from "@material-ui/styles";
import { withCookies } from "react-cookie";
import { withRouter } from "react-router-dom";
import { withDialog } from "./dialogs/index";
import { amber, blue, green } from "@material-ui/core/colors";
import KeyboardArrowDown from "@material-ui/icons/KeyboardArrowDown";
import API from "./API";
import { DialogProvider } from "./dialogs/index";
import CustomSnacks from "./controls/CustomSnacks";
import Titlebar from "./layout/Titlebar";
import Sidebar from "./layout/Sidebar";
import Transcript from "./tools/Transcript";
import MessageChannel from "./MessageChannel";
import Hotkeys from "react-hot-keys";
import SplitIcon from "@material-ui/icons/VerticalSplit";

var ide = null;

class IDE extends Component {
	constructor(props) {
		super(props);
		ide = this;
		this.settings = { baseUri: "", developer: "", dialect: "" };
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

	initializeSettings = () => {
		const settings = this.queryOptions();
		settings.shortcuts = this.defaultShortcuts();
		this.updateSettings(settings);
	};

	defaultShortcuts() {
		return {
			evaluateExpression: "Ctrl+d",
			inspectEvaluation: "Ctrl+i",
			showEvaluation: "Ctrl+p",
			debugExpression: "Ctrl+u",
			acceptCode: "Ctrl+s",
			browseClass: "Ctrl+b",
			browseSenders: "Alt+n",
			browseImplementors: "Alt+m",
			browseClassReferences: "Alt+r",
		};
	}

	initializeAPI() {
		this.api = new API(
			this.settings.baseUri,
			this.settings.developer,
			this.reportError,
			this.reportChange
		);
	}

	initializeMessageChannel() {
		const url = this.settings.messageChannelUrl;
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

	applySettings(settings) {
		this.mainContainer().removeAllPages();
		this.props.history.push(
			"/ide?baseUri=" +
				settings.baseUri +
				"&dialect=" +
				settings.dialect +
				"&developer=" +
				settings.developer
		);
		this.updateSettings(settings);
		this.removeExtraContainers();
	}

	async updateSettings(settings) {
		this.settings = settings;
		document.title = settings.dialect;
		this.initializeAPI();
		this.updateTheme();
		this.initializeMessageChannel();
	}

	welcomeMessage() {
		const backend =
			this.settings.dialect !== "undefined"
				? this.settings.dialect
				: "It looks like the Smalltalk system could not be determined";
		return (
			'"Welcome to Webside ' +
			this.settings.developer +
			"!\rA Smalltalk IDE for the web.\r\r" +
			"Backend: " +
			backend +
			"\r" +
			"URL: " +
			this.settings.baseUri +
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
		if (containers.length == 0) {
			return 0;
		}
		const sorted = containers.map((p) => p.id).sort();
		const maxId = sorted[sorted.length - 1];
		if (1 == 1) {
			("We don't recycle ids for the moment");
			return maxId + 1;
		}
		const used = new Array(maxId);
		sorted.forEach((id) => (used[id] = true));
		const unused = used.findIndex((id) => id !== true);
		return unused == -1 ? maxId + 1 : unused;
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
						if (c.pages().length == 0) {
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

	updateTheme() {
		var mainPrimaryColor;
		var mainSecondaryColor;
		switch (this.settings.dialect) {
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
					passed > 0 && failed == 0 && errors == 0
						? "success"
						: failed > 0 && errors == 0
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
				<ThemeProvider theme={this.theme}>
					<DialogProvider>
						<div className={styles.root}>
							<Titlebar
								developer={this.settings.developer}
								dialect={this.settings.dialect}
								styles={styles}
								sidebarExpanded={sidebarExpanded}
								expandSidebar={this.expandSidebar}
								searchOptions={[]}
								onAvatarClicked={this.openSettings}
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
								onClose={this.collapseSidebar}
							/>

							<main className={styles.content}>
								<div className={styles.appBarSpacer} />
								<Container
									className={styles.container}
									maxWidth={totalWidth}
									disableGutters
								>
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
														ref={
															this
																.mainContainerRef
														}
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
															{
																container.component
															}
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
							</main>
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
						<CustomSnacks
							open={lastMessage !== null}
							onClose={() => this.setState({ lastMessage: null })}
							text={lastMessage ? lastMessage.text : ""}
							severity={lastMessage ? lastMessage.type : ""}
						/>
					</DialogProvider>
				</ThemeProvider>
			</Hotkeys>
		);
	}
}

export default withDialog()(withRouter(withCookies(IDE)));

export { ide };
