import React, { Component } from "react";
import {
	Box,
	Backdrop,
	CircularProgress,
	Dialog,
	DialogTitle,
	DialogContent,
	Popper,
	Card,
	CardContent,
	CardHeader,
	IconButton,
	Fab,
	Typography,
} from "@mui/material";
import CodeAssistantChat from "./tools/CodeAssistantChat";
import MinimizeIcon from "@mui/icons-material/Minimize";
import MaximizeIcon from "@mui/icons-material/Maximize";
import ToolContainer from "./ToolContainer";
import { withNavigation } from "./withNavigation";
import { withDialog } from "./dialogs/index";
import Backend from "./Backend";
import { DialogProvider } from "./dialogs/index";
import CustomSnacks from "./controls/CustomSnacks";
import Titlebar from "./layout/Titlebar";
import Sidebar from "./layout/Sidebar";
import MessageChannel from "./MessageChannel";
import Hotkeys from "react-hot-keys";
//import DrawerHeader from "./layout/DrawerHeader";
import { Settings, Setting } from "../model/Settings";
import { app as mainApp } from "../App";
import { v4 as uuidv4 } from "uuid";
import CustomSplit from "./controls/CustomSplit";
import QuickSearch from "./tools/QuickSearch";
import AssistantIcon from "@mui/icons-material/Assistant";
import { AIInterface } from "../model/ai/AIInterface";
import AICodeAssistant from "../model/ai/AICodeAssistant";

var ide = null;
var MaxExtraContainers = 3;

class IDE extends Component {
	constructor(props) {
		super(props);
		ide = this;
		this.backend = props.backend;
		this.initializeThemes();
		this.initializeSettings();
		this.mainContainerRef = React.createRef();
		this.colorModeChangeHandlers = [];
		this.state = {
			sidebarExpanded: false,
			lastMessage: null,
			unreadErrorsCount: 0,
			unreadMessages: 0,
			transcriptText: this.welcomeMessage(),
			extraContainers: [],
			waiting: false,
			quickSearchOpen: false,
			quickSearchOptions: {},
			activeContainer: null,
			assistantChatOpened: false,
			assistantChatMaximized: true,
		};
		this.codeAssistant = new AICodeAssistant(this.backend);
		this.assistantChatRef = React.createRef();
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
		this.loadSettings();
		this.setConnectionSettings();
		this.updateSettings();
	};

	setConnectionSettings() {
		let url, developer;
		if (this.backend) {
			// Backend provided externally
			url = this.backend.url;
			developer = this.backend.author;
		} else {
			const options = this.queryOptions();
			url = options.backend;
			developer = options.developer;
		}
		const connection = this.settings.section("connection");
		connection.set("backend", url);
		connection.set("developer", developer);
	}

	initializeThemes() {
		this.themes = [this.defaultTheme()];
	}

	newTheme() {
		const settings = new Settings("theme");
		const dark = settings.addSection("dark", "Dark Colors");
		dark.addColor("background", "#ffffff");
		const light = settings.addSection("light", "Light Colors");
		light.addColor("background", "#000000");
		[
			[
				"primaryColor",
				"Main distinctive color of determined by target system",
			],
			[
				"secondaryColor",
				"Secondary distinctive color determined by the target system",
			],
			["primaryText", "Color used for labels"],
			["secondaryText", ""],
			["disabledText", ""],
			["appliedChange", ""],
			["unappliedChange", ""],
			["separatorColor", ""],
			["selectionColor", ""],
			["systemBrowserColor", "System browser background color", false],
			["classBrowserColor", "Class browser background color", false],
			["debuggerColor", "Debugger background color", false],
			["inspectorColor", "Inspector background color", false],
			["workspaceColor", "Workspace background color", false],
			["transcriptColor", "Transcript background color", false],
			["methodBrowserColor", "Method browser background color", false],
			["changesBrowserColor", "Changes browser background color", false],
			["historyBrowserColor", "History browser background color", false],
		].forEach((a) => {
			dark.addColor(a[0], "#000000", a[1]).active = a[2] !== false;
			light.addColor(a[0], "#ffffff", a[1]).active = a[2] !== false;
		});
		[
			"selectorStyle",
			"symbolStyle",
			"argumentStyle",
			"temporaryStyle",
			"assignmentStyle",
			"stringStyle",
			"variableStyle",
			"metaStyle",
			"bracketStyle",
			"selfStyle",
			"superStyle",
			"trueStyle",
			"falseStyle",
			"nilStyle",
			"thisContextStyle",
			"returnStyle",
			"globalStyle",
			"numberStyle",
			"commentStyle",
		].forEach((s) => {
			dark.addTextStyle(s, "#000000");
			light.addTextStyle(s, "#ffffff");
		});
		dark.setting("primaryColor").readOnly();
		dark.setting("secondaryColor").readOnly();
		light.setting("primaryColor").readOnly();
		light.setting("secondaryColor").readOnly();
		return settings;
	}

	defaultTheme() {
		const theme = this.newTheme();
		theme.name = "Default";
		const light = theme.section("light");
		light.set("primaryColor", "#cccccc");
		light.set("secondaryColor", "#cccccc");
		light.set("background", "#ffffff");
		light.set("systemBrowserColor", "#ffffff");
		light.set("classBrowserColor", "#ffffff");
		light.set("debuggerColor", "#ffffff");
		light.set("inspectorColor", "#ffffff");
		light.set("workspaceColor", "#ffffff");
		light.set("transcriptColor", "#ffffff");
		light.set("methodBrowserColor", "#ffffff");
		light.set("changesBrowserColor", "#ffffff");
		light.set("historyBrowserColor", "#ffffff");
		light.set("primaryText", "#000000");
		light.set("secondaryText", "#808080");
		light.set("disabledText", "#00000080");
		light.set("appliedChange", "green");
		light.set("unappliedChange", "#969696");
		light.set("separatorColor", "#b3bab6");
		light.set("selectionColor", "#9bcaef64");
		light.setting("selectorStyle").color = "#000000";
		light.setting("symbolStyle").color = "#2aa9b2";
		light.setting("argumentStyle").color = "#f06520";
		light.setting("temporaryStyle").color = "#28739f";
		light.setting("assignmentStyle").color = "#000000";
		light.setting("stringStyle").color = "#00b32d";
		light.setting("variableStyle").color = "#4fc1ff";
		light.setting("metaStyle").color = "#ffcb6b";
		light.setting("bracketStyle").color = "#9b9b9b";
		light.setting("selfStyle").color = "#c792ea";
		light.setting("superStyle").color = "#c792ea";
		light.setting("trueStyle").color = "#c792ea";
		light.setting("falseStyle").color = "#c792ea";
		light.setting("nilStyle").color = "#c792ea";
		light.setting("thisContextStyle").color = "#c792ea";
		light.setting("returnStyle").color = "#72bb19";
		light.setting("globalStyle").color = "#a22598";
		light.setting("numberStyle").color = "#65a14e";
		light.setting("commentStyle").color = "#586e75";
		const dark = theme.section("dark");
		dark.set("primaryColor", "#ffffff");
		dark.set("secondaryColor", "#cccccc");
		dark.set("background", "#1f1f1f");
		dark.set("systemBrowserColor", "#1f1f1f");
		dark.set("classBrowserColor", "#1f1f1f");
		dark.set("debuggerColor", "#1f1f1f");
		dark.set("inspectorColor", "#1f1f1f");
		dark.set("workspaceColor", "#1f1f1f");
		dark.set("transcriptColor", "#1f1f1f");
		dark.set("methodBrowserColor", "#1f1f1f");
		dark.set("changesBrowserColor", "#1f1f1f");
		dark.set("historyBrowserColor", "#1f1f1f");
		dark.set("primaryText", "#aaaaaa");
		dark.set("secondaryText", "#808080");
		dark.set("disabledText", "#aaaaaa80");
		dark.set("appliedChange", "#c0ff61");
		dark.set("unappliedChange", "#c8c8c8");
		dark.set("separatorColor", "#b3bab6");
		dark.set("selectionColor", "#9bcaef50");
		dark.setting("selectorStyle").color = "#d3dddd";
		dark.setting("symbolStyle").color = "#3cd2dd";
		dark.setting("argumentStyle").color = "#f06520";
		dark.setting("temporaryStyle").color = "#81c9f3";
		dark.setting("assignmentStyle").color = "#ffffff";
		dark.setting("stringStyle").color = "#c3e88d";
		dark.setting("variableStyle").color = "#4fc1ff";
		dark.setting("metaStyle").color = "#ffcb6b";
		dark.setting("bracketStyle").color = "#9b9b9b";
		dark.setting("selfStyle").color = "#c792ea";
		dark.setting("superStyle").color = "#c792ea";
		dark.setting("trueStyle").color = "#c792ea";
		dark.setting("falseStyle").color = "#c792ea";
		dark.setting("nilStyle").color = "#c792ea";
		dark.setting("thisContextStyle").color = "#c792ea";
		dark.setting("returnStyle").color = "#72bb19";
		dark.setting("globalStyle").color = "#bb73b5";
		dark.setting("numberStyle").color = "#65a14e";
		dark.setting("commentStyle").color = "#586e75";
		return theme;
	}

	defaultSettings() {
		const settings = new Settings("settings");
		// General...
		const general = settings.addSection("general");
		general.addImage("photo").description = "Your photo";
		// Connection...
		const connection = settings.addSection("connection");
		connection.addUrl("backend").readOnly();
		connection.addText("developer");
		connection.addText("dialect").readOnly();
		// Code...
		const code = settings.addSection("editor");
		code.addBoolean("lineNumbers", false, "Show line numbers");
		code.addBoolean("autocompletion", false, "Use autocompletion");
		code.addBoolean("tooltips", true, "Show tooltips");
		// Appearance...
		const appearance = settings.addSection("appearance");
		appearance.addOptions(
			"fontFamily",
			[
				"Arial",
				"Arial black",
				"Consolas",
				"Courier",
				"Monospace",
				"Roboto",
				"Tahoma",
			],
			"Roboto"
		);
		appearance.addOptions(
			"fontSize",
			[8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24],
			14
		);
		const theme = this.themes[0].copy();
		appearance.addOptions(
			"theme",
			this.themes.map((t) => t.name),
			theme.name
		);
		appearance.addOptions("mode", ["dark", "light"], theme.mode);
		theme.settings.forEach((s) => appearance.add(s));
		// Shortcuts...
		const shortcuts = settings.addSection("shortcuts");
		shortcuts.addShortcut("quickSearch", "Shift+Enter");
		shortcuts.addShortcut("openClassBrowser", "Ctrl+b");
		shortcuts.addShortcut("newWorkspace", "Ctrl+Alt+w");
		shortcuts.addShortcut("moveToLeftTab", "Ctrl+Alt+Left");
		shortcuts.addShortcut("moveToRightTab", "Ctrl+Alt+Right");
		shortcuts.addShortcut("evaluateExpression", "Ctrl+d");
		shortcuts.addShortcut("inspectEvaluation", "Ctrl+i");
		shortcuts.addShortcut("showEvaluation", "Ctrl+p");
		shortcuts.addShortcut("debugExpression", "Ctrl+u");
		shortcuts.addShortcut("acceptCode", "Ctrl+s");
		shortcuts.addShortcut("browseClass", "Ctrl+b");
		shortcuts.addShortcut("browseSenders", "Alt+n");
		shortcuts.addShortcut("browseImplementors", "Alt+m");
		shortcuts.addShortcut("browseClassReferences", "Alt+r");
		// Code assistant...
		const assistant = settings.addSection("codeAssistant");
		assistant.addBoolean("enabled", false, "Use code assistant");
		const types = AIInterface.availableTypes();
		if (types.length > 0) {
			assistant.addOptions(
				"interface",
				types.map((t) => t.displayName()),
				types[0].displayName()
			);
		}
		assistant.addText("apiKey");
		if (types.length > 0) {
			const model = assistant.addOptions(
				"model",
				[],
				AIInterface.newNamed(types[0].displayName()).defaultModel()
			);
			model.refreshHandler = async () => {
				const aiInterface = AIInterface.newNamed(
					assistant.get("interface")
				);
				aiInterface.key = assistant.get("apiKey") || "";
				const models = await aiInterface.getModels();
				return models.map((m) => m.id).sort();
			};
		}
		assistant.addNumber("temperature", 0);
		assistant.addParagraph(
			"systemMessage",
			"You are an expert Smalltalk programmer.\nWhen I ask for help to analyze, explain or write Smalltalk code, you will reply accordingly.\nIn your response you will avoid using the words 'Smalltalk' and 'snippet'."
		);
		// Advanced...
		const advanced = settings.addSection("advanced");
		advanced.addNumber(
			"evaluationPollingFrequency",
			300,
			"Evaluation polling frequency (ms)",
			"Warning. This is used to poll the backend for evaluation progress. A high frequency might affect frontend performance. On the other hand, a low frequency could introduce a huge delay to detecting evaluation finalization."
		);
		return settings;
	}

	colorSetting(name) {
		const appearance = this.settings.section("appearance");
		const mode = appearance.get("mode");
		const setting = appearance.section(mode).setting(name);
		return setting.active ? setting.value : "inherit";
	}

	applyTheme(name) {
		const theme = this.themes.find((t) => t.name === name);
		if (!theme) return;
		const appearance = this.settings.section("appearance");
		appearance.copyFrom(theme);
	}

	applySettings(settings) {
		this.settings = settings;
		this.applyTheme(settings.section("appearance").get("theme"));
		this.storeSettings();
		this.updateSettings();
	}

	disconnect = () => {
		this.props.navigate("/");
	};

	resetSettingsSection(name) {
		const section = this.defaultSettings().section(name);
		this.settings.setSection(name, section);
		this.applySettings(this.settings);
	}

	toggleColorMode = () => {
		const appearance = this.settings.section("appearance");
		const mode = appearance.get("mode") === "dark" ? "light" : "dark";
		appearance.set("mode", mode);
		this.storeSettings();
		this.updateAppTheme();
		this.forceUpdate();
		this.colorModeChanged();
	};

	onColorModeChange = (handler) => {
		this.colorModeChangeHandlers.push(handler);
	};

	removeColorModeChangeHandler = (handler) => {
		const index = this.colorModeChangeHandlers.indexOf(handler);
		this.colorModeChangeHandlers.splice(index, 1);
	};

	settingsStoreName() {
		const options = this.queryOptions();
		return (
			"webside-settings-" +
			options.backend +
			"-developer-" +
			options.developer
		);
	}

	loadSettings() {
		const data = localStorage.getItem(this.settingsStoreName());
		if (data) {
			this.settings.fromJson(JSON.parse(data));
		}
	}

	storeSettings() {
		const data = JSON.stringify(this.settings.toJson());
		localStorage.setItem(this.settingsStoreName(), data);
	}

	async updateSettings() {
		this.initializeBackend();
		let dialect;
		try {
			dialect = await this.backend.dialect();
		} catch (error) {
			this.reportError(error);
		}
		try {
			this.logo = await this.backend.logo();
		} catch (error) {}
		document.title = dialect;
		this.settings.section("connection").set("dialect", dialect);
		let autocompletion = false;
		try {
			await ide.backend.autocompletions("Object", "m\r Objec", 8);
			autocompletion = true;
		} catch (ignored) {}
		this.settings.section("editor").set("autocompletion", autocompletion);
		await this.fetchThemes();
		await this.updateDialectColorSettings();
		this.updateAppTheme();
		this.initializeMessageChannel();
		this.initializeCodeAssistant();
	}

	async fetchThemes() {
		try {
			const json = await ide.backend.themes();
			json.forEach((j) => {
				if (!this.themes.find((t) => t.name === j.name)) {
					const theme = this.defaultTheme();
					theme.name = j.name;
					theme.fromJson(j);
					this.themes.push(theme);
				}
			});
			this.settings
				.section("appearance")
				.setting("theme")
				.setOptions(this.themes.map((t) => t.name));
		} catch (ignored) {
			console.log(ignored);
		}
	}

	async updateDialectColorSettings() {
		let primary;
		let secondary;
		try {
			let colors = await this.backend.colors();
			primary = colors.primary;
			secondary = colors.secondary;
		} catch (error) {
			primary = "#000000";
			secondary = "#000000";
		}
		const appearance = this.settings.section("appearance");
		const dark = appearance.section("dark");
		dark.set("primaryColor", primary);
		dark.set("secondaryColor", secondary);
		dark.set("appliedChange", primary);
		const light = appearance.section("light");
		light.set("primaryColor", Setting.adjustColor(primary, -40));
		light.set("secondaryColor", secondary);
		light.set("appliedChange", Setting.adjustColor(primary, -60));
		this.colorModeChanged();
	}

	colorModeChanged() {
		this.colorModeChangeHandlers.forEach((h) => h());
	}

	updateAppTheme() {
		mainApp.updateTheme(this.settings);
	}

	usesCodeAssistant() {
		return this.codeAssistant.active;
	}

	initializeCodeAssistant() {
		const section = this.settings.section("codeAssistant");
		const enabled = section.get("enabled");
		this.codeAssistant.active = enabled;
		if (!enabled) return;
		const aiInterface = AIInterface.newNamed(section.get("interface"));
		aiInterface.key = section.get("apiKey") || "";
		aiInterface.temperature = section.get("temperature");
		this.codeAssistant.useInterface(aiInterface);
	}

	initializeBackend() {
		const connection = this.settings.section("connection");
		if (!this.backend) {
			this.backend = new Backend(
				connection.get("backend"),
				connection.get("developer"),
				this.reportError,
				this.reportChange
			);
		}
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
		return (
			'"Welcome to Webside ' +
			this.currentDeveloper() +
			"!\rA Smalltalk IDE for the web.\r\r" +
			"Backend: " +
			this.currentDialect() +
			"\r" +
			"URL: " +
			this.currentBackend() +
			'"'
		);
	}

	currentDeveloper() {
		const connection = this.settings.section("connection");
		return connection.get("developer");
	}

	currentDialect() {
		const connection = this.settings.section("connection");
		return connection.get("dialect");
	}

	currentBackend() {
		const connection = this.settings.section("connection");
		return connection.get("backend");
	}

	transcriptText() {
		return this.state.transcriptText;
	}

	mainContainer() {
		return this.mainContainerRef.current;
	}

	// Containers...

	newContainerId() {
		return uuidv4();
	}

	addContainer = (pages, index) => {
		const containers = this.state.extraContainers;
		const id = this.newContainerId();
		const ref = React.createRef();
		const container = {
			id: id,
			component: (
				<ToolContainer
					id={id}
					key={id}
					ref={ref}
					onPageFocus={this.pageFocusedInContainer}
					onPagesRemove={this.pagesRemovedInContainer}
					//onSplit={this.splitContainer} //disabled for the moment
					pages={pages}
					showClose={true}
					sx={{ width: "100%", height: "100%" }}
				/>
			),
		};
		let i = index;
		if (i === undefined) i = containers.length;
		containers.splice(i, 0, container);
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

	splitContainer = (container) => {
		const containers = this.state.extraContainers;
		if (containers.length < MaxExtraContainers) {
			const index = container
				? containers.findIndex(
						(c) => c.component.ref.current === container
				  )
				: containers.length - 1;
			this.addContainer([], index + 1);
		}
	};

	pagesRemovedInContainer = (container) => {
		if (container.pages().length === 0) {
			this.removeContainer(container);
		}
	};

	pageFocusedInContainer = (container, page) => {
		if (container === this.state.activeContainer) return;
		this.setState({ activeContainer: container });
	};

	// ...

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
		//this.mainContainer().openSearch();
		this.openQuickSearch();
	};

	openQuickSearch = () => {
		this.setState({ quickSearchOpen: true });
	};

	closeQuickSearch = () => {
		this.setState({ quickSearchOpen: false });
	};

	browseLastChanges = () => {
		this.mainContainer().browseLastChanges();
	};

	browseChanges = (changes) => {
		this.mainContainer().browseChanges(changes);
	};

	browsePackage = (packname) => {
		this.mainContainer().browsePackage(packname);
	};

	browseClass = (classname) => {
		this.mainContainer().browseClass(classname);
	};

	browseImplementors = (selector) => {
		this.mainContainer().browseImplementors(selector);
	};

	openResources = () => {
		this.mainContainer().openResources();
	};

	openChat = () => {
		this.mainContainer().openChat();
	};

	async inspectExpression(expression) {
		const object = await this.backend.evaluateExpression(
			expression,
			true,
			true
		);
		if (object) {
			this.mainContainer().openInspector(object);
		}
	}

	async followTestRun(id, debug, onRun) {
		try {
			const status = await this.backend.testRunStatus(id);
			if (status.running) {
				setTimeout(async () => {
					await this.followTestRun(id, debug, onRun);
				}, 1000);
			}
			if (!status.running) {
				if (onRun) onRun();
				const results = await this.backend.testRunResults(id);
				const { failed, errors } = results;
				var d;
				if (debug && (failed.length > 0 || errors.length > 0)) {
					const test = failed.length > 0 ? failed[0] : errors[0];
					d = await this.backend.debugTest(
						id,
						test.class,
						test.selector
					);
					this.mainContainer().openDebugger(
						d.id,
						d.description || "Debugging test " + test.selector,
						() => this.backend.deleteTestRun(id),
						() => this.backend.deleteTestRun(id)
					);
					return;
				} else {
					await this.backend.deleteTestRun(id);
				}
				var message = this.testRunResultMessage(results);
				// if (d) {
				// 	message.action = {
				// 		label: "Debug",
				// 		handler: () =>
				// 			this.mainContainer().openDebugger(
				// 				d.id,
				// 				d.description ||
				// 					"Debugging test " + test.selector
				// 			),
				// 	};
				// }
				this.setState({ lastMessage: message });
			}
		} catch (error) {
			this.reportError(error);
		}
	}

	testRunResultMessage(results) {
		var text = "";
		var first = true;
		["passed", "failed", "errors", "skipped", "knownIssues"].forEach(
			(type) => {
				if (results[type]) {
					text +=
						(first ? "" : ", ") + results[type].length + " " + type;
					first = false;
				}
			}
		);
		var { passed, failed, errors } = results;
		var type =
			passed.length > 0 && failed.length === 0 && errors.length === 0
				? "success"
				: failed.length > 0 && errors.length === 0
				? "warning"
				: errors.length > 0
				? "error"
				: "info";
		return { text: text, type: type };
	}

	expandSidebar = () => {
		this.setState({ sidebarExpanded: true });
	};

	collapseSidebar = () => {
		this.setState({ sidebarExpanded: false });
	};

	//Services...

	reportError = (error) => {
		if (!error) return;
		let description = error.toString();
		let message = error.data
			? typeof error.data === "string"
				? error.data
				: error.data.description
			: "";
		if (message.length > 0)
			description = description + "\rServer message: " + message;
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
		const shortcuts = this.settings.section("shortcuts");
		switch (hotkey) {
			case shortcuts.get("quickSearch"):
				this.openQuickSearch();
				break;
			case shortcuts.get("openClassBrowser"):
				this.mainContainer().openClassBrowser();
				break;
			case shortcuts.get("newWorkspace"):
				this.mainContainer().newWorkspace();
				break;
			case shortcuts.get("moveToLeftTab"):
				this.mainContainer().selectPageAtOffset(-1);
				break;
			case shortcuts.get("moveToRightTab"):
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

	async fillForm(info) {
		return await this.props.dialog.form(info);
	}

	waitFor = async (evaluation) => {
		this.setState({ waiting: true });
		let result;
		try {
			result = await evaluation();
		} catch (error) {
			this.reportError(error);
		}
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

	extensionMenuOptions(definitions, handler) {
		if (!definitions || definitions.length === 0) return [];
		let options = [null];
		if (!definitions || definitions.length === 0) return options;
		let grouped = {};
		let ungrouped = [];
		definitions.forEach((d) => {
			let s = d.section;
			if (s) {
				if (!grouped[s]) grouped[s] = [];
				grouped[s].push(d);
			} else {
				ungrouped.push(d);
			}
		});
		Object.entries(grouped).forEach((e) => {
			grouped[e[0]] = e[1].sort((a, b) => (a.label <= b.label ? -1 : 1));
		});
		ungrouped = ungrouped.sort((a, b) => (a.label <= b.label ? -1 : 1));
		ungrouped.forEach((d) => {
			options.push({
				label: d.label,
				action: (e) => handler(d, e),
			});
		});
		for (const [s, v] of Object.entries(grouped)) {
			options.push({
				label: s,
				suboptions: v.map((d) => {
					return {
						label: d.label,
						action: (e) => handler(d, e),
					};
				}),
			});
		}
		return options;
	}

	performExtendedOption = async (specification, element) => {
		if (specification.extensionType === "change") {
			await this.performExtendedChange(specification, element);
		}
		if (specification.extensionType === "export") {
			await this.performExport(specification, element);
		}
		if (specification.extensionType === "search") {
			await this.performExtendedSearch(specification, element);
		}
	};

	performExport = async (specification, element) => {
		const get = this.resolveDotExpressions(
			specification.get,
			"element",
			element
		);
		let data = get;
		if (get.startsWith("/")) {
			data = await this.backend.get(get);
		}
		if (!data) return;
		const blob = new Blob([data], {
			type: "text/plain",
		});
		let filename = this.resolveDotExpressions(
			specification.defaultFilename,
			"element",
			element
		);
		this.download(blob, filename || "file");
	};

	performExtendedChange = async (specification, element) => {
		const description = specification.label.toLowerCase();
		if (specification.needsConfirmation) {
			const proceed = await this.confirm(
				"Do you want to " + description + "?"
			);
			if (!proceed) return;
		}
		let parameters;
		if (specification.parameters && specification.parameters.length > 0) {
			parameters = await this.promptChangeParameters(
				specification,
				element
			);
			if (!parameters) return;
		}
		let change = {};
		for (const [k, v] of Object.entries(specification.properties)) {
			let value = v;
			if (typeof value === "string") {
				value = this.resolveDotExpressions(value, "element", element);
				if (parameters) {
					value = this.resolveDotExpressions(
						value,
						"parameters",
						parameters
					);
				}
			}
			change[k] = value;
		}
		let result;
		try {
			result = await this.backend.postChange(change, description);
		} catch (error) {
			this.handleChangeError(error);
		}
		return result;
	};

	performExtendedSearch = async (specification, element) => {
		const uri = this.resolveDotExpressions(
			specification.get,
			"element",
			element
		);
		const result = await this.searchMethods(() => {
			return this.backend.get(uri);
		}, specification.label);
		if (!result)
			return this.inform(
				"No results for " + specification.label.toLowerCase()
			);
		this.mainContainer().openMethodBrowser(
			result,
			specification.label,
			null,
			null,
			"methodClass"
		);
	};

	async promptChangeParameters(specification, element) {
		const parameters = specification.parameters;
		let options = {};
		await Promise.all(
			parameters.map(async (p) => {
				let list = await this.resolveChangeParameterOptions(p.options);
				options[p.name] = list;
			})
		);
		const inputs = parameters.map((p) => {
			let defaultValue;
			if (p.defaultValue) {
				defaultValue = this.resolveDotExpressions(
					p.defaultValue,
					"element",
					element
				);
			}
			return {
				name: p.name,
				label: p.label,
				type: p.type,
				defaultValue: defaultValue,
				options: options[p.name],
				required: true,
			};
		});
		return await this.fillForm({
			title: specification.label,
			message: specification.description,
			inputs: inputs,
		});
	}

	resolveDotExpressions(string, variable, target) {
		if (typeof string !== "string") return string;
		const parts = string.split(/{([^}]+)}/g).filter((p) => p.length > 0);
		if (parts.length === 1 && parts[0].startsWith(variable + ".")) {
			return target[parts[0].split(".")[1]];
		}
		let result = "";
		parts.forEach((p) => {
			result += p.startsWith(variable + ".")
				? target[p.split(".")[1]]
				: p;
		});
		return result;
	}

	async resolveChangeParameterOptions(options) {
		if (options === "{packages}") return await this.backend.packageNames();
		if (options === "{classes}") return await this.backend.classNames();
		return options;
	}

	performChange = async (fx) => {
		let applied;
		try {
			applied = await fx(this.backend);
		} catch (error) {
			applied = await this.handleChangeError(error);
		}
		return applied;
	};

	async handleChangeError(error) {
		const data = error.data;
		if (!data || typeof data === "string") {
			this.reportError(error);
			return;
		}
		const suggestions = data.suggestions;
		if (suggestions && suggestions.length > 0) {
			let suggestion;
			if (suggestions.length === 1) {
				let confirm = await this.confirm({
					title: data.description,
					message: suggestions[0].description,
				});
				if (confirm) suggestion = suggestions[0];
			} else {
				let description = await this.choose({
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
			let last;
			if (suggestion) {
				try {
					for (const change of suggestion.changes) {
						last = await this.backend.postChange(change);
					}
				} catch (inner) {
					return this.handleChangeError(inner);
				}
			}
			return last;
		}
		const description = data ? data.description : data;
		ide.reportError(description || "Unknown change error");
	}

	download(blob, filename) {
		try {
			const url = window.URL.createObjectURL(blob);
			const link = document.createElement("a");
			link.href = url;
			link.setAttribute("download", filename);
			document.body.appendChild(link);
			link.click();
			document.body.removeChild(link);
		} catch (error) {
			this.reportError(error);
		}
	}

	// Code assistant chat...

	updateAssistantChat = () => {
		if (!this.assistantChatRef) return;
		const chat = this.assistantChatRef.current;
		if (chat) {
			chat.forceUpdate();
			chat.scrollToLastMessage();
		}
	};

	closeAssistantChat = () => {
		this.setState({ assistantChatOpened: false });
	};

	toggleOpenAssistantChat = () => {
		this.setState(
			{ assistantChatOpened: !this.state.assistantChatOpened },
			this.updateAssistantChat
		);
	};

	minimizeAssistantChat = () => {
		this.setState({
			assistantChatOpened: false,
			assistantChatMaximized: false,
		});
	};

	maximizeAssistantChat = () => {
		this.setState(
			{ assistantChatMaximized: true },
			this.updateAssistantChat
		);
	};

	explainCode = async (code) => {
		this.codeAssistant.explainCode(code).then(this.updateAssistantChat);
		this.setState({ assistantChatOpened: true }, this.updateAssistantChat);
	};

	testCode = async (code) => {
		this.codeAssistant
			.writeTestForCode(code)
			.then(this.updateAssistantChat);
		this.setState({ assistantChatOpened: true }, this.updateAssistantChat);
	};

	improveCode = async (code) => {
		this.codeAssistant.improveCode(code).then(this.updateAssistantChat);
		this.setState({ assistantChatOpened: true }, this.updateAssistantChat);
	};

	render() {
		console.log("rendering IDE");
		const {
			sidebarExpanded,
			unreadErrorsCount,
			unreadMessages,
			lastMessage,
			extraContainers,
			waiting,
			quickSearchOpen,
			quickSearchOptions,
			assistantChatOpened,
			assistantChatMaximized,
		} = this.state;
		const extraContainersWidth =
			Math.round(100 / (extraContainers.length + 1)) + "%";
		const extraContainersMinWidth = "10%";
		const shortcuts = this.settings.section("shortcuts");
		const showsAssistant = this.usesCodeAssistant();
		const photo = this.settings.section("general").get("photo");
		return (
			<Hotkeys
				keyName={
					shortcuts.get("quickSearch") +
					"," +
					shortcuts.get("openClassBrowser") +
					"," +
					shortcuts.get("newWorkspace") +
					"," +
					shortcuts.get("moveToLeftTab") +
					"," +
					shortcuts.get("moveToRightTab")
				}
				filter={(event) => {
					return true;
				}}
				allowRepeat={false}
				onKeyDown={(hotkey, e, handle) => this.hotkeyPressed(hotkey)}
			>
				<DialogProvider>
					<Box display="flex" sx={{ height: "95vh" }}>
						<Titlebar
							developer={this.currentDeveloper()}
							dialect={this.currentDialect()}
							logo={this.logo}
							sidebarExpanded={sidebarExpanded}
							onSidebarExpand={this.expandSidebar}
							searchOptions={[]}
							onSettingsClick={this.openSettings}
							onDisconnectClick={this.disconnect}
							colorMode={this.settings
								.section("appearance")
								.get("mode")}
							onColorModeToggle={this.toggleColorMode}
							onSearchClick={this.openQuickSearch}
							searchPlaceholder={
								"Use " + shortcuts.get("quickSearch")
							}
							onSplit={
								extraContainers.length < MaxExtraContainers
									? this.splitContainer
									: null
							}
							photo={photo}
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
							onPeersClick={this.messageChannel && this.openChat}
							onSettingsClick={this.openSettings}
							onCollapse={this.collapseSidebar}
						/>

						<Box
							component="main"
							mt={6}
							//disableGutters
							flexGrow={1}
							sx={{
								height: "95vh",
								width: "100vw",
								maxWidth: "95vw",
								padding: 1,
							}}
						>
							<CustomSplit>
								<Box flex={1}>
									<CustomSplit>
										<Box
											key="mainContainerBox"
											flex={1}
											sx={{
												minWidth:
													extraContainersMinWidth,
												width: extraContainersWidth,
											}}
										>
											<ToolContainer
												id={99999}
												key="mainContainer"
												ref={this.mainContainerRef}
												onPageFocus={
													this.pageFocusedInContainer
												}
												//onSplit={this.splitContainer} //disabled for the moment
												showClose={false}
											/>
										</Box>
										{extraContainers.map(
											(container, index) => (
												<Box
													key={
														"container" +
														index +
														"Box"
													}
													sx={{
														minWidth:
															extraContainersMinWidth,
														width: extraContainersWidth,
													}}
												>
													{container.component}
												</Box>
											)
										)}
									</CustomSplit>
								</Box>
								{showsAssistant && assistantChatMaximized && (
									<Box
										width="25%"
										minWidth="15%"
										display="flex"
										flexDirection="column"
									>
										<Box display="flex" flexDirection="row">
											<Box flexGrow={1} mt={1}>
												<Typography variant="body1">
													Code assistant
												</Typography>
											</Box>
											<IconButton
												onClick={
													this.minimizeAssistantChat
												}
												size="small"
											>
												<MinimizeIcon fontSize="small" />
											</IconButton>
										</Box>
										<Box flexGrow={1} mt={1}>
											<CodeAssistantChat
												ref={this.assistantChatRef}
											/>
										</Box>
									</Box>
								)}
							</CustomSplit>
							{showsAssistant && !assistantChatMaximized && (
								<Fab
									id="assitantLocation"
									sx={{
										position: "fixed",
										bottom: (theme) => theme.spacing(2),
										right: (theme) => theme.spacing(2),
									}}
									onClick={this.toggleOpenAssistantChat}
									color="primary"
								>
									<AssistantIcon />
								</Fab>
							)}
							{showsAssistant && !assistantChatMaximized && (
								<Popper
									open={assistantChatOpened}
									//placement="left-end"
									anchorEl={document.getElementById(
										"assitantLocation"
									)}
								>
									<Card variant="outlined">
										<CardHeader
											disableTypography
											action={
												<Box>
													<IconButton
														onClick={
															this
																.closeAssistantChat
														}
													>
														<MinimizeIcon fontSize="small" />
													</IconButton>
													<IconButton
														onClick={
															this
																.maximizeAssistantChat
														}
													>
														<MaximizeIcon fontSize="small" />
													</IconButton>
												</Box>
											}
											title="Code assistant"
											style={{
												paddingTop: 8,
												paddingBottom: 5,
											}}
										/>
										<CardContent
											style={{
												paddingTop: 5,
												paddingBottom: 5,
											}}
										>
											<Box
												display="flex"
												sx={{
													height: 400,
													minWidth: 400,
												}}
											>
												<CodeAssistantChat
													ref={this.assistantChatRef}
												/>
											</Box>
										</CardContent>
									</Card>
								</Popper>
							)}
						</Box>
					</Box>
					<Backdrop
						//sx={{ zIndex: (theme) => theme.zIndex.drawer + 1 }}
						open={waiting}
					>
						<CircularProgress color="inherit" />
					</Backdrop>
					<CustomSnacks
						open={lastMessage !== null}
						onClose={() => this.setState({ lastMessage: null })}
						action={lastMessage ? lastMessage.action : null}
						text={lastMessage ? lastMessage.text : ""}
						severity={lastMessage ? lastMessage.type : ""}
					/>
				</DialogProvider>
				<Dialog
					onClose={() => this.setState({ quickSearchOpen: false })}
					open={quickSearchOpen}
				>
					<DialogTitle>Quick Search</DialogTitle>
					<DialogContent dividers sx={{ width: 600, height: 400 }}>
						<QuickSearch
							initialOptions={quickSearchOptions}
							onResultSelect={(result, options) =>
								this.setState({
									quickSearchOpen: false,
									quickSearchOptions: options,
								})
							}
						/>
					</DialogContent>
				</Dialog>
			</Hotkeys>
		);
	}
}

export default withDialog()(withNavigation(IDE));

export { ide };
