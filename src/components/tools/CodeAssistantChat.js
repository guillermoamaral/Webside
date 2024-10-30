import React from "react";
import Tool from "./Tool";
import {
	Box,
	List,
	ListItem,
	Avatar,
	Typography,
	IconButton,
	Paper,
	Button,
	Link,
} from "@mui/material";
import SendIcon from "@mui/icons-material/Send";
import CodeEditor from "../parts/CodeEditor";
import PromptEditor from "../parts/PromptEditor";
import Scrollable from "../controls/Scrollable";
import { ide } from "../IDE";
import MarkdownView from "../parts/MarkdownView";
import CustomSplit from "../controls/CustomSplit";
import CopyIcon from "@mui/icons-material/ContentCopy";
import AssistantIcon from "@mui/icons-material/Assistant";
//import ChangesBrowserIcon from "../icons/ChangesBrowserIcon";

const MessageItem = ({ message }) => {
	console.log(message);
	const parts = message ? message.parts : [];
	const who = message.role === "user" ? ide.currentDeveloper() : message.role;
	const photo = ide.settings.section("general").get("photo");
	return (
		<Box display="flex" flexDirection="column" sx={{ width: "100%" }}>
			<Box display="flex" alignItems="center">
				{message.role === "user" && (
					<Avatar
						alt={who}
						sx={{
							width: 26,
							height: 26,
							marginRight: 1,
						}}
						src={photo}
					/>
				)}
				{message.role === "assistant" && (
					<Avatar
						alt={who}
						sx={{
							width: 26,
							height: 26,
							marginRight: 1,
						}}
					>
						<AssistantIcon fontSize="small" />
					</Avatar>
				)}
				<Typography>{who}</Typography>
			</Box>
			{parts && (
				<Box ml={5}>
					{parts.map((part, index) => {
						if (part.type === "code") {
							const height = Math.min(
								part.content.split("\n").length * 20 + 40,
								400
							);
							return (
								<Paper variant="outlined" key={"code" + index}>
									<Box
										display="flex"
										flexDirection="row"
										justifyContent="flex-end"
									>
										<IconButton
											size="small"
											key={"copyCode" + index}
											color="primary"
											onClick={() =>
												navigator.clipboard.writeText(
													part.content
												)
											}
										>
											<CopyIcon fontSize="small" />
										</IconButton>
										{/* {part.code && (
											<IconButton
												size="small"
												key={"browseCode" + index}
												color="primary"
												onClick={() =>
													ide.browseChanges(
														part.code.changeset()
													)
												}
											>
												<ChangesBrowserIcon fontSize="small" />
											</IconButton>
										)} */}
									</Box>
									<Box
										key={index}
										display="flex"
										variant="outlined"
										ml={2}
										mt={1}
										sx={{ height: height }}
									>
										<CodeEditor
											source={part.content}
											readOnly
											showAccept={false}
											fontSize={
												ide.settings
													.section("appearance")
													.get("fontSize") -
												1 +
												"px"
											}
											noTooltips
										/>
									</Box>
								</Paper>
							);
						}
						return (
							<MarkdownView
								key={"markdown" + index}
								source={part.content}
							/>
						);
					})}
				</Box>
			)}
		</Box>
	);
};

class CodeAssistantChat extends Tool {
	constructor(props) {
		super(props);
		this.assistant = ide.codeAssistant;
		this.state = {
			prompt: "",
			processing: false,
		};
		this.messagesRef = React.createRef();
		this.lastItemRef = React.createRef();
		this.editorRef = React.createRef();
	}

	componentDidUpdate() {
		this.scrollToLastMessage();
	}

	scrollToLastMessage = () => {
		const ref = this.lastItemRef;
		if (ref && ref.current) {
			ref.current.scrollIntoView({
				behaviour: "smooth",
			});
		}
	};

	updateState(processing) {
		this.setState(
			{
				messages: this.messages(),
				prompt: "",
				processing: processing,
			},
			this.scrollToLastMessage
		);
	}

	explain = async () => {
		this.assistant.explainCode(this.props.source).then(() => {
			this.updateState(false);
		});
		this.updateState(true);
	};

	writeTest = async () => {
		this.assistant.testCode(this.props.source).then(() => {
			this.updateState(false);
		});
		this.updateState(true);
	};

	improve = async () => {
		this.assistant.improveCode(this.props.source).then(() => {
			this.updateState(false);
		});
		this.updateState(true);
	};

	sendPrompt = () => {
		const prompt = this.state.prompt;
		this.assistant.freePrompt(prompt).then(() => {
			this.updateState(false);
		});
		this.updateState(true);
		this.clearEditor();
	};

	clearEditor() {
		const ref = this.editorRef;
		if (ref && ref.current) ref.current.clear();
	}

	messages() {
		return this.assistant.messages.filter((m) => !m.isSystemMessage());
	}

	deleteMessageHistory = (event) => {
		event.preventDefault();
		this.assistant.clearHistory();
		this.forceUpdate();
	};

	promptChanged = (text) => {
		this.setState({ prompt: text });
	};

	keyPressed = (event) => {
		event.stopPropagation();
		if (event.key === "Enter" && !event.ctrlKey && !event.shiftKey) {
			this.sendPrompt();
		}
	};

	render() {
		console.log("rendering CAC");
		const { prompt, processing } = this.state;
		const messages = this.messages();
		const showButtons = false;
		return (
			<Box
				display="flex"
				flexDirection="column"
				sx={{ height: "100%", width: "100%" }}
			>
				<Box flexGrow={1}>
					<CustomSplit mode="vertical">
						<Box height={"90%"}>
							<Paper variant="outlined" sx={{ height: "100%" }}>
								<Scrollable>
									<List
										ref={this.messagesRef}
										sx={{ height: "100%" }}
									>
										{messages.map((m, i) => {
											const ref =
												i === messages.length - 1
													? this.lastItemRef
													: null;
											return (
												<ListItem
													key={i}
													ref={ref}
													sx={{
														flexDirection: "column",
														alignItems:
															"flex-start",
														//padding: 2,
													}}
												>
													<MessageItem message={m} />
												</ListItem>
											);
										})}
									</List>
								</Scrollable>
							</Paper>
						</Box>
						<Box height={"10%"}>
							{showButtons && (
								<Box display="flex" flexDirection="row" mt={1}>
									<Button
										size="small"
										variant="outlined"
										//tabIndex={-1}
										//startIcon={<DescriptionIcon />}
										sx={{ flexGrow: 1 }}
										onClick={this.explain}
										disabled={processing}
									>
										Explain
									</Button>
									<Button
										size="small"
										variant="outlined"
										//tabIndex={-1}
										//startIcon={<TestRunnerIcon />}
										sx={{
											flexGrow: 1,
											marginLeft: 1,
											marginRight: 1,
										}}
										onClick={this.writeTest}
										disabled={processing}
									>
										Write test
									</Button>
									<Button
										size="small"
										variant="outlined"
										//tabIndex={-1}
										//startIcon={<ImproveIcon />}
										sx={{ flexGrow: 1 }}
										onClick={this.improve}
										disabled={processing}
									>
										Improve
									</Button>
								</Box>
							)}
							<Box
								display="flex"
								flexDirection="row"
								alignItems="center"
								style={{ width: "100%", height: "100%" }}
							>
								<Paper
									variant="outlined"
									style={{ width: "100%", height: "100%" }}
								>
									<PromptEditor
										ref={this.editorRef}
										onChange={this.promptChanged}
										onAccept={this.sendPrompt}
									/>
								</Paper>
								<IconButton
									onClick={this.sendPrompt}
									disabled={prompt === ""}
								>
									<SendIcon size="small" />
								</IconButton>
							</Box>
						</Box>
					</CustomSplit>
				</Box>
				<Box
					display="flex"
					flexDirection="row"
					alignContent="space-between"
				>
					<Box flexGrow={1}>
						<Typography variant="caption">
							Use @ to include a class and # to include a method
							in a given class
						</Typography>
					</Box>
					<Box display="flex" flexDirection="row">
						<Box
							display="flex"
							flexDirection="row"
							alignItems="center"
						>
							<Link
								variant="caption"
								onClick={this.deleteMessageHistory}
								color="inherit"
								underline="hover"
								href="#"
							>
								Clear history
							</Link>
						</Box>
					</Box>
				</Box>
			</Box>
		);
	}
}

export default CodeAssistantChat;
