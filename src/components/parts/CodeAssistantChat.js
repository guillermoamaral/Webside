import React, { Component } from "react";
import {
	Box,
	TextField,
	List,
	ListItem,
	Avatar,
	Typography,
	IconButton,
	Paper,
	Button,
	FormGroup,
	FormControlLabel,
	Checkbox,
	Link,
} from "@mui/material";
import SendIcon from "@mui/icons-material/Send";
import CodeEditor from "./CodeEditor";
import Scrollable from "../controls/Scrollable";
import { ide } from "../IDE";

const MessageItem = ({ imageSrc, role, parts }) => (
	<Box display="flex" flexDirection="column" sx={{ width: "100%" }}>
		<Box display="flex" alignItems="center" mb={1}>
			<Avatar
				src={imageSrc}
				alt={role}
				sx={{ width: 26, height: 26, marginRight: 1 }}
			/>
			<Typography>{role}</Typography>
		</Box>
		{parts && (
			<Box ml={5} mb={1}>
				{parts.map((part, index) => {
					if (part.type === "code") {
						const height = Math.min(
							part.content.split("\r").length * 18,
							400
						);
						return (
							<Box
								key={index}
								display="flex"
								variant="outlined"
								ml={2}
								mt={1}
								sx={{
									//width: 600,
									height: height,
								}}
							>
								<CodeEditor
									source={part.content}
									readOnly
									//noTooltips
									showAccept={false}
									fontSize={
										ide.settings
											.section("appearance")
											.get("fontSize") -
										1 +
										"px"
									}
								/>
							</Box>
						);
					}
					// part.type === "text")
					return (
						<Typography key={index} variant="body2">
							{part.content}
						</Typography>
					);
				})}
			</Box>
		)}
	</Box>
);

class CodeAssistantChat extends Component {
	constructor(props) {
		super(props);
		this.assistant = ide.codeAssistant;
		this.state = {
			prompt: "",
			processing: false,
			useClassContext: true,
		};
		this.messagesRef = React.createRef();
		this.lastItemRef = React.createRef();
	}

	componentDidUpdate() {
		console.log("updated");
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

	prompt = () => {
		this.assistant.sendPrompt(this.state.prompt).then(() => {
			this.updateState(false);
		});
		this.updateState(true);
	};

	async useClassContext(boolean) {
		if (boolean && this.props.class) {
			await this.assistant.useClassContext(this.props.class.name);
		} else {
			this.assistant.clearLocalContext();
		}
		this.setState({ useClassContext: boolean });
	}

	messages() {
		return this.assistant.messages.filter((m) => m.parts);
	}

	deleteMessageHistory = (event) => {
		event.preventDefault();
		this.assistant.deleteMessageHistory();
		this.forceUpdate();
	};

	render() {
		console.log("rendering CAC");
		const { prompt, processing, useClassContext } = this.state;
		const messages = this.messages();
		const developer = ide.currentDeveloper();
		const showButtons = false;
		return (
			<Box
				display="flex"
				flexDirection="column"
				sx={{ height: "100%", width: "100%" }}
			>
				<Box flexGrow={1}>
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
									const role =
										m.role === "user" ? developer : m.role;
									return (
										<ListItem
											key={i}
											ref={ref}
											sx={{
												flexDirection: "column",
												alignItems: "flex-start",
												//padding: 2,
											}}
										>
											<MessageItem
												//imageSrc="https://example.com/avatar1.png"
												role={role}
												parts={m.parts}
											/>
										</ListItem>
									);
								})}
							</List>
						</Scrollable>
					</Paper>
				</Box>
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
							sx={{ flexGrow: 1, marginLeft: 1, marginRight: 1 }}
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
				<Box display="flex" flexDirection="row" alignItems="center">
					<Box flexGrow={1}>
						<TextField
							size="small"
							value={prompt}
							onChange={(event) =>
								this.setState({ prompt: event.target.value })
							}
							placeholder="Ask something..."
							name="text"
							variant="outlined"
							fullWidth
							margin="dense"
							autoFocus
							type="text"
							onKeyDown={(event) => {
								if (event.key === "Enter") {
									this.prompt();
								}
							}}
							disabled={processing}
						/>
					</Box>
					<Box display="flex" justifyContent="center">
						<IconButton
							onClick={this.prompt}
							disabled={prompt === ""}
						>
							<SendIcon size="small" />
						</IconButton>
					</Box>
				</Box>
				{/* <Box
					display="flex"
					flexDirection="row"
					alignContent="space-between"
				>
					<Box flexGrow={1}>
						<FormGroup>
							<FormControlLabel
								control={
									<Checkbox
										size="small"
										checked={useClassContext}
										color="primary"
										onChange={(event) =>
											this.useClassContext(
												event.target.checked
											)
										}
									/>
								}
								label={
									<Typography variant="caption">
										Use class context
									</Typography>
								}
							/>
						</FormGroup>
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
								Delete history
							</Link>
						</Box>
					</Box>
				</Box> */}
			</Box>
		);
	}
}

export default CodeAssistantChat;
