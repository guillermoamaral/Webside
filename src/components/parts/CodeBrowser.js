import React, { Component } from "react";
import {
	Box,
	Paper,
	Link,
	ToggleButton,
	ToggleButtonGroup,
	FormGroup,
	FormControlLabel,
	Checkbox,
	Typography,
} from "@mui/material";
import { ide } from "../IDE";
import ToolContainerContext from "../ToolContainerContext";
import CodeEditor from "./CodeEditor";
import MarkdownPreview from "@uiw/react-markdown-preview";

class CodeBrowser extends Component {
	static contextType = ToolContainerContext;

	constructor(props) {
		super(props);
		this.state = {
			method: null,
			selectedMode: "source",
			previewMarkdown: true,
		};
	}

	static getDerivedStateFromProps(props, state) {
		const selected = state.selectedMode;
		const mode =
			!props.method && selected === "source"
				? "definition"
				: (selected === "definition" || selected === "comment") &&
				  props.method &&
				  props.method !== state.method
				? "source"
				: selected;
		if (props.method === state.method && mode === selected) {
			return null;
		}
		return {
			selectedMode: mode,
			method: props.method,
		};
	}

	defineClass = async (definition) => {
		if (!this.props.class) {
			return;
		}
		const pack = this.props.package;
		const species = this.props.class;
		const classname = species ? species.name : null;
		const superclass = species ? species.superclass : null;
		const packagename = pack ? pack.name : species ? species.package : null;
		try {
			const change = await ide.backend.defineClass(
				classname,
				superclass,
				packagename,
				definition
			);
			const species = await ide.backend.classNamed(change.className);
			if (this.props.onClassDefine) {
				this.props.onClassDefine(species);
			}
		} catch (error) {
			ide.reportError(error);
		}
	};

	renameClass = async (target) => {
		if (
			target !== this.props.class.name &&
			target !== this.props.class.superclass
		) {
			return;
		}
		try {
			const newName = await ide.prompt({
				title: "Rename class",
				defaultValue: target,
				required: true,
			});
			await ide.backend.renameClass(target, newName);
			this.props.class.name = newName;
			if (this.props.onRenameClass) {
				this.props.onRenameClass(this.props.class);
			}
		} catch (error) {
			ide.reportError(error);
		}
	};

	commentClass = async (comment) => {
		if (!this.props.class) return;
		try {
			await ide.backend.commentClass(this.props.class.name, comment);
			const species = await ide.backend.classNamed(this.props.class.name);
			if (this.props.onClassComment) {
				this.props.onClassComment(species);
			}
		} catch (error) {
			ide.reportError(error);
		}
	};

	compileMethod = async (source) => {
		if (!this.props.class) return;
		try {
			const pack = this.props.package;
			const species = this.props.class;
			const method = this.props.method;
			const packagename = pack
				? pack.name
				: method
				? method.package
				: species
				? species.package
				: null;
			const category = method
				? method.category || this.props.category
				: this.props.category;
			const classname = species
				? species.name
				: method
				? method.methodClass
				: null;
			const change = await ide.backend.compileMethod(
				classname,
				packagename,
				category,
				source
			);
			const compiled = await ide.backend.method(
				classname,
				change.selector
			);
			if (this.props.onMethodCompile)
				this.props.onMethodCompile(compiled);
		} catch (error) {
			this.handleCompilationError(error, source);
		}
	};

	async handleCompilationError(error, source) {
		const method = this.props.method;
		if (!method) return;
		method.source = source;
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
					let applied;
					for (const change of suggestion.changes) {
						applied = await ide.backend.postChange(change);
					}
					let method;
					if (applied && applied.type === "AddMethod") {
						try {
							method = await ide.backend.method(
								this.props.class.name,
								applied.selector
							);
						} catch (error) {}
						if (method && this.props.onMethodCompile)
							this.props.onMethodCompile(method);
					}
				} catch (inner) {
					this.handleCompilationError(
						inner,
						suggestion.changes[suggestion.changes.length - 1]
							.sourceCode
					);
				}
			}
		} else {
			if (data && data.interval) {
				method.annotations = [
					{
						from: data.interval.start,
						to: data.interval.end,
						type: "error",
						description: data.description,
					},
				];
			} else {
				const description = data ? data.description : null;
				ide.reportError(description || "Unknown compilation error");
			}
			this.setState({ method: method });
		}
	}

	availableModes = () => {
		const modes = [
			{ mode: "definition", label: "Class definition" },
			{ mode: "comment", label: "Class comment" },
		];
		const method = this.props.method;
		if (method) {
			modes.push({ mode: "source", label: "Method definition" });
			if (method.bytecodes) {
				modes.push({ mode: "bytecodes", label: "Bytecodes" });
			}
			if (method.disassembly) {
				modes.push({ mode: "disassembly", label: "Disassembly" });
			}
		}
		return modes;
	};

	currentSource = () => {
		const species = this.props.class;
		const method = this.props.method;
		let source;
		switch (this.state.selectedMode) {
			case "comment":
				source = species ? species.comment : "";
				break;
			case "definition":
				source = species ? species.definition : "";
				break;
			case "source":
				source = method ? method.source : "";
				break;
			case "bytecodes":
				source = method ? method.bytecodes || "" : "";
				break;
			case "disassembly":
				source = method ? method.disassembly || "" : "";
				break;
			default:
		}
		return source;
	};

	currentAst = () => {
		return this.state.selectedMode === "source"
			? this.props.method
				? this.props.method.ast
				: null
			: null;
	};

	currentCodeMode = () => {
		let mode;
		switch (this.state.selectedMode) {
			case "comment":
				mode = "text";
				break;
			case "disassembly":
				mode = "assembler";
				break;
			default:
				mode = "smalltalk";
		}
		return mode;
	};

	currentAuthor = () => {
		if (this.state.selectedMode === "source") {
			const method = this.props.method;
			return method ? method.author : "";
		}
		return "";
	};

	currentTimestamp = () => {
		const method = this.props.method;
		if (
			this.state.selectedMode === "source" &&
			method &&
			method.timestamp &&
			method.timestamp !== ""
		) {
			return new Date(method.timestamp).toLocaleString();
		}
		return "";
	};

	currentPackageName = () => {
		const pack = this.props.package;
		const species = this.props.class;
		const method = this.props.method;
		if (this.state.selectedMode === "source" && method) {
			return method.package;
		}
		if (species) {
			return species.package;
		}
		if (pack) return pack.name;
		return "";
	};

	currentAnnotations = () => {
		if (this.state.selectedMode === "source") {
			const method = this.props.method;
			return method ? method.annotations : [];
		}
		return "";
	};

	modeChanged = (event, mode) => {
		this.setState({ selectedMode: mode });
	};

	acceptClicked = (source) => {
		switch (this.state.selectedMode) {
			case "comment":
				this.commentClass(source);
				break;
			case "definition":
				this.defineClass(source);
				break;
			case "source":
				this.compileMethod(source);
				break;
			default:
		}
	};

	extendedOptionPerformed = () => {
		if (this.props.onExtendedOptionPerform)
			this.props.onExtendedOptionPerform();
	};

	render() {
		const { selectedMode, previewMarkdown } = this.state;
		const author = this.currentAuthor();
		const timestamp = this.currentTimestamp();
		const packagename = this.currentPackageName();
		const {
			selectedInterval,
			selectedSelector,
			selectedIdentifier,
			onTooltipShow,
			onTooltipClick,
		} = this.props;
		return (
			<Box display="flex" flexDirection="column" sx={{ height: "100%" }}>
				<Box display="flex" flexDirection="row">
					<Box flexGrow={1}>
						<ToggleButtonGroup
							label="primary"
							value={selectedMode}
							exclusive
							onChange={this.modeChanged}
						>
							{this.availableModes().map((m) => (
								<ToggleButton
									value={m.mode}
									variant="outlined"
									size="small"
									key={m.mode}
								>
									{m.label}
								</ToggleButton>
							))}
						</ToggleButtonGroup>
					</Box>
					{selectedMode === "comment" && (
						<FormGroup>
							<FormControlLabel
								control={
									<Checkbox
										size="small"
										checked={previewMarkdown}
										color="primary"
										onChange={(event) =>
											this.setState({
												previewMarkdown:
													event.target.checked,
											})
										}
									/>
								}
								label={
									<Typography variant="caption">
										Markdown view
									</Typography>
								}
							/>
						</FormGroup>
					)}
				</Box>
				<Box flexGrow={1}>
					<Paper
						variant="outlined"
						sx={{ height: "100%", minHeight: 300 }}
					>
						{(selectedMode !== "comment" || !previewMarkdown) && (
							<CodeEditor
								context={this.props.context}
								class={this.props.class}
								method={this.props.method}
								source={this.currentSource()}
								mode={this.currentCodeMode()}
								ast={this.currentAst()}
								annotations={this.currentAnnotations()}
								selectedInterval={selectedInterval}
								selectedSelector={selectedSelector}
								selectedIdentifier={selectedIdentifier}
								showAccept
								onAccept={this.acceptClicked}
								onRename={(target) => this.renameClass(target)}
								onTooltipShow={onTooltipShow}
								onTooltipClick={onTooltipClick}
								showAssistant
								onExtendedOptionPerform={
									this.extendedOptionPerformed
								}
							/>
						)}
						{selectedMode === "comment" && previewMarkdown && (
							<MarkdownPreview
								source={this.currentSource()}
								style={{ padding: 16 }}
							/>
						)}
					</Paper>
				</Box>
				<Box>
					{timestamp ? "Modified on " : ""}
					{timestamp}
					{author ? " by " : ""}
					{author && (
						<Link
							href="#"
							onClick={() => this.context.openChat(author)}
						>
							{author}
						</Link>
					)}
					{timestamp || author ? " - " : ""}
					{packagename && (
						<Link
							href="#"
							onClick={(event) => {
								event.preventDefault();
								this.context.browsePackage(packagename);
							}}
						>
							{packagename}
						</Link>
					)}
				</Box>
			</Box>
		);
	}
}

export default CodeBrowser;