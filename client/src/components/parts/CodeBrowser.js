import React, { Component } from "react";
import { Grid, Paper, Link } from "@material-ui/core";
import { ToggleButton, ToggleButtonGroup } from "@material-ui/lab";
import { IDEContext } from "../IDEContext";
import CodeEditor from "./CodeEditor";
import { withDialog } from "../dialogs/index";
//import clsx from "clsx";

class CodeBrowser extends Component {
	static contextType = IDEContext;

	constructor(props) {
		super(props);
		this.state = {
			method: null,
			selectedMode: "source",
		};
	}

	static getDerivedStateFromProps(props, state) {
		const mode =
			!props.method && state.selectedMode === "source"
				? "definition"
				: (state.selectedMode === "definition" ||
						state.selectedMode === "comment") &&
				  props.method !== state.method
				? "source"
				: state.selectedMode;
		if (props.method === state.method && mode === state.selectedMode) {
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
		const classname = this.props.class ? this.props.class.name : null;
		const packagename = this.props.class ? this.props.class.package : null;
		try {
			const change = await this.context.api.defineClass(
				classname,
				packagename,
				definition
			);
			const species = await this.context.api.getClass(change.class);
			const handler = this.props.onDefineClass;
			if (handler) {
				handler(species);
			}
		} catch (error) {
			this.context.reportError(error);
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
			const newName = await this.props.dialog.prompt({
				title: "Rename class",
				defaultValue: target,
				required: true,
			});
			await this.context.api.renameClass(target, newName);
			this.props.class.name = newName;
			const handler = this.props.onRenameClass;
			if (handler) {
				handler(this.props.class);
			}
		} catch (error) {
			this.context.reportError(error);
		}
	};

	commentClass = async (comment) => {
		if (!this.props.class) {
			return;
		}
		try {
			await this.context.api.commentClass(this.props.class.name, comment);
			const species = await this.context.api.getClass(this.props.class.name);
			const handler = this.props.onCommentClass;
			if (handler) {
				handler(species);
			}
		} catch (error) {
			this.context.reportError(error);
		}
	};

	compileMethod = async (source) => {
		if (!this.props.class) {
			return;
		}
		try {
			const method = this.props.method;
			const species = this.props.class;
			const packagename = method ? method.package : null;
			const category = method ? method.category : null;
			const classname = species ? species.name : method ? method.class : null;
			const change = await this.context.api.compileMethod(
				classname,
				packagename,
				category,
				source
			);
			const compiled = await this.context.api.getMethod(
				classname,
				change.selector
			);
			const handler = this.props.onCompileMethod;
			if (handler) {
				handler(compiled);
			}
		} catch (error) {
			this.handleCompilationError(error, source);
		}
	};

	async handleCompilationError(error, source) {
		const method = this.props.method;
		if (!method) {
			return;
		}
		method.source = source;
		const data = error.data;
		console.log(data);
		if (data && data.suggestion && data.changes) {
			const retry = await this.props.dialog.confirm(data.suggestion + "?");
			if (retry) {
				try {
					let method;
					for (const change of data.changes) {
						const applied = await this.context.api.postChange(change);
						method = await this.context.api.getMethod(
							this.props.class.name,
							applied.selector
						);
					}
					const handler = this.props.onCompileMethod;
					if (handler) {
						handler(method);
					}
				} catch (error) {
					this.handleCompilationError(
						error,
						data.changes[data.changes.length - 1].sourceCode
					);
				}
			}
		} else {
			if (data && data.interval) {
				method.lintAnnotations = [
					{
						from: data.interval.start,
						to: data.interval.end,
						severity: "error",
						description: data.description,
					},
				];
			} else {
				const description = data ? data.description : null;
				this.context.reportError(description || "Unknown compilation error");
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
				mode = "gas";
				break;
			default:
				mode =
					this.context.dialect === "Python" ? "python" : "smalltalk-method";
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

	currentPackage = () => {
		const species = this.props.class;
		const method = this.props.method;
		if (this.state.selectedMode === "source" && method) {
			return method.package;
		}
		if (species) {
			return species.package;
		}
		return "";
	};

	currentLintAnnotations = () => {
		if (this.state.selectedMode === "source") {
			const method = this.props.method;
			return method ? method.lintAnnotations : [];
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

	render() {
		const mode = this.state.selectedMode;
		const author = this.currentAuthor();
		const timestamp = this.currentTimestamp();
		const packagename = this.currentPackage();
		const { selectedInterval, selectedWord } = this.props;
		//const styles = this.props.styles;
		//const fixedHeightPaper = clsx(styles.paper, styles.fixedHeight);
		return (
			<Grid container spacing={1}>
				<Grid item xs={12} md={12} lg={12}>
					<ToggleButtonGroup
						label="primary"
						value={mode}
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
				</Grid>
				<Grid item xs={12} md={12} lg={12}>
					<Paper variant="outlined" style={{ height: "100%", minHeight: 300 }}>
						<CodeEditor
							context={this.props.context}
							styles={this.props.styles}
							lineNumbers={true}
							source={this.currentSource()}
							ast={this.currentAst()}
							mode={this.currentCodeMode()}
							lintAnnotations={this.currentLintAnnotations()}
							selectedInterval={selectedInterval}
							selectedWord={selectedWord}
							showAccept
							onAccept={this.acceptClicked}
							onRename={(target) => this.renameClass(target)}
						/>
					</Paper>
				</Grid>
				<Grid item xs={12} md={12} lg={12}>
					{timestamp ? "Modified on " : ""}
					{timestamp}
					{author ? " by " : ""}
					{author && (
						<Link href="#" onClick={() => this.context.openChat(author)}>
							{author}
						</Link>
					)}
					{timestamp || author ? " - " : ""}
					{packagename && (
						<Link
							href="#"
							onClick={() => this.context.browsePackage(packagename)}
						>
							{packagename}
						</Link>
					)}
				</Grid>
			</Grid>
		);
	}
}

export default withDialog()(CodeBrowser);
