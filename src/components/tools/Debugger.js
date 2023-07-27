import React from "react";
import Tool from "./Tool";
import {
	Grid,
	Typography,
	LinearProgress,
	Tabs,
	Tab,
	Box,
} from "@mui/material";
import { ide } from "../IDE";
import FrameList from "../parts/FrameList";
import BindingTable from "../parts/BindingTable";
import ExpressionTable from "../parts/ExpressionTable";
import CodeBrowser from "../parts/CodeBrowser";
import DebuggerControls from "../parts/DebuggerControls";
import CustomSplit from "../controls/CustomSplit";
import CustomPaper from "../controls/CustomPaper";

class Debugger extends Tool {
	constructor(props) {
		super(props);
		this.expressionTableRef = React.createRef();
		this.state = {
			frames: [],
			selectedFrame: null,
			stepping: false,
			showBindings: true,
			expressions: [],
		};
	}

	async aboutToClose() {
		try {
			await ide.backend.deleteDebugger(this.props.id);
		} catch (error) {
			ide.reportError(error);
		}
		if (this.props.onTerminate) {
			this.props.onTerminate();
		}
	}

	componentDidMount() {
		if (ide.messageChannel) {
			ide.messageChannel.onEvent("onMessageReceived", (message) => {
				if (message.type === "debuggerEvent") {
					//this.updateFrames();
				}
			});
		}
		this.updateFrames();
	}

	async updateFrames() {
		try {
			const frames = await ide.backend.debuggerFrames(this.props.id);
			var frame;
			if (frames.length > 0) {
				frame = frames[0];
				await this.updateFrame(frame);
			}
			this.setState({
				frames: frames,
				selectedFrame: frame,
				stepping: false,
			});
		} catch (error) {
			ide.reportError(error);
		}
	}

	updateExpressions = () => {
		if (
			!this.state.showBindings &&
			this.expressionTableRef &&
			this.expressionTableRef.current
		) {
			this.expressionTableRef.current.updateExpressions();
		}
	};

	frameSelected = async (frame) => {
		await this.updateFrame(frame, true);
		this.setState({ selectedFrame: frame }, this.updateExpressions);
	};

	updateFrame = async (frame, forced) => {
		try {
			if (forced || !frame.method) {
				const info = await ide.backend.debuggerFrame(
					this.props.id,
					frame.index
				);
				frame.method = info.method;
				frame.class = info.class;
				frame.interval = info.interval;
			}
			if (!frame.bindings || true) {
				const bindings = await ide.backend.frameBindings(
					this.props.id,
					frame.index
				);
				frame.bindings = bindings;
			}
		} catch (error) {
			ide.reportError(error);
		}
	};

	stepIntoClicked = async () => {
		try {
			await ide.backend.stepIntoDebugger(
				this.props.id,
				this.state.selectedFrame.index
			);
			this.notifyEvent("stepInto");
			this.updateFrames();
			this.updateExpressions();
		} catch (error) {
			ide.reportError(error);
		}
	};

	stepOverClicked = async () => {
		this.setState({ stepping: true });
		try {
			await ide.backend.stepOverDebugger(
				this.props.id,
				this.state.selectedFrame.index
			);
			this.notifyEvent("stepOver");
			this.updateFrames();
			this.updateExpressions();
		} catch (error) {
			ide.reportError(error);
		}
	};

	stepThroughClicked = async () => {
		try {
			await ide.backend.stepThroughDebugger(
				this.props.id,
				this.state.selectedFrame.index
			);
			this.notifyEvent("stepThrough");
			this.updateFrames();
			this.updateExpressions();
		} catch (error) {
			ide.reportError(error);
		}
	};

	restartClicked = async () => {
		try {
			await ide.backend.restartDebugger(
				this.props.id,
				this.state.selectedFrame.index
			);
			this.notifyEvent("restart");
			this.updateFrames();
			this.updateExpressions();
		} catch (error) {
			ide.reportError(error);
		}
	};

	resumeClicked = async () => {
		try {
			await ide.backend.resumeDebugger(this.props.id);
			if (this.props.onResume) {
				this.props.onResume();
			}
		} catch (error) {
			ide.reportError(error);
		}
	};

	terminateClicked = async () => {
		try {
			await ide.backend.terminateDebugger(this.props.id);
			if (this.props.onTerminate) {
				this.props.onTerminate();
			}
		} catch (error) {
			ide.reportError(error);
		}
	};

	methodCompiled = async (method) => {
		const selected = this.state.selectedFrame.method;
		if (method.selector !== selected.selector) {
			return;
		}
		try {
			await ide.backend.restartDebugger(
				this.props.id,
				this.state.selectedFrame.index,
				true
			);
			this.updateFrames();
		} catch (error) {
			ide.reportError(error);
		}
	};

	notifyEvent(event) {
		if (ide.messageChannel) {
			ide.messageChannel.sendDebuggerEvent(event, this.props.id);
		}
	}

	evaluationContext() {
		const frame = this.state.selectedFrame;
		return frame
			? {
					debugger: this.props.id,
					frame: frame.index,
			  }
			: {};
	}

	tooltipForBinding = (name) => {
		const frame = this.state.selectedFrame;
		if (!frame) return;
		const binding = frame.bindings.find((b) => b.name === name);
		if (!binding) return;
		return binding.value;
	};

	inspectBinding = async (name) => {
		try {
			const object = await this.context.evaluateExpression(
				name,
				false,
				true,
				this.evaluationContext()
			);
			this.context.openInspector(object);
		} catch (error) {
			this.context.reportError(error);
		}
	};

	addExpression = (expression) => {
		this.setState({
			expressions: [...this.state.expressions, expression],
		});
	};

	removeExpression = (expression) => {
		this.setState({
			expressions: this.state.expressions.filter(
				(e) => e.id !== expression.id
			),
		});
	};

	render() {
		const id = this.props.id;
		var title = this.props.title || "";
		if (title.length > 100) title = title.slice(0, 50) + "...";
		const { frames, selectedFrame, stepping, showBindings, expressions } =
			this.state;
		return (
			<Box
				display="flex"
				flexDirection="column"
				style={{ height: "100%" }}
			>
				<Box>
					<Grid
						container
						spacing={1}
						direction="row"
						alignItems="center"
						justify="center"
					>
						<Grid item xs={4} md={4} lg={4}>
							<DebuggerControls
								disabled={stepping}
								onStepIntoClicked={this.stepIntoClicked}
								onStepOverClicked={this.stepOverClicked}
								onStepThroughClicked={this.stepThroughClicked}
								onRestartClicked={this.restartClicked}
								onResumeClicked={this.resumeClicked}
								onTerminateClicked={this.terminateClicked}
							/>
						</Grid>
						<Grid item xs={4} md={4} lg={4}>
							<Box
								display="flex"
								alignItems="center"
								justifyContent="center"
							>
								<Typography variant="body1">{title}</Typography>
							</Box>
						</Grid>
						<Grid item>
							<Tabs
								value={showBindings ? 0 : 1}
								onChange={(event, value) => {
									this.setState({
										showBindings: value === 0,
									});
								}}
								indicatorColor="primary"
								textColor="primary"
							>
								<Tab label="Bindings" />
								<Tab label="Watch expressions" />
							</Tabs>
						</Grid>
					</Grid>
				</Box>
				<Box flexGrow={1}>
					<CustomSplit mode="vertical">
						<Box sx={{ minHeight: 50, height: "35%" }}>
							<Grid container spacing={1} sx={{ height: "100%" }}>
								<Grid item xs={12} md={8} lg={8}>
									<CustomPaper>
										<FrameList
											frames={frames}
											selectedFrame={selectedFrame}
											onFrameSelect={this.frameSelected}
										/>
									</CustomPaper>
								</Grid>
								<Grid item xs={12} md={4} lg={4}>
									{showBindings && (
										<BindingTable
											style={{ height: 300 }}
											id={id}
											frame={selectedFrame}
										/>
									)}
									{!showBindings && (
										<ExpressionTable
											ref={this.expressionTableRef}
											style={{ height: 300 }}
											id={id}
											frame={selectedFrame}
											expressions={expressions}
											onExpressionAdd={this.addExpression}
											onExpressionRemove={
												this.removeExpression
											}
										/>
									)}
								</Grid>
							</Grid>
						</Box>
						<Box sx={{ height: "60%" }}>
							<CodeBrowser
								context={this.evaluationContext()}
								class={
									selectedFrame ? selectedFrame.class : null
								}
								method={
									selectedFrame ? selectedFrame.method : null
								}
								selectedInterval={
									selectedFrame
										? selectedFrame.interval
										: null
								}
								onCompileMethod={this.methodCompiled}
								onDefineClass={this.classDefined}
								onCommentClass={this.classCommented}
								onTooltipShow={this.tooltipForBinding}
								onTooltipClick={this.inspectBinding}
							/>
						</Box>
					</CustomSplit>
				</Box>
				<Box>
					{stepping && <LinearProgress variant="indeterminate" />}
				</Box>
			</Box>
		);
	}
}

export default Debugger;
