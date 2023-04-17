import React, { PureComponent } from "react";
import {
	Grid,
	Paper,
	Typography,
	LinearProgress,
	Tabs,
	Tab,
	Box,
} from "@material-ui/core";
import { ide } from "../IDE";
import FrameList from "../parts/FrameList";
import BindingTable from "../parts/BindingTable";
import ExpressionTable from "../parts/ExpressionTable";
import CodeBrowser from "../parts/CodeBrowser";
import DebuggerControls from "../parts/DebuggerControls";

class Debugger extends PureComponent {
	constructor(props) {
		super(props);
		this.state = {
			frames: [],
			selectedFrame: null,
			stepping: false,
			showBindings: true,
		};
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
			const frames = await ide.api.debuggerFrames(this.props.id);
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

	frameSelected = async (frame) => {
		await this.updateFrame(frame);
		this.setState({ selectedFrame: frame });
	};

	updateFrame = async (frame) => {
		try {
			if (!frame.method) {
				const info = await ide.api.debuggerFrame(
					this.props.id,
					frame.index
				);
				frame.method = info.method;
				frame.class = info.class;
				frame.interval = info.interval;
			}
			if (!frame.bindings || true) {
				const bindings = await ide.api.frameBindings(
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
			await ide.api.stepIntoDebugger(
				this.props.id,
				this.state.selectedFrame.index
			);
			this.notifyEvent("stepInto");
			this.updateFrames();
		} catch (error) {
			ide.reportError(error);
		}
	};

	stepOverClicked = async () => {
		this.setState({ stepping: true });
		try {
			await ide.api.stepOverDebugger(
				this.props.id,
				this.state.selectedFrame.index
			);
			this.notifyEvent("stepOver");
			this.updateFrames();
		} catch (error) {
			ide.reportError(error);
		}
	};

	stepThroughClicked = async () => {
		try {
			await ide.api.stepThroughDebugger(
				this.props.id,
				this.state.selectedFrame.index
			);
			this.notifyEvent("stepThrough");
			this.updateFrames();
		} catch (error) {
			ide.reportError(error);
		}
	};

	restartClicked = async () => {
		try {
			await ide.api.restartDebugger(
				this.props.id,
				this.state.selectedFrame.index
			);
			this.notifyEvent("restart");
			this.updateFrames();
		} catch (error) {
			ide.reportError(error);
		}
	};

	resumeClicked = async () => {
		try {
			await ide.api.resumeDebugger(this.props.id);
			ide.closeDebugger(this.props.id);
			if (this.props.onResume) {
				this.props.onResume();
			}
		} catch (error) {
			ide.reportError(error);
		}
	};

	terminateClicked = async () => {
		try {
			await ide.api.terminateDebugger(this.props.id);
			ide.closeDebugger(this.props.id);
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
			await ide.api.restartDebugger(
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

	render() {
		const { frames, selectedFrame, stepping, showBindings } = this.state;
		const styles = this.props.styles;
		return (
			<Grid container spacing={1}>
				<Grid item xs={12} md={12} lg={12}>
					<Grid
						container
						spacing={1}
						direction="row"
						alignItems="center"
						justifyContent="center"
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
								<Typography variant="body1">
									{this.props.title || ""}
								</Typography>
							</Box>
						</Grid>
						<Grid item>
							<Tabs
								value={showBindings ? 0 : 1}
								onChange={(event, value) => {
									this.setState({
										showBindings: value == 0,
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
				</Grid>
				<Grid item xs={12} md={12} lg={12}>
					<Grid container spacing={1}>
						<Grid item xs={12} md={8} lg={8}>
							<Paper style={{ height: 300 }} variant="outlined">
								<FrameList
									frames={frames}
									selected={selectedFrame}
									onSelect={this.frameSelected}
								/>
							</Paper>
						</Grid>
						<Grid item xs={12} md={4} lg={4}>
							{showBindings && (
								<BindingTable
									style={{ height: 300 }}
									styles={styles}
									id={this.props.id}
									frame={selectedFrame}
								/>
							)}
							{!showBindings && (
								<ExpressionTable
									style={{ height: 300 }}
									styles={styles}
									id={this.props.id}
									frame={selectedFrame}
								/>
							)}
						</Grid>
					</Grid>
				</Grid>
				<Grid item xs={12} md={12} lg={12}>
					<CodeBrowser
						context={this.evaluationContext()}
						styles={styles}
						class={selectedFrame ? selectedFrame.class : null}
						method={selectedFrame ? selectedFrame.method : null}
						selectedInterval={
							selectedFrame ? selectedFrame.interval : null
						}
						onCompileMethod={this.methodCompiled}
						onDefineClass={this.classDefined}
						onCommentClass={this.classCommented}
					/>
				</Grid>
				<Grid item xs={12} md={12} lg={12}>
					{stepping && <LinearProgress variant="indeterminate" />}
				</Grid>
			</Grid>
		);
	}
}

export default Debugger;
