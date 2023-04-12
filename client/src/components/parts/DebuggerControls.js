import React, { PureComponent } from "react";
import { Box, IconButton, Tooltip } from "@material-ui/core";
import { Icon } from "@iconify/react";
import RestartIcon from "@iconify/icons-mdi/replay";
import StepIntoIcon from "@iconify/icons-mdi/debug-step-into";
import StepOverIcon from "@iconify/icons-mdi/debug-step-over";
import StepThroughIcon from "../icons/StepThroughIcon";
import ResumeIcon from "@iconify/icons-mdi/play";
import TerminateIcon from "@iconify/icons-mdi/stop";
import Hotkeys from "react-hot-keys";

class DebuggerControls extends PureComponent {
	stepIntoClicked = () => {
		const handler = this.props.onStepIntoClicked;
		if (handler) {
			handler();
		}
	};

	stepOverClicked = () => {
		const handler = this.props.onStepOverClicked;
		if (handler) {
			handler();
		}
	};

	stepThroughClicked = () => {
		const handler = this.props.onStepThroughClicked;
		if (handler) {
			handler();
		}
	};

	restartClicked = () => {
		const handler = this.props.onRestartClicked;
		if (handler) {
			handler();
		}
	};

	resumeClicked = () => {
		const handler = this.props.onResumeClicked;
		if (handler) {
			handler();
		}
	};

	terminateClicked = () => {
		const handler = this.props.onTerminateClicked;
		if (handler) {
			handler();
		}
	};

	hotkeyPressed = async (hotkey) => {
		console.log(hotkey);
		const map = this.hotkeyMap();
		const command = Object.keys(map).find((k) => map[k] == hotkey);
		this.handlerFor(command)();
	};

	handlerFor(command) {
		var handler;
		switch (command) {
			case "restart":
				handler = this.restartClicked;
				break;
			case "stepInto":
				handler = this.stepIntoClicked;
				break;
			case "stepOver":
				handler = this.stepOverClicked;
				break;
			case "stepThrough":
				handler = this.stepThroughClicked;
				break;
			case "resume":
				handler = this.resumeClicked;
				break;
			default:
		}
		return handler;
	}

	hotkeyMap() {
		return {
			restart: "Ctrl+F6",
			stepInto: "Ctrl+F7",
			stepOver: "Ctrl+F8",
			stepThrough: "Ctrl+F9",
			resume: "Ctrl+F10",
		};
	}

	hotkeyFor(command) {
		return this.hotkeyMap()[command];
	}

	hotkeys() {
		var hotkeys = "";
		var first = true;
		Object.values(this.hotkeyMap()).forEach((k) => {
			hotkeys = hotkeys + (first ? "" : ", ") + k;
			first = false;
		});
		return hotkeys;
	}

	render() {
		const { disabled } = this.props;
		const stepColor = disabled ? "grey" : "#2ba5de";
		return (
			<Hotkeys
				keyName={this.hotkeys()}
				filter={(event) => {
					return true;
				}}
				allowRepeat={false}
				onKeyDown={(hotkey, e, handle) => this.hotkeyPressed(hotkey)}
			>
				<Box>
					<Tooltip
						title={"Step into (" + this.hotkeyFor("stepInto") + ")"}
						placement="top"
					>
						<span>
							<IconButton
								style={{ color: stepColor }}
								onClick={this.stepIntoClicked}
								size="medium"
								disabled={disabled}
							>
								<Icon icon={StepIntoIcon} />
							</IconButton>
						</span>
					</Tooltip>
					<Tooltip
						title={"Step over (" + this.hotkeyFor("stepOver") + ")"}
						placement="top"
					>
						<span>
							<IconButton
								style={{ color: stepColor }}
								onClick={this.stepOverClicked}
								size="medium"
								disabled={disabled}
							>
								<Icon icon={StepOverIcon} />
							</IconButton>
						</span>
					</Tooltip>
					<Tooltip
						title={
							"Step through (" +
							this.hotkeyFor("stepThrought") +
							")"
						}
						placement="top"
					>
						<span>
							<IconButton
								style={{ color: stepColor }}
								onClick={this.stepThroughClicked}
								size="medium"
								disabled={disabled}
							>
								<StepThroughIcon />
							</IconButton>
						</span>
					</Tooltip>
					<Tooltip
						title={"Restart (" + this.hotkeyFor("restart") + ")"}
						placement="top"
					>
						<span>
							<IconButton
								style={{ color: stepColor }}
								onClick={this.restartClicked}
								size="medium"
								disabled={disabled}
							>
								<Icon icon={RestartIcon} />
							</IconButton>
						</span>
					</Tooltip>
					<Tooltip
						title={"Resume (" + this.hotkeyFor("resume") + ")"}
						placement="top"
					>
						<span>
							<IconButton
								style={{ color: disabled ? "grey" : "#3bba5d" }}
								onClick={this.resumeClicked}
								size="medium"
								disabled={disabled}
							>
								<Icon icon={ResumeIcon} />
							</IconButton>
						</span>
					</Tooltip>
					<Tooltip title="Terminate" placement="top">
						<span>
							<IconButton
								style={{ color: disabled ? "grey" : "#ba4343" }}
								onClick={this.terminateClicked}
								size="medium"
								disabled={disabled}
							>
								<Icon icon={TerminateIcon} />
							</IconButton>
						</span>
					</Tooltip>
				</Box>
			</Hotkeys>
		);
	}
}

export default DebuggerControls;
