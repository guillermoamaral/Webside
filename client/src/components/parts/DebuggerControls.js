import React, { PureComponent } from "react";
import { Box, IconButton, Tooltip } from "@material-ui/core";
import { Icon } from "@iconify/react";
import RestartIcon from "@iconify/icons-mdi/replay";
import StepIntoIcon from "@iconify/icons-mdi/debug-step-into";
import StepOverIcon from "@iconify/icons-mdi/debug-step-over";
import StepThroughIcon from "../icons/StepThroughIcon";
import ResumeIcon from "@iconify/icons-mdi/play";
import TerminateIcon from "@iconify/icons-mdi/stop";

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

	render() {
		const { disabled } = this.props;
		const stepColor = disabled ? "grey" : "#2ba5de";
		return (
			<Box>
				<Tooltip title="Step into" placement="top">
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
				<Tooltip title="Step over" placement="top">
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
				<Tooltip title="Step through" placement="top">
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
				<Tooltip title="Restart" placement="top">
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
				<Tooltip title="Resume" placement="top">
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
		);
	}
}

export default DebuggerControls;
