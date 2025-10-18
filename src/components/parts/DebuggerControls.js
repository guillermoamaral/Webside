import React, { PureComponent } from "react";
import { Box, IconButton, Tooltip } from "@mui/material";
import { Icon } from "@iconify/react";
import RestartIcon from "../icons/RestartIcon";
import StepIntoIcon from "../icons/StepIntoIcon";
import StepOverIcon from "../icons/StepOverIcon";
import StepThroughIcon from "../icons/StepThroughIcon";
import ResumeIcon from "@iconify/icons-mdi/play";
import TerminateIcon from "@iconify/icons-mdi/stop";
import Hotkeys from "react-hot-keys";
import { ide } from "../IDE";

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
        const shortcuts = ide.settings.section("shortcuts");
        switch (hotkey) {
            case shortcuts.get("stepInto"):
                this.stepIntoClicked();
                break;
            case shortcuts.get("stepOver"):
                this.stepOverClicked();
                break;
            case shortcuts.get("stepThrough"):
                this.stepThroughClicked();
                break;
            case shortcuts.get("restart"):
                this.restartClicked();
                break;
            case shortcuts.get("resume"):
                this.resumeClicked();
                break;
            case shortcuts.get("terminate"):
                this.terminateClicked();
                break;
            default:
        }
    };

    render() {
        const { disabled } = this.props;
        const stepColor = disabled ? "grey" : "#2ba5de";
        const shortcuts = ide.settings.section("shortcuts");
        return (
            <Hotkeys
                keyName={
                    shortcuts.get("stepInto") +
                    "," +
                    shortcuts.get("stepOver") +
                    "," +
                    shortcuts.get("stepThrough") +
                    "," +
                    shortcuts.get("restart") +
                    "," +
                    shortcuts.get("resume") +
                    "," +
                    shortcuts.get("terminate")
                }
                filter={(event) => {
                    return true;
                }}
                allowRepeat={false}
                onKeyDown={(hotkey, e, handle) => this.hotkeyPressed(hotkey)}
            >
                <Box>
                    <Tooltip
                        title={"Step into (" + shortcuts.get("stepInto") + ")"}
                        placement="top"
                    >
                        <span>
                            <IconButton
                                style={{ color: stepColor }}
                                onClick={this.stepIntoClicked}
                                size="medium"
                                disabled={disabled}
                            >
                                <StepIntoIcon />
                            </IconButton>
                        </span>
                    </Tooltip>
                    <Tooltip
                        title={"Step over (" + shortcuts.get("stepOver") + ")"}
                        placement="top"
                    >
                        <span>
                            <IconButton
                                style={{ color: stepColor }}
                                onClick={this.stepOverClicked}
                                size="medium"
                                disabled={disabled}
                            >
                                <StepOverIcon />
                            </IconButton>
                        </span>
                    </Tooltip>
                    <Tooltip
                        title={
                            "Step through (" +
                            shortcuts.get("stepThrough") +
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
                        title={"Restart (" + shortcuts.get("restart") + ")"}
                        placement="top"
                    >
                        <span>
                            <IconButton
                                style={{ color: stepColor }}
                                onClick={this.restartClicked}
                                size="medium"
                                disabled={disabled}
                            >
                                <RestartIcon />
                            </IconButton>
                        </span>
                    </Tooltip>
                    <Tooltip
                        title={"Resume (" + shortcuts.get("resume") + ")"}
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
                    <Tooltip
                        title={"Terminate (" + shortcuts.get("terminate") + ")"}
                        placement="top"
                    >
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
