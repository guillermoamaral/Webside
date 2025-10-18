import React, { Component } from "react";
import {
    Link,
    Card,
    CardContent,
    CardActions,
    Button,
    Typography,
    Paper,
    IconButton,
    Box,
} from "@mui/material";
import { Close as CloseIcon, PushPin as PinIcon } from "@mui/icons-material";
import { ide } from "../IDE";
import Inspector from "../tools/Inspector";
import CodeEditorBackend from "./CodeEditorBackend";

class CodeTooltip extends Component {
    constructor(props) {
        super(props);
        this.inspectorRef = React.createRef();
        this.pinned = false;
    }

    aboutToClose() {
        if (this.pinned) return;
        if (this.inspectorRef && this.inspectorRef.current) {
            this.inspectorRef.current.aboutToClose();
        }
    }

    handlePin = () => {
        if (this.props.object) {
            this.pinned = true;
            ide.inspectObject(this.props.object);
        }
    };

    render() {
        const appearance = ide.settings.section("appearance");
        const mode = appearance.section(appearance.get("mode"));
        const color = mode.get("primaryColor");
        const background = mode.get("background");
        const {
            title,
            titleAction,
            description,
            code,
            object,
            actions,
            inspectorRef,
            onClose,
        } = this.props;
        return (
            <Card
                sx={{
                    minWidth: 200,
                    maxWidth: 600,
                    border: 1,
                    borderColor: "grey.500",
                    background: background,
                    position: "relative",
                }}
            >
                {(onClose || object) && (
                    <Box
                        sx={{
                            position: "absolute",
                            top: 8,
                            right: 8,
                            zIndex: 1,
                            display: "flex",
                            gap: 0.5,
                        }}
                    >
                        {object && (
                            <IconButton
                                size="small"
                                onClick={this.handlePin}
                                sx={{
                                    color: mode.get("primaryText"),
                                    "&:hover": {
                                        backgroundColor: "rgba(0, 0, 0, 0.04)",
                                    },
                                }}
                                title="Pin object"
                            >
                                <PinIcon fontSize="small" />
                            </IconButton>
                        )}
                        {onClose && (
                            <IconButton
                                size="small"
                                onClick={onClose}
                                sx={{
                                    color: mode.get("primaryText"),
                                    "&:hover": {
                                        backgroundColor: "rgba(0, 0, 0, 0.04)",
                                    },
                                }}
                                title="Close"
                            >
                                <CloseIcon fontSize="small" />
                            </IconButton>
                        )}
                    </Box>
                )}
                <CardContent>
                    {titleAction ? (
                        <Link
                            component="button"
                            variant="body1"
                            onClick={(e) => {
                                if (titleAction) titleAction(title);
                            }}
                            color={color}
                        >
                            {title}
                        </Link>
                    ) : (
                        <Typography variant="body1" color={color}>
                            {title}
                        </Typography>
                    )}
                    {description && (
                        <Typography
                            variant="body2"
                            color={mode.get("primaryText")}
                        >
                            {description || ""}
                        </Typography>
                    )}
                    {code && (
                        <Paper
                            variant="outlined"
                            sx={{
                                minWidth: 400,
                                width: "100%",
                                height: 150,
                            }}
                        >
                            <CodeEditorBackend
                                source={code}
                                readOnly
                                noTooltips
                            />
                        </Paper>
                    )}
                    {object && (
                        <Paper
                            variant="outlined"
                            sx={{
                                minWidth: 500,
                                width: "100%",
                                height: 400,
                            }}
                        >
                            <Inspector
                                key={object.id}
                                root={object}
                                showWorkspace={false}
                                embedded={true}
                                ref={this.inspectorRef}
                            />
                        </Paper>
                    )}
                </CardContent>
                {actions && (
                    <CardActions>
                        {actions.map((action, i) => {
                            return (
                                <Button
                                    size="small"
                                    sx={{
                                        textTransform: "none",
                                        color: color,
                                    }}
                                    key={"tipAction" + i}
                                    onClick={() => {
                                        console.log("dale");
                                        action.handler(title);
                                    }}
                                >
                                    {action.label}
                                </Button>
                            );
                        })}
                    </CardActions>
                )}
            </Card>
        );
    }
}

export default CodeTooltip;
