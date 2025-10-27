import React from "react";
import { Box, Typography } from "@mui/material";

class ShortcutsLegend extends React.Component {
    constructor(props) {
        super(props);
    }

    getShortcuts() {
        const shortcuts = this.props.settings?.section("shortcuts");
        if (!shortcuts) return [];

        return [
            {
                action: "Search",
                shortcut: shortcuts.get("quickSearch"),
                description: "Open search",
            },
            {
                action: "Open Class Browser",
                shortcut: shortcuts.get("openClassBrowser"),
                description: "Open class browser",
            },
            {
                action: "Open System Browser",
                shortcut: shortcuts.get("openSystemBrowser"),
                description: "Open system browser",
            },
            {
                action: "New Workspace",
                shortcut: shortcuts.get("newWorkspace"),
                description: "Open new workspace",
            },
        ];
    }

    render() {
        const shortcuts = this.getShortcuts();
        const isDarkMode =
            this.props.settings?.section("appearance")?.get("mode") === "dark";

        const textColor = isDarkMode ? "#aaaaaa" : "#000000";
        const shortcutColor = isDarkMode ? "#808080" : "#666666";

        return (
            <Box
                sx={{
                    position: "absolute",
                    top: "50%",
                    left: "50%",
                    transform: "translate(-50%, -50%)",
                    textAlign: "center",
                    pointerEvents: "none",
                    zIndex: 1,
                    opacity: 0.7,
                    userSelect: "none",
                }}
            >
                {shortcuts.map((item, index) => (
                    <Box
                        key={index}
                        sx={{
                            marginBottom: 1,
                            display: "flex",
                            alignItems: "center",
                            justifyContent: "center",
                            gap: 2,
                        }}
                    >
                        <Typography
                            variant="body1"
                            sx={{
                                color: textColor,
                                fontWeight: 500,
                                fontSize: "14px",
                            }}
                        >
                            {item.action}
                        </Typography>
                        <Typography
                            variant="body2"
                            sx={{
                                color: shortcutColor,
                                fontSize: "12px",
                                fontFamily: "monospace",
                                backgroundColor: isDarkMode
                                    ? "rgba(255,255,255,0.1)"
                                    : "rgba(0,0,0,0.1)",
                                padding: "2px 6px",
                                borderRadius: "3px",
                                border: `1px solid ${
                                    isDarkMode
                                        ? "rgba(255,255,255,0.2)"
                                        : "rgba(0,0,0,0.2)"
                                }`,
                            }}
                        >
                            {item.shortcut}
                        </Typography>
                    </Box>
                ))}
            </Box>
        );
    }
}

export default ShortcutsLegend;
