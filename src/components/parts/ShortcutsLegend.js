import React from "react";
import { Box, Typography } from "@mui/material";
import { withTheme } from "@emotion/react";
import ShortcutLegend from "./ShortcutLegend";

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
                                fontWeight: 500,
                                fontSize: "14px",
                            }}
                        >
                            {item.action}
                        </Typography>
                        <ShortcutLegend shortcut={item.shortcut} />
                    </Box>
                ))}
            </Box>
        );
    }
}

export default withTheme(ShortcutsLegend);
