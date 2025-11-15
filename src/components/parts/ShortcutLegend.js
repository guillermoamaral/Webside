import React from "react";
import { Box, Typography } from "@mui/material";
import { withTheme } from "@emotion/react";

class ShortcutLegend extends React.Component {
    constructor(props) {
        super(props);
    }

    render() {
        const { theme, shortcut } = this.props;
        const dark = theme.palette.mode === "dark";
        const shortcutColor = theme.palette.text.secondary;
        return (
            <Box>
                <Typography
                    variant="body2"
                    sx={{
                        color: shortcutColor,
                        fontSize: "12px",
                        fontFamily: "monospace",
                        backgroundColor: dark
                            ? "rgba(255,255,255,0.1)"
                            : "rgba(0,0,0,0.1)",
                        padding: "2px 6px",
                        borderRadius: "3px",
                    }}
                >
                    {shortcut}
                </Typography>
            </Box>
        );
    }
}

export default withTheme(ShortcutLegend);
