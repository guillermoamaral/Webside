import React, { Component } from "react";
import { Box, TextField, ToggleButton, Typography } from "@mui/material";

class ShortcutEditor extends Component {
    constructor(props) {
        super(props);
        this.state = {
            shift: false,
            ctrl: false,
            alt: false,
            key: "",
        };
    }

    static getDerivedStateFromProps(props, state) {
        const shortcut = props.value;
        if (!shortcut) return null;
        const parts = shortcut.split("+");
        return {
            shift: parts.includes("Shift"),
            ctrl: parts.includes("Ctrl"),
            alt: parts.includes("Alt"),
            key: parts[parts.length - 1],
        };
    }

    keyChanged(event) {
        const key = event.target.value;
        if (!key || key.length === 0) return;
        this.setState({ key: key.slice(key.length - 1, key.length) });
    }

    setShift(value) {
        const { ctrl, alt, key } = this.state;
        this.setShortcut(value, ctrl, alt, key);
    }

    setCtrl(value) {
        const { shift, alt, key } = this.state;
        this.setShortcut(shift, value, alt, key);
    }

    setAlt(value) {
        const { shift, ctrl, key } = this.state;
        this.setShortcut(shift, ctrl, value, key);
    }

    setShortcut(shift, ctrl, alt, key) {
        if (this.props.onChange) {
            const shortcut = this.shortcut(shift, ctrl, alt, key);
            return this.props.onChange(shortcut);
        }
        this.setState({
            shift: shift,
            ctrl: ctrl,
            alt: alt,
            key: key,
        });
    }

    keyDown = (event) => {
        event.preventDefault();
        const key = event.key;
        if (key === "Shift") {
            return this.setShift(true);
        }
        if (key === "Control") {
            return this.setCtrl(true);
        }
        if (key === "Alt") {
            return this.setAlt(true);
        }
        this.setShortcut(
            event.shiftKey,
            event.ctrlKey,
            event.altKey,
            event.key.toLowerCase()
        );
    };

    shortcut = (shift, ctrl, alt, key) => {
        return key
            ? (shift ? "Shift+" : "") +
                  (ctrl ? "Ctrl+" : "") +
                  (alt ? "Alt+" : "") +
                  key
            : "";
    };

    render() {
        const { shift, ctrl, alt, key } = this.state;
        return (
            <Box 
                display="flex" 
                flexDirection="row" 
                alignItems="center"
                justifyContent="space-between"
                sx={{ width: "100%" }}
            >
                <Box
                    display="flex"
                    alignItems="center"
                    justifyContent="flex-start"
                >
                    <ToggleButton
                        selected={shift}
                        onChange={() => this.setShift(!shift)}
                        size="small"
                        value="shift"
                    >
                        Shift
                    </ToggleButton>
                    +
                    <ToggleButton
                        selected={ctrl}
                        onChange={() => this.setCtrl(!ctrl)}
                        size="small"
                        value="ctrl"
                    >
                        Ctrl
                    </ToggleButton>
                    +
                    <ToggleButton
                        selected={alt}
                        onChange={() => this.setAlt(!alt)}
                        size="small"
                        value="alt"
                    >
                        Alt
                    </ToggleButton>
                    +
                    <TextField
                        sx={{ width: 100 }}
                        size="small"
                        id="key"
                        type="text"
                        placeholder="key"
                        name="key"
                        variant="outlined"
                        value={key}
                        // onChange={(event) => {
                        // 	event.preventDefault();
                        // 	this.keyChanged(event);
                        // }}
                        onKeyDown={this.keyDown}
                        required
                    />
                </Box>
                <Box
                    display="flex"
                    alignItems="center"
                    justifyContent="flex-end"
                >
                    <Typography variant="body2" sx={{ ml: 2, opacity: 0.6 }}>
                        {"(" + this.shortcut(shift, ctrl, alt, key) + ")"}
                    </Typography>
                </Box>
            </Box>
        );
    }
}

export default ShortcutEditor;
