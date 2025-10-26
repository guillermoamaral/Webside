import { Component } from "react";
import { Box, IconButton } from "@mui/material";
import MinimizeIcon from "@mui/icons-material/Minimize";
import FullscreenExitIcon from "@mui/icons-material/FullscreenExit";
import CropSquareIcon from "@mui/icons-material/CropSquare";
import CloseIcon from "@mui/icons-material/Close";

class WindowControls extends Component {
    constructor(props) {
        super(props);
        this.state = {
            maximized: false,
        };
    }

    handleMinimize = async () => {
        try {
            const { ipcRenderer } = window.require("electron");
            await ipcRenderer.invoke("minimize-window");
        } catch (error) {
            console.log("Not in Electron environment");
        }
    };

    handleMaximize = async () => {
        try {
            const { ipcRenderer } = window.require("electron");
            await ipcRenderer.invoke("maximize-window");
            this.setState({ maximized: true });
        } catch (error) {
            console.log("Not in Electron environment");
        }
    };

    handleRestore = async () => {
        try {
            const { ipcRenderer } = window.require("electron");
            await ipcRenderer.invoke("restore-window");
            this.setState({ maximized: false });
        } catch (error) {
            console.log("Not in Electron environment");
        }
    };

    handleClose = async () => {
        try {
            const { ipcRenderer } = window.require("electron");
            await ipcRenderer.invoke("close-window");
        } catch (error) {
            console.log("Not in Electron environment");
        }
    };

    render() {
        const { maximized } = this.state;
        return (
            <Box
                sx={{
                    display: "flex",
                    alignItems: "center",
                    height: "40px",
                    justifyContent: "flex-end",
                    marginLeft: "auto",
                    marginRight: "5px",
                    WebkitAppRegion: "no-drag",
                    appRegion: "no-drag",
                }}
            >
                <IconButton
                    onClick={this.handleMinimize}
                    sx={{
                        width: 32,
                        height: 32,
                        minWidth: 32,
                        p: 0,
                        borderRadius: 0,
                    }}
                >
                    <MinimizeIcon fontSize="small" />
                </IconButton>
                <IconButton
                    onClick={
                        maximized ? this.handleRestore : this.handleMaximize
                    }
                    sx={{
                        width: 32,
                        height: 32,
                        minWidth: 32,
                        p: 0,
                        borderRadius: 0,
                    }}
                >
                    {maximized ? (
                        <FullscreenExitIcon fontSize="small" />
                    ) : (
                        <CropSquareIcon fontSize="small" />
                    )}
                </IconButton>
                <IconButton
                    onClick={this.handleClose}
                    sx={{
                        width: 32,
                        height: 32,
                        minWidth: 32,
                        p: 0,
                        borderRadius: 0,
                        "&:hover": { backgroundColor: "error.main" },
                    }}
                >
                    <CloseIcon fontSize="small" />
                </IconButton>
            </Box>
        );
    }
}

export default WindowControls;
