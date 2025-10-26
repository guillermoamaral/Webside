import { Component } from "react";
import {
    Toolbar,
    Avatar,
    Typography,
    IconButton,
    Link,
    Box,
    Menu,
    MenuItem,
} from "@mui/material";
import MenuIcon from "@mui/icons-material/Menu";
import StyledAppBar from "./StyledAppBar";
import SearchField from "../controls/SearchField";
import LightModeIcon from "@mui/icons-material/LightModeRounded";
import DarkModeIcon from "@mui/icons-material/DarkModeRounded";
import SplitIcon from "../icons/SplitIcon";
import AssistantIcon from "@mui/icons-material/Assistant";
import WindowControls from "./WindowControls";

class Titlebar extends Component {
    constructor(props) {
        super(props);
        this.state = {
            developerMenuOpen: false,
        };
    }

    isElectron = () => {
        return (
            !!(window.require && window.require("electron")) ||
            !!(
                window.process &&
                window.process.versions &&
                window.process.versions.electron
            )
        );
    };

    render() {
        const {
            dialect,
            version,
            logo,
            backend,
            developer,
            sidebarExpanded,
            onSidebarExpand,
            onSettingsClick,
            onDisconnectClick,
            colorMode,
            onColorModeToggle,
            onSearchClick,
            searchPlaceholder,
            onSplit,
            onAssistantClick,
            onReleaseNotesClick,
            photo,
        } = this.props;
        const developerMenuOpen = this.state.developerMenuOpen;
        const electron = this.isElectron();
        return (
            <StyledAppBar
                color="primary"
                position="fixed"
                open={sidebarExpanded}
                enableColorOnDark
                isElectron={electron}
            >
                <Toolbar
                    variant="dense"
                    sx={{
                        py: 0.75,
                        px: 1,
                    }}
                >
                    {onSidebarExpand && (
                        <IconButton
                            edge="start"
                            color="inherit"
                            onClick={onSidebarExpand}
                            sx={{
                                mr: 2,
                                ...(sidebarExpanded && { display: "none" }),
                                ...(electron && {
                                    WebkitAppRegion: "no-drag",
                                    appRegion: "no-drag",
                                }),
                            }}
                        >
                            <MenuIcon />
                        </IconButton>
                    )}
                    <Box px={0.25} display="flex" alignItems="center">
                        {logo && (
                            <Link to={backend}>
                                <img
                                    src={"data:image/png;base64," + logo}
                                    width={24}
                                    height={24}
                                    alt={dialect}
                                    style={{ verticalAlign: "middle" }}
                                />
                            </Link>
                        )}
                    </Box>
                    {dialect && (
                        <Box
                            display="flex"
                            flexDirection="row"
                            alignItems="baseline"
                        >
                            <Typography
                                variant="h6"
                                color="inherit"
                                display="inline"
                                noWrap
                            >
                                {dialect}
                            </Typography>
                            {version && version !== "" && (
                                <Typography
                                    variant="body2"
                                    color="inherit"
                                    display="inline"
                                    noWrap
                                    ml={1}
                                >
                                    {`v${version}`}
                                </Typography>
                            )}

                            <Link
                                ml={1}
                                href="https://github.com/guillermoamaral/Webside"
                                variant="body2"
                                color="inherit"
                                target="_blank"
                            >
                                {"(Powered by Webside)"}
                            </Link>
                        </Box>
                    )}
                    <Box flexGrow={1} />
                    {onSearchClick && (
                        <Box
                            sx={{
                                width: "300px",
                                mx: 1,
                                ...(electron && {
                                    WebkitAppRegion: "no-drag",
                                    appRegion: "no-drag",
                                }),
                            }}
                        >
                            <SearchField
                                onClick={onSearchClick}
                                placeholder={searchPlaceholder}
                            />
                        </Box>
                    )}
                    <Box flexGrow={1} />
                    {onColorModeToggle && (
                        <IconButton
                            color="inherit"
                            onClick={onColorModeToggle}
                            size="small"
                            sx={
                                electron
                                    ? {
                                          WebkitAppRegion: "no-drag",
                                          appRegion: "no-drag",
                                      }
                                    : {}
                            }
                        >
                            {colorMode === "dark" ? (
                                <LightModeIcon />
                            ) : (
                                <DarkModeIcon />
                            )}
                        </IconButton>
                    )}
                    {onSplit && (
                        <IconButton
                            color="inherit"
                            onClick={(event) => {
                                onSplit();
                            }}
                            disabled={!onSplit}
                            size="small"
                            sx={
                                electron
                                    ? {
                                          WebkitAppRegion: "no-drag",
                                          appRegion: "no-drag",
                                      }
                                    : {}
                            }
                        >
                            <SplitIcon />
                        </IconButton>
                    )}
                    {onAssistantClick && (
                        <IconButton
                            color="inherit"
                            onClick={(event) => {
                                onAssistantClick();
                            }}
                            size="small"
                            sx={
                                electron
                                    ? {
                                          WebkitAppRegion: "no-drag",
                                          appRegion: "no-drag",
                                      }
                                    : {}
                            }
                        >
                            <AssistantIcon />
                        </IconButton>
                    )}
                    {developer && (
                        <Box px={1}>
                            <Typography
                                variant="subtitle1"
                                color="inherit"
                                noWrap
                            >
                                {developer}
                            </Typography>
                        </Box>
                    )}
                    {developer && (
                        <Box sx={{ flexGrow: 0 }}>
                            <IconButton
                                id="developer"
                                color="primary"
                                onClick={() => {
                                    this.setState({
                                        developerMenuOpen: true,
                                    });
                                }}
                                size="small"
                                sx={
                                    electron
                                        ? {
                                              WebkitAppRegion: "no-drag",
                                              appRegion: "no-drag",
                                          }
                                        : {}
                                }
                            >
                                <Avatar
                                    alt={developer}
                                    src={photo}
                                    sx={{ width: 24, height: 24 }}
                                />
                            </IconButton>
                            <Menu
                                sx={{ mt: "45px" }}
                                id="menu-appbar"
                                anchorEl={document.getElementById("developer")}
                                anchorOrigin={{
                                    vertical: "top",
                                    horizontal: "right",
                                }}
                                keepMounted
                                transformOrigin={{
                                    vertical: "top",
                                    horizontal: "right",
                                }}
                                open={developerMenuOpen}
                                onClose={() => {
                                    this.setState({
                                        developerMenuOpen: false,
                                    });
                                }}
                            >
                                <MenuItem
                                    key="settings"
                                    onClick={() => {
                                        this.setState(
                                            { developerMenuOpen: false },
                                            onSettingsClick
                                        );
                                    }}
                                >
                                    <Typography textAlign="center">
                                        Settings
                                    </Typography>
                                </MenuItem>
                                <MenuItem
                                    key="releaseNotes"
                                    onClick={() => {
                                        this.setState(
                                            { developerMenuOpen: false },
                                            onReleaseNotesClick
                                        );
                                    }}
                                >
                                    <Typography textAlign="center">
                                        Release Notes
                                    </Typography>
                                </MenuItem>
                                <MenuItem
                                    key="disconnect"
                                    onClick={() => {
                                        this.setState(
                                            { developerMenuOpen: false },
                                            onDisconnectClick
                                        );
                                    }}
                                >
                                    <Typography textAlign="center">
                                        Disconnect
                                    </Typography>
                                </MenuItem>
                            </Menu>
                        </Box>
                    )}
                    {electron && <WindowControls />}
                </Toolbar>
            </StyledAppBar>
        );
    }
}

export default Titlebar;
