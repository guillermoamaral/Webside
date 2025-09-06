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

class Titlebar extends Component {
	constructor(props) {
		super(props);
		this.state = { developerMenuOpen: false };
	}

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
		return (
			<StyledAppBar
				color="primary"
				position="fixed"
				open={sidebarExpanded}
				enableColorOnDark
			>
				<Toolbar variant="dense">
					<IconButton
						edge="start"
						color="inherit"
						onClick={onSidebarExpand}
						sx={{
							mr: 5,
							...(sidebarExpanded && { display: "none" }),
						}}
					>
						<MenuIcon />
					</IconButton>
					<Box p={1}>
						{logo && (
							<Link to={backend}>
								<img
									src={"data:image/png;base64," + logo}
									width={28}
									height={28}
									alt={dialect}
								/>
							</Link>
						)}
					</Box>
					<Box display="flex" flexDirection="row" alignItems="center">
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
								variant="caption"
								color="inherit"
								display="inline"
								noWrap
								ml={1}
							>
								{`(version ${version})`}
							</Typography>
						)}
					</Box>
					<Box p={1}>
						<Link
							href="https://github.com/guillermoamaral/Webside"
							variant="body2"
							color="inherit"
							target="_blank"
						>
							{"(Powered by Webside)"}
						</Link>
					</Box>
					<Box flexGrow={1} />
					<SearchField
						onClick={onSearchClick}
						placeholder={searchPlaceholder}
					/>
					<Box flexGrow={1} />
					<IconButton color="inherit" onClick={onColorModeToggle}>
						{colorMode === "dark" ? (
							<LightModeIcon />
						) : (
							<DarkModeIcon />
						)}
					</IconButton>
					<IconButton
						color="inherit"
						onClick={(event) => {
							onSplit();
						}}
						disabled={!onSplit}
					>
						<SplitIcon />
					</IconButton>
					{onAssistantClick && (
						<IconButton
							color="inherit"
							onClick={(event) => {
								onAssistantClick();
							}}
						>
							<AssistantIcon />
						</IconButton>
					)}
					<Box sx={{ flexGrow: 0 }}>
						<IconButton
							id="developer"
							color="primary"
							onClick={() => {
								this.setState({
									developerMenuOpen: true,
								});
							}}
						>
							<Avatar alt={developer} src={photo} />
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
					<Box p={1}>
						<Typography
							variant="subtitle1"
							gutterBottom
							color="inherit"
							noWrap
						>
							{developer}
						</Typography>
					</Box>
				</Toolbar>
			</StyledAppBar>
		);
	}
}

export default Titlebar;
