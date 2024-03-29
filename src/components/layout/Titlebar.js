import React, { Component } from "react";
import {
	Toolbar,
	Avatar,
	Typography,
	IconButton,
	Link,
	Box,
} from "@mui/material";
import MenuIcon from "@mui/icons-material/Menu";
import StyledAppBar from "./StyledAppBar";
import SearchField from "../controls/SearchField";
import LightModeIcon from "@mui/icons-material/LightModeRounded";
import DarkModeIcon from "@mui/icons-material/DarkModeRounded";
import SplitIcon from "../icons/SplitIcon";

class Titlebar extends Component {
	render() {
		const {
			dialect,
			logo,
			backend,
			developer,
			sidebarExpanded,
			onSidebarExpand,
			onUserClick,
			colorMode,
			onColorModeToggle,
			onSearchClick,
			searchPlaceholder,
			onSplit,
		} = this.props;
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
					<Typography
						variant="h6"
						color="inherit"
						display="inline"
						noWrap
					>
						{dialect}
					</Typography>
					<Box p={1}>
						<Link
							href="https://github.com/guillermoamaral/Webside"
							variant="body2"
							color="inherit"
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
					<IconButton color="primary" onClick={onUserClick}>
						<Avatar alt={developer} />
					</IconButton>
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
