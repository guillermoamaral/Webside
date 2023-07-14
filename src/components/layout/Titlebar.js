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
import { ide } from "../IDE";
import StyledAppBar from "./StyledAppBar";
import SearchField from "../controls/SearchField";
import LightModeIcon from "@mui/icons-material/LightModeRounded";
import DarkModeIcon from "@mui/icons-material/DarkModeRounded";

class Titlebar extends Component {
	search = async (text) => {
		if (text && text.length > 0) {
			try {
				await ide.backend.classNamed(text);
				ide.browseClass(text);
			} catch (error) {}
		}
	};

	render() {
		const {
			dialect,
			backend,
			developer,
			sidebarExpanded,
			onSidebarExpand,
			onUserClick,
			colorMode,
			onColorModeToggle,
		} = this.props;
		let logo;
		if (dialect) {
			try {
				logo = require("../../resources/" + dialect + ".png");
			} catch (error) {}
		}
		return (
			<StyledAppBar
				color="primary"
				position="fixed"
				open={sidebarExpanded}
				enableColorOnDark
			>
				<Toolbar>
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
									src={logo}
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
						{dialect + " Web IDE "}
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
					<SearchField onSearch={(text) => this.search(text)} />
					<Box flexGrow={1} />
					<IconButton color="inherit" onClick={onColorModeToggle}>
						{colorMode === "dark" ? (
							<LightModeIcon />
						) : (
							<DarkModeIcon />
						)}
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
