import React, { Component } from "react";
import {
	AppBar,
	Toolbar,
	Avatar,
	Typography,
	IconButton,
	Link,
	InputBase,
	Box,
} from "@material-ui/core";
import clsx from "clsx";
import MenuIcon from "@material-ui/icons/Menu";
import SearchIcon from "@material-ui/icons/Search";

class Titlebar extends Component {
	render() {
		const { dialect, baseUri, developer } = this.props;
		let logo;
		if (dialect) {
			try {
				logo = require("../../resources/" + dialect + ".png");
			} catch (error) {}
		}
		const styles = this.props.styles;
		return (
			<AppBar
				color="primary"
				position="absolute"
				className={clsx(
					styles.appBar,
					this.props.sidebarExpanded && styles.appBarShift
				)}
			>
				<Toolbar className={styles.toolbar}>
					<IconButton
						edge="start"
						color="inherit"
						onClick={this.props.expandSidebar.bind(this)}
						className={clsx(
							styles.menuButton,
							this.props.sidebarExpanded && styles.menuButtonHidden
						)}
					>
						<MenuIcon />
					</IconButton>
					<Box p={1}>
						{logo && (
							<Link to={baseUri}>
								<img src={logo} width={28} height={28} alt={dialect} />
							</Link>
						)}
					</Box>
					<Typography variant="h6" color="inherit" display="inline" noWrap>
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
					<div className={styles.globalSearch}>
						<div className={styles.globalSearchIcon}>
							<SearchIcon />
						</div>
						<InputBase
							placeholder="Search…"
							classes={{
								root: styles.globalSearchInputRoot,
								input: styles.globalSearchInputInput,
							}}
							inputProps={{ "aria-label": "search" }}
						/>
					</div>
					<Box flexGrow={1} />
					<IconButton
						color="primary"
						component="span"
						onClick={(event) => this.props.onAvatarClicked()}
					>
						<Avatar alt={developer} />
					</IconButton>
					<Box p={1}>
						<Typography variant="subtitle1" gutterBottom color="inherit" noWrap>
							{developer}
						</Typography>
					</Box>
				</Toolbar>
			</AppBar>
		);
	}
}

export default Titlebar;
