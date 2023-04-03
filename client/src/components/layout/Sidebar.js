import React, { Component } from "react";
import {
	Drawer,
	List,
	ListItemIcon,
	ListItem,
	ListItemText,
	Divider,
	IconButton,
	Badge,
	Tooltip,
} from "@mui/material";
import clsx from "clsx";
import ChevronLeftIcon from "@mui/icons-material/ChevronLeft";
import TranscriptIcon from "../icons/TranscriptIcon";
import SearchIcon from "@mui/icons-material/Search";
import ResourcesIcon from "@mui/icons-material/PinDropRounded";
import ChangesBrowserIcon from "../icons/ChangesBrowserIcon";
import PeopleIcon from "@mui/icons-material/People";
import SaveImageIcon from "@mui/icons-material/CameraAlt";
import { withDialog } from "../dialogs/index";

class Sidebar extends Component {
	render() {
		const styles = this.props.styles;
		return (
			<Drawer
				variant="permanent"
				classes={{
					paper: clsx(
						styles.drawerPaper,
						!this.props.expanded && styles.drawerPaperClose
					),
				}}
				open={this.props.expanded}
			>
				<div className={styles.toolbarIcon}>
					<IconButton onClick={this.props.onClose}>
						<ChevronLeftIcon />
					</IconButton>
				</div>
				<Divider />
				<List>
					<ListItem button onClick={this.props.onSaveImageClicked}>
						<ListItemIcon>
							<Tooltip title="Save image" placement="top">
								<SaveImageIcon />
							</Tooltip>
						</ListItemIcon>
						<ListItemText primary="Save image" />
					</ListItem>
					<ListItem button onClick={this.props.onTranscriptClicked}>
						<ListItemIcon>
							<Tooltip title="Transcript" placement="top">
								<Badge
									badgeContent={this.props.unreadErrorsCount}
									color="secondary"
								>
									<TranscriptIcon />
								</Badge>
							</Tooltip>
						</ListItemIcon>
						<ListItemText primary="Transcript" />
					</ListItem>
					<ListItem button onClick={this.props.onSearchClicked}>
						<ListItemIcon>
							<Tooltip title="Search" placement="top">
								<SearchIcon />
							</Tooltip>
						</ListItemIcon>
						<ListItemText primary="Search" />
					</ListItem>
					<ListItem button onClick={(event) => this.props.onChangesClicked()}>
						<ListItemIcon>
							<Tooltip title="Last Changes" placement="top">
								<Badge badgeContent={this.props.changesCount} color="secondary">
									<ChangesBrowserIcon />
								</Badge>
							</Tooltip>
						</ListItemIcon>
						<ListItemText primary="Changes" />
					</ListItem>
					<ListItem button onClick={(event) => this.props.onResourcesClicked()}>
						<Tooltip title="Resources" placement="top">
							<ListItemIcon>
								<ResourcesIcon />
							</ListItemIcon>
						</Tooltip>
						<ListItemText primary="Resources" />
					</ListItem>
					<ListItem button onClick={(event) => this.props.onPeersClicked()}>
						<ListItemIcon>
							<Tooltip title="Chat" placement="top">
								<Badge
									badgeContent={this.props.unreadMessages}
									color="secondary"
								>
									<PeopleIcon />
								</Badge>
							</Tooltip>
						</ListItemIcon>
						<ListItemText primary="Peers" />
					</ListItem>
				</List>
				<Divider />
			</Drawer>
		);
	}
}

export default withDialog()(Sidebar);
