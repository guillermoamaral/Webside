import React, { Component } from "react";
import {
    Drawer,
    List,
    ListItemIcon,
    ListItem,
    ListItemText,
    Divider,
    IconButton,
    Badge
} from '@material-ui/core';
import ChevronLeftIcon from '@material-ui/icons/ChevronLeft';
import PeopleIcon from '@material-ui/icons/People';
import GitHubIcon from '@material-ui/icons/GitHub';
import SettingsIcon from '@material-ui/icons/Settings';
import clsx from 'clsx';
import TranscriptIcon from '../icons/TranscriptIcon';
import ChangesBrowserIcon from '../icons/ChangesBrowserIcon';

class Sidebar extends Component {
    render() {
        const styles = this.props.styles;
        return (
            <Drawer
                variant="permanent"
                classes={{paper: clsx(styles.drawerPaper, !this.props.expanded && styles.drawerPaperClose)}}
                open={this.props.expanded}>
                <div className={styles.toolbarIcon}>
                    <IconButton onClick={this.props.onClose}>
                        <ChevronLeftIcon />
                    </IconButton>
                </div>
                <Divider />
                <List>
                    <ListItem button onClick={this.props.onTranscriptClicked}>
                        <ListItemIcon>
                            <TranscriptIcon />
                        </ListItemIcon>
                        <ListItemText primary="Transcript" />
                    </ListItem>
                    <ListItem button onClick={this.props.onChangesClicked}>
                        <ListItemIcon>                                
                            <Badge badgeContent={this.props.changesCount} color="secondary">
                                <ChangesBrowserIcon />
                            </Badge>
                        </ListItemIcon>
                        <ListItemText primary="Changes" />
                    </ListItem>                    
                    <ListItem button onClick={this.props.onPeersClicked}>
                        <ListItemIcon>
                            <PeopleIcon />
                        </ListItemIcon>
                        <ListItemText primary="Peers" />
                    </ListItem>
                    <ListItem button>
                        <ListItemIcon>
                            <GitHubIcon />
                        </ListItemIcon>
                        <ListItemText primary="GitHub" />
                    </ListItem>
                    <ListItem button>
                        <ListItemIcon>
                            <SettingsIcon />
                      </ListItemIcon>
                      <ListItemText primary="Settings" />
                    </ListItem>
                </List>
                <Divider />
            </Drawer>
            )
    }
}

export default Sidebar;