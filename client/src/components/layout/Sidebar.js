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
import clsx from 'clsx';
import ChevronLeftIcon from '@material-ui/icons/ChevronLeft';
import PeopleIcon from '@material-ui/icons/People';
import GitHubIcon from '@material-ui/icons/GitHub';
import SettingsIcon from '@material-ui/icons/Settings';
import TranscriptIcon from '../icons/TranscriptIcon';
import ChangesBrowserIcon from '../icons/ChangesBrowserIcon';
import InspectorIcon from '../icons/InspectorIcon';
import { withDialog } from '../dialogs/index';

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
                        <ChevronLeftIcon/>
                    </IconButton>
                </div>
                <Divider/>
                <List>
                    <ListItem button onClick={this.props.onTranscriptClicked}>
                        <ListItemIcon>
                            <Badge badgeContent={this.props.unreadErrorsCount} color="secondary">
                                <TranscriptIcon/>
                            </Badge>
                        </ListItemIcon>
                        <ListItemText primary="Transcript"/>
                    </ListItem>
                    <ListItem button onClick={e => this.props.onChangesClicked()}>
                        <ListItemIcon>                                
                            <Badge badgeContent={this.props.changesCount} color="secondary">
                                <ChangesBrowserIcon/>
                            </Badge>
                        </ListItemIcon>
                        <ListItemText primary="Changes"/>
                    </ListItem>
                    <ListItem button onClick={e => this.props.onPinnedObjectsClicked()}>
                        <ListItemIcon>                                
                            <InspectorIcon/>
                        </ListItemIcon>
                        <ListItemText primary="Pinned Objects"/>
                    </ListItem>              
                    <ListItem button onClick={e => this.props.onPeersClicked()}>
                        <ListItemIcon>
                            <Badge badgeContent={this.props.unreadMessages} color="secondary">
                                <PeopleIcon/>
                            </Badge>
                        </ListItemIcon>
                        <ListItemText primary="Peers"/>
                    </ListItem>
                    <ListItem button onClick={e => this.props.dialog.alert('Not implemented yet!')}>
                        <ListItemIcon>
                            <GitHubIcon/>
                        </ListItemIcon>
                        <ListItemText primary="GitHub"/>
                    </ListItem>
                    <ListItem button onClick={e => this.props.onSettingsClicked()}>
                        <ListItemIcon>
                            <SettingsIcon/>
                      </ListItemIcon>
                      <ListItemText primary="Settings"/>
                    </ListItem>
                </List>
                <Divider />
            </Drawer>
            )
    }
}

export default withDialog()(Sidebar);