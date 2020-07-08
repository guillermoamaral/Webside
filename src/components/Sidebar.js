import React, { Component } from "react";
import {
    Drawer,
    List,
    ListItemIcon,
    ListItem,
    ListItemText,
    Divider,
    IconButton
} from '@material-ui/core';
import ChevronLeftIcon from '@material-ui/icons/ChevronLeft';
import AddIcon from '@material-ui/icons/Add';
import TranscriptIcon from '@material-ui/icons/CallToAction';
import ClassBrowserIcon from '@material-ui/icons/AccountTree';
import MethodBrowserIcon from '@material-ui/icons/Reorder';
import WorkspaceIcon from '@material-ui/icons/Code';
import PeopleIcon from '@material-ui/icons/People';
import GitHubIcon from '@material-ui/icons/GitHub';
import SettingsIcon from '@material-ui/icons/Settings';
import clsx from 'clsx';

class Sidebar extends Component {
    render() {
        return (
            <Drawer
                variant="permanent"
                classes={{paper: clsx(this.props.classes.drawerPaper, !this.props.expanded && this.props.classes.drawerPaperClose)}}
                open={this.props.expanded}>
                <div className={this.props.classes.toolbarIcon}>
                    <IconButton onClick={this.props.onClose.bind(this)}>
                        <ChevronLeftIcon />
                    </IconButton>
                </div>
                <Divider />
                <List>
                    <ListItem button>
                        <ListItemIcon>
                            <AddIcon />
                        </ListItemIcon>
                        <ListItemText primary="New" />
                    </ListItem>                    
                    <ListItem button>
                        <ListItemIcon>
                            <TranscriptIcon className={this.props.classes.transcriptIcon} />
                        </ListItemIcon>
                        <ListItemText primary="Transcript" />
                    </ListItem>
                    <ListItem button>
                        <ListItemIcon>
                            <ClassBrowserIcon className={this.props.classes.classBrowserIcon} />
                        </ListItemIcon>
                        <ListItemText primary="New class Browser" />
                    </ListItem>
                    <ListItem button>
                        <ListItemIcon>
                            <MethodBrowserIcon className={this.props.classes.methodBrowserIcon} />
                        </ListItemIcon>
                        <ListItemText primary="New class Browser" />
                    </ListItem>
                    <ListItem button>
                        <ListItemIcon>
                            <WorkspaceIcon className={this.props.classes.workspaceIcon} />
                        </ListItemIcon>
                        <ListItemText primary="New workspace" />
                    </ListItem>
                    <ListItem button>
                        <ListItemIcon>
                            <PeopleIcon />
                        </ListItemIcon>
                        <ListItemText primary="People" />
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