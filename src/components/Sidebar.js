import React, { Component } from "react";
import {
    Drawer,
    List,
    ListItemIcon,
    ListItem,
    ListItemText,
    Divider,
    IconButton
} from "@material-ui/core";
import ChevronLeftIcon from "@material-ui/icons/ChevronLeft";
import ClassBrowserIcon from "@material-ui/icons/AccountTree";
import WorkspaceIcon from "@material-ui/icons/Code";
import PeopleIcon from "@material-ui/icons/People";
import GitHubIcon from "@material-ui/icons/GitHub";
import SettingsIcon from "@material-ui/icons/Settings";
import clsx from "clsx";

class Sidebar extends Component {
    render() {
        return (
            <Drawer
                variant="permanent"
                classes={{paper: clsx(this.props.classes.drawerPaper, !this.props.expanded && this.props.classes.drawerPaperClose)}}
                open={this.props.expanded}
                >
                <div className={this.props.classes.toolbarIcon}>
                    <IconButton onClick={this.props.onClose.bind(this)}>
                        <ChevronLeftIcon />
                    </IconButton>
                </div>
                <Divider />
                <List>
                    <ListItem button>
                        <ListItemIcon>
                            <ClassBrowserIcon />
                        </ListItemIcon>
                        <ListItemText primary="Class Browser" />
                    </ListItem>
                    <ListItem button>
                        <ListItemIcon>
                            <WorkspaceIcon />
                        </ListItemIcon>
                        <ListItemText primary="Workspace" />
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