import React, { Component } from 'react';
import {
    AppBar,
    Toolbar,
    Avatar,
    Typography,
    IconButton,
  } from "@material-ui/core";
import clsx from "clsx";  
import MenuIcon from "@material-ui/icons/Menu";

class Titlebar extends Component {
    render() {
        return (
            <AppBar
                color="default"
                position="absolute"
                className={clsx(this.props.classes.appBar, this.props.sidebarExpanded && this.props.classes.appBarShift)}
            >
            <Toolbar className={this.props.classes.toolbar}> 
              <IconButton
                edge="start"
                color="inherit"
                aria-label="open drawer"
                onClick={this.props.expandSidebar.bind(this)}
                className={clsx(
                  this.props.classes.menuButton,
                  this.props.sidebarExpanded && this.props.classes.menuButtonHidden
                )}
              >
                <MenuIcon />
              </IconButton>
              <Typography
                component="h1"
                variant="h6"
                color="inherit"
                noWrap
                className={this.props.classes.title}
              >
                Smalltalk Web IDE
              </Typography>
              <Avatar
                alt="Uddeshya Singh"
              />
            </Toolbar>
          </AppBar>
        )
    }
}

export default Titlebar;
