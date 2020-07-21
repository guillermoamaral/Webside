import React, { Component } from 'react';
import {
    AppBar,
    Toolbar,
    Avatar,
    Typography,
    IconButton,
    Link
  } from '@material-ui/core';
import clsx from 'clsx';  
import MenuIcon from '@material-ui/icons/Menu';

class Titlebar extends Component {
    render() {
        return (
          <AppBar
                color="primary"
                position="absolute"
                className={clsx(this.props.classes.appBar, this.props.sidebarExpanded && this.props.classes.appBarShift)}>
            <Toolbar className={this.props.classes.toolbar}> 
              <IconButton
                edge="start"
                color="inherit"
                onClick={this.props.expandSidebar.bind(this)}
                className={clsx(
                  this.props.classes.menuButton,
                  this.props.sidebarExpanded && this.props.classes.menuButtonHidden
                )}
              >
                <MenuIcon />
              </IconButton>
              <Link to="http://localhost:3000">
              <img
                src={require("../resources/" + this.props.appName + ".png")}
                width={28}
                height={28}
                alt={"Open"}/>
              </Link>
              <Typography
                component="h1"
                variant="h6"
                color="inherit"
                noWrap
                className={this.props.classes.title}
              >
                {this.props.title} 
              </Typography>
              <Avatar
                alt="guest"
              />
            </Toolbar>
          </AppBar>
        )
    }
}

export default Titlebar;
