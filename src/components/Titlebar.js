import React, { Component } from 'react';
import {
    AppBar,
    Toolbar,
    Avatar,
    Typography,
    IconButton,
    SvgIcon
  } from '@material-ui/core';
import clsx from 'clsx';  
import MenuIcon from '@material-ui/icons/Menu';
import { ReactComponent as Logo } from "../resources/logo.svg";

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
              <SvgIcon><Logo style={{width: 24, height: 24}}/></SvgIcon>
              <Typography
                component="h1"
                variant="h6"
                color="inherit"
                noWrap
                className={this.props.classes.title}
              >
                Bee Smalltalk (Powered by Online Smalltalk IDE v1.0.0) 
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
