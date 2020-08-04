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
    const styles = this.props.styles;
    return (
      <AppBar
            color="primary"
            position="absolute"
            className={clsx(styles.appBar, this.props.sidebarExpanded && styles.appBarShift)}>
        <Toolbar className={styles.toolbar}> 
          <IconButton
            edge="start"
            color="inherit"
            onClick={this.props.expandSidebar.bind(this)}
            className={clsx(styles.menuButton, this.props.sidebarExpanded && styles.menuButtonHidden)}>
          <MenuIcon />
          </IconButton>
          <Link to="http://localhost:3000">
          <img
            src={require("../../resources/" + this.props.appName + ".png")}
            width={28}
            height={28}
            alt={"Open"}/>
          </Link>
          <Typography
            component="h1"
            variant="h6"
            color="inherit"
            noWrap
            className={styles.title}
            margin='normal'>
              {this.props.title} 
          </Typography>
          <Avatar
            alt="guest"
          />
        </Toolbar>
      </AppBar>)
    }
}

export default Titlebar;
