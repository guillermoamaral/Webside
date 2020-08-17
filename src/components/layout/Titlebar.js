import React, { Component } from 'react';
import {
    AppBar,
    Toolbar,
    Avatar,
    Typography,
    IconButton,
    Link,
    InputBase,
    Box
  } from '@material-ui/core';
import clsx from 'clsx';  
import MenuIcon from '@material-ui/icons/Menu';
import SearchList from '../controls/SearchList';
import SearchIcon from '@material-ui/icons/Search';

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
          <Box p={2}>
            <Link to="http://localhost:3000">
              <img
                src={require("../../resources/" + this.props.appName + ".png")}
                width={28}
                height={28}
                alt={"Open"}/>
            </Link>
          </Box>
          <Typography
            component="h1"
            variant="h6"
            color="inherit"
            noWrap>
              {this.props.title} 
          </Typography>
          <div className={styles.grow}/>
          <div className={styles.globalSearch}>
            <div className={styles.globalSearchIcon}>
              <SearchIcon />
            </div>
            <InputBase
              placeholder="Search…"
              classes={{
                root: styles.inputRoot,
                input: styles.inputInput,
              }}
              inputProps={{'aria-label': 'search'}}/>
          </div>
          <div className={styles.grow}/>
          <Avatar
            alt="guest"
          />
        </Toolbar>
      </AppBar>)
    }
}

export default Titlebar;
