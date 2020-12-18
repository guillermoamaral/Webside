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
            <Link to={this.props.baseUri}>
              <img
                src={this.props.dialect? require("../../resources/" + this.props.dialect + ".png") : null}
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
              {this.props.dialect + ' Web IDE (Powered by Webside)'}
          </Typography>
          <Box flexGrow={1}/>
          <div className={styles.globalSearch}>
            <div className={styles.globalSearchIcon}>
              <SearchIcon />
            </div>
            <InputBase
              placeholder="Search…"
              classes={{
                root: styles.globalSearchInputRoot,
                input: styles.globalSearchInputInput,
              }}
              inputProps={{'aria-label': 'search'}}/>
          </div>
          <Box flexGrow={1}/>
          <IconButton color="primary" aria-label="upload picture" component="span" onClick={e => {alert('yes')}}>
            <Avatar alt={this.props.developer}/>
          </IconButton>
          <Box p={1}>
            <Typography
              variant="subtitle1"
              gutterBottom
              color="inherit"
              noWrap>
                {this.props.developer}
            </Typography>
          </Box>
        </Toolbar>
      </AppBar>)
    }
}

export default Titlebar;
