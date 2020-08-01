import React, { Component } from 'react';
import PropTypes from 'prop-types';
import { Tabs, Tab, Box, IconButton } from '@material-ui/core';
import CloseIcon from '@material-ui/icons/Close';

function TabPanel(props) {
  const {id, children, visible, ...other} = props;
  return (
    <div
      role="tabpanel"
      hidden={!visible}
      id={id}
      {...other}>
        <Box className={props.classes.box} p={1}>
          {children}
        </Box>
    </div>
  )
}

TabPanel.propTypes = {
  children: PropTypes.node,
  id: PropTypes.any.isRequired,
  visible: PropTypes.any.isRequired,
}

class TabControl extends Component {
  tabLabel = (page, index) => {
    return (
      <span>
        {React.cloneElement(page.icon, {className: this.props.classes.tabIcon})}
        {page.label}
        <IconButton 
          onClick={event => {this.tabClosed(event, index)}}
          id={index}
          value={index}
          size="small">
            <CloseIcon fontSize="small" id={index} value={index}/>
        </IconButton>
      </span>
    )
  }

  tabChanged = (event, index) => {
    event.preventDefault();
    const handler = this.props.onSelect;
    if (handler) {handler(this.props.pages[index])}
  }

  tabClosed = (event, index) => {
    event.stopPropagation();
    const handler = this.props.onClose;
    if (handler) {handler(this.props.pages[index])}
  }

  render() {
    const {pages, selectedPage} = this.props;
    const selectedIndex = pages.indexOf(selectedPage);
    return (
        <div className={this.props.classes.tabControl}>
            <Tabs
              value={Math.max(selectedIndex, 0)}
              onChange={this.tabChanged}
              indicatorColor="primary"
              textColor="primary"
              variant="scrollable"
              scrollButtons="auto">
                {pages.map((p, i) => {
                  return (
                    <Tab
                      component="div"
                      key={i.toString()}
                      label={this.tabLabel(p, i)}
                      id= {`tab-${i}`}/>)
                })}
            </Tabs>
            {pages.map((p, i) => {
              return (
                <TabPanel
                  id={`tabpanel-${i}`}
                  key={i.toString()}
                  index={i}
                  classes={this.props.classes}
                  visible={p === selectedPage}>
                    {p.component}
                </TabPanel>)
            })}
        </div>
    )
  }
}

export default TabControl;