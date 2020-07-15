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
      {...other}
    >
      {visible && (
        <Box className={props.classes.box} p={3}>
          {children}
        </Box>
      )}
    </div>
  );
}

TabPanel.propTypes = {
  children: PropTypes.node,
  id: PropTypes.any.isRequired,
  visible: PropTypes.any.isRequired,
};

class TabControl extends Component {
    constructor(props){
        super(props);
        this.state = {
            selectedIndex: props.selectedIndex === undefined ? 0 : props.selectedIndex
        }
    }

    tabChanged = (event, value) => {
       this.setState({selectedIndex: value});
    }

    tabLabel = (index) => {
      const page = this.props.pages[index];
      return (
        <span>
          {React.cloneElement(page.icon, {className: this.props.classes.tabIcon})}
          {page.label}
          <IconButton 
            onClick={(event) => {this.tabClosed(event, index)}}
            id={index}
            value={index}
            size="small">
              <CloseIcon fontSize="small" id={index} value={index}/>
          </IconButton>
        </span>
      )
    }

    tabClosed = (event, index) => {
      event.stopPropagation();
      if (this.props !== null) {
        const handler = this.props.onClose;
        if (handler !== undefined) {
            //handler.bind(this);
            handler(this.props.pages[index]);
            if (index <= this.state.selectedIndex) {
              index = Math.max(index - 1, 0)
              this.setState({selectedIndex: index})
            }
        }
      }
    };    

    render() {
        return (
            <div className={this.props.classes.tabControl}>
                <Tabs
                    value={this.state.selectedIndex}
                    onChange={this.tabChanged}
                    indicatorColor="primary"
                    textColor="primary"
                    variant="scrollable"
                    scrollButtons="auto"
                    >
                        {this.props.pages.map((p, i) => {
                            return (
                              <Tab
                                component="div"
                                key={i.toString()}
                                label={this.tabLabel(i)}
                                id= {`tab-${i}`}
                              />
                            );
                        })}
                </Tabs>
                  {this.props.pages.map((p, i) => {
                    return (
                        <TabPanel
                            id={`tabpanel-${i}`}
                            key={i.toString()}
                            index={i}
                            classes={this.props.classes}
                            visible={i === this.state.selectedIndex}
                          >
                            {p.component}
                        </TabPanel>
                    );
                })}

            </div>
        );
    };
};

export default TabControl;