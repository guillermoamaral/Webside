import React, { Component } from 'react';
import PropTypes from 'prop-types';
import IconButton from '@material-ui/core/IconButton';
import Tabs from '@material-ui/core/Tabs';
import Tab from '@material-ui/core/Tab';
import Box from '@material-ui/core/Box';
import CloseIcon from '@material-ui/icons/Close';

function TabPanel(props) {
  const { id, children, visible, ...other } = props;
  return (
    <div
      role="tabpanel"
      hidden={!visible}
      id={id}
      {...other}
    >
      {visible && (
        <Box p={3}>
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
            selectedIndex: 0
        }
    }

    tabChanged = (e, value) => {
       this.setState({selectedIndex: value});
    }

    tabLabel = (index) => {
      const page = this.props.pages[index];
      return (
        <span>
          {React.cloneElement(page.icon, {className: this.props.classes.tabIcon})}
          {page.label}
          <IconButton onClick={this.tabClosed} value={index}>
            <CloseIcon fontSize="small"/>
          </IconButton>
        </span>
      )
    }

    tabClosed = (event) => {
      if (event.target == null) { return }
      event.stopPropagation();
      if (this.props !== null) {
        const handler = this.props.onClose;
        if (handler !== undefined) {
            var index = event.target.value;
            handler.bind(this);
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
                              />);
                        })}
                </Tabs>
                  {this.props.pages.map((p, i) => {
                    return (
                        <TabPanel
                            id={`tabpanel-${i}`}
                            key={i.toString()}
                            index={i}
                            visible={i === this.state.selectedIndex}
                          >
                            {p.component}
                        </TabPanel>);
                })}

            </div>
        );
    };
};

export default TabControl;