import React, { Component } from 'react';
import PropTypes from 'prop-types';
import { withStyles } from '@material-ui/core/styles';
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

const styles = (theme) => ({
  root: {
    flexGrow: 1,
    width: '100%',
    backgroundColor: theme.palette.background.paper,
  },
  icon: {
    //display: "flex",
    alignItems: "left",
    justifyContent: "flex-end",
    paddingTop: "8px"
  },
});

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

    tabLabel = (i) => {
      const page = this.props.pages[i];
      return (
        <span>
          {React.cloneElement(page.icon, {fontSize: "small", className: this.props.classes.icon})}
          {page.label}
          <IconButton onClick={this.tabClosed}>
            <CloseIcon id={i} fontSize="small"/>
          </IconButton>
        </span>
      )
    }

    tabClosed = (e) => {
      e.stopPropagation();
      if (this.props !== null) {
        const handler = this.props.onClose;
        if (handler !== undefined) {
            var index = parseInt(e.target.id);
            handler.bind(this);
            handler(this.props.pages[index]);
            var selected = this.state.selectedIndex;
            if (index < selected) {
              selected = Math.max(selected - 1, 0)
            }
            this.setState({selectedIndex: selected})
        }
      }
    };    

    render() {
        return (
            <div className={this.props.classes.root}>
                <Tabs
                    value={this.state.selectedIndex}
                    onChange={this.tabChanged}
                    indicatorColor="primary"
                    textColor="inherit"
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

export default withStyles(styles)(TabControl);