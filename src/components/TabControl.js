import React, { Component } from 'react';
import PropTypes from 'prop-types';
import { withStyles } from '@material-ui/core/styles';
import Tabs from '@material-ui/core/Tabs';
import Tab from '@material-ui/core/Tab';
import Box from '@material-ui/core/Box';

function TabPanel(props) {
  const { children, value, index, ...other } = props;
  return (
    <div
      role="tabpanel"
      hidden={value !== index}
      id={`scrollable-auto-tabpanel-${index}`}
      aria-labelledby={`scrollable-auto-tab-${index}`}
      {...other}
    >
      {value === index && (
        <Box p={3}>
          {children}
        </Box>
      )}
    </div>
  );
}

TabPanel.propTypes = {
  children: PropTypes.node,
  index: PropTypes.any.isRequired,
  value: PropTypes.any.isRequired,
};

const useStyles = (theme) => ({
  root: {
    flexGrow: 1,
    width: '100%',
    backgroundColor: theme.palette.background.paper,
  },
});

class TabControl extends Component {
    constructor(props){
        super(props);
        this.state = {
            value: 0
        }
    }

    tabChanged = (event, value) => {
       this.setState({value: value});
    };

    render() {
        return (
            <div className={this.props.classes.root}>
                <Tabs
                    value={this.state.value}
                    onChange={this.tabChanged}
                    indicatorColor="primary"
                    textColor="inherit"
                    variant="scrollable"
                    scrollButtons="auto"
                    aria-label="scrollable auto tabs"
                    >
                        {this.props.pages.map((p, i) => {
                            return (<Tab
                                key={i.toString()}
                                label={p.label}
                                id= {`scrollable-auto-tab-${i}`}
                                aria-controls = {`scrollable-auto-tabpanel-${i}`}/>);
                        })}
                </Tabs>
                {this.props.pages.map((p, i) => {
                    return (
                        <TabPanel
                            key={i.toString()}
                            index={i}
                            value={this.state.value}>
                            {p.component}
                        </TabPanel>);
                })}

            </div>
        );
    };
};

export default withStyles(useStyles)(TabControl);