import React, { PureComponent } from 'react';
import { Tabs, Tab, IconButton, Box } from '@material-ui/core';
import TabPanel from './TabPanel';
import TabLabel from './TabLabel';
import CloseIcon from '@material-ui/icons/Close';

class TabControl extends PureComponent {
  tabChanged = (event, index) => {
    event.preventDefault();
    const handler = this.props.onSelect;
    if (handler) {handler(this.props.pages[index])}
  }

  closeTab = (event, index) => {
    event.stopPropagation();
    const page = this.props.pages[index];
    const handler = this.props.onClose;  
    if (handler) {handler(page)}
  }

  render() {
    const {pages, selectedPage} = this.props;
    const selectedIndex = pages.indexOf(selectedPage);
    const styles = this.props.styles;
    return (
        <div className={styles.tabControl}>
            <Tabs
              value={Math.max(selectedIndex, 0)}
              onChange={this.tabChanged}
              indicatorColor="primary"
              textColor="primary"
              variant="scrollable"
              scrollButtons="auto">
                {pages.map((page, index) => {
                  return (
                    <Tab
                      component="div"
                      key={index.toString()}
                      label={<TabLabel index={index} icon={page.icon} label={page.label} onClose={this.closeTab}/>}
                      id= {`tab-${index}`}/>)
                })}
            </Tabs>
            {pages.map((page, index) => {
              return (
                <TabPanel
                  id={`tabpanel-${index}`}
                  key={index.toString()}
                  index={index}
                  styles={styles}
                  visible={page === selectedPage}>
                    {page.component}
                </TabPanel>)
            })}
        </div>
    )
  }
}

export default TabControl;