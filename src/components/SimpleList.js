import React, { Component } from 'react';
import { List, ListItem, ListItemText } from '@material-ui/core';

class SimpleList extends Component {
  constructor(props) {
    super(props);
    this.state = {
      selectedIndex: null}
  }

  itemSelected = (event, index) => {
    this.setState({selectedIndex: index});
    if (this.props !== null) { 
      const handler = this.props.onSelect;
      if (handler !== null) {
          handler.bind(this);
          handler(this.props.items[index]);
      }
    }
  };

  render () {
    return (
      <List>
          {
            (this.props.items == null ? [] : this.props.items).map((v, i) =>
              <ListItem
                style={{paddingTop: 0, paddingBottom: 0}}
                button
                key={v}
                selected={this.state.selectedIndex === i}
                onClick={(event) => this.itemSelected(event, i)}>
                  <ListItemText primary={v} 
              width="100%"/>
              </ListItem>
          )}
      </List>
    )
  };
}

export default SimpleList;
