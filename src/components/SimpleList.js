import React, { Component } from 'react';
import { List, ListItem, ListItemText, ListItemIcon } from '@material-ui/core';

class SimpleList extends Component {
  constructor(props) {
    super(props);
    this.state = {
      selectedItem: props.selectedItem,
      selectedIndex: props.selectedItem == null ? null : props.indexOf(props.selectedItem)}
  }

  /*
  static getDerivedStateFromProps(props, state) {
    if (props.selectedItem !== state.selectedItem) {
        return {
            selectedItem: null,
            selectedIndex: null
         }
    };
    return null;
 }*/

  itemSelected = (e, index) => {
    const item = this.props.items[index];
    this.setState({selectedItem: item, selectedIndex: index});
    if (this.props !== null) { 
      const handler = this.props.onSelect;
      if (handler !== null) {
          handler.bind(this);
          handler(item);
      }
    }
  };

  itemIcon = (i) => {
    if (this.props.icons !== undefined && i < this.props.icons.length) {
      return (
        <ListItemIcon style={{minWidth: 0}}>
          {this.props.icons[i]}
      </ListItemIcon>
      )
    }
  }

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
                onClick={(e) => this.itemSelected(e, i)}
                >
                  {this.itemIcon(i)}
                  <ListItemText primary={v} />
              </ListItem>
          )}
      </List>
    )
  };
}

export default SimpleList;
