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

  itemSelected = (e, index, item) => {
    this.setState({selectedItem: item, selectedIndex: index});
    if (this.props !== null) { 
      const handler = this.props.onSelect;
      if (handler !== null) {
          handler.bind(this);
          handler(item);
      }
    }
  };

  itemIcon = (index) => {
    if (this.props.icons !== undefined && index < this.props.icons.length) {
      return (
        <ListItemIcon style={{minWidth: 0}}>
          {this.props.icons[index]}
        </ListItemIcon>
      )
    }
  }

  render () {
    return (
      <List>
          {
            (this.props.items == null ? [] : this.props.items).map((item, index) =>
              <ListItem
                style={{paddingTop: 0, paddingBottom: 0}}
                button
                key={"item" + index}
                selected={this.state.selectedIndex === index}
                onClick={(e) => this.itemSelected(e, index, item)}
                >
                  {this.itemIcon(index)}
                  <ListItemText primary={this.props.label == null ? item : item[this.props.label]} />
              </ListItem>
          )}
      </List>
    )
  };
}

export default SimpleList;
