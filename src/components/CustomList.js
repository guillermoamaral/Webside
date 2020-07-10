import React, { Component } from 'react';
import { List, ListItem, ListItemText, ListItemIcon } from '@material-ui/core';

class CustomList extends Component {
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
    //this cannot happend... check
    //if (this.props !== null) { 
    const handler = this.props.onSelect;
    if (handler !== undefined) {
        handler.bind(this);
        handler(item);
    }
  };

  getItemLabel = (item) => {
    const getter = this.props.label;
    if (getter == undefined) { return item }    
    if (typeof getter == "string")  { return item[getter] }
    getter.bind(this);
    return getter(item)
  }

  getItemIcon = (index) => {
    const icons = this.props.icons;
    if (icons !== undefined && index < icons.length) {
      return (
        <ListItemIcon style={{minWidth: 0}}>
          {icons[index]}
        </ListItemIcon>
      )
    }
  }

  render () {
    return (
      <List style={{paddingTop: 0, paddingBottom: 0}}>
          {
            (this.props.items == null ? [] : this.props.items).map((item, index) =>
              <ListItem
                style={{paddingTop: 0, paddingBottom: 0}}
                button
                key={"item" + index}
                selected={this.state.selectedIndex === index}
                onClick={(e) => this.itemSelected(e, index, item)}
                >
                  {this.getItemIcon(index)}
                  <ListItemText primary={this.getItemLabel(item)} />
              </ListItem>
          )}
      </List>
    )
  };
}

export default CustomList;
