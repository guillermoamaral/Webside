import React, { Component } from 'react';
import { List, ListItem, ListItemText, ListItemIcon, Menu, MenuItem } from '@material-ui/core';

class CustomList extends Component {
  constructor(props) {
    super(props);
    this.state = {
      selectedItem: props.selectedItem,
      selectedIndex: props.selectedItem == null ? null : props.indexOf(props.selectedItem),
      menuX: null,
      menuY: null}
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

  createItems = () => {
    if (this.props.items == undefined) { return [] };
    return (
      this.props.items.map((item, index) => {
        return (
          <ListItem
            style={{paddingTop: 0, paddingBottom: 0}}
            button
            key={"item" + index}
            selected={this.state.selectedIndex === index}
            onClick={(e) => this.itemSelected(e, index, item)}
            onContextMenu={this.openMenu}
            >
              {this.getItemIcon(index)}
              <ListItemText primary={this.getItemLabel(item)} />
          </ListItem>
        )
      })
    )
  }

  createMenuItems = () => {
    if (this.props.menuOptions === undefined) { return [] };
    return (
      this.props.menuOptions.map(option => {
        return (
          <MenuItem
            key={option.label}
            onClick={this.menuOptionClicked}
            style={{paddingTop: 0, paddingBottom: 0}}
          >
            {option.label}
          </MenuItem>
        )
      })
    )
  }

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

  openMenu = (event) => {
    if (this.props.menuOptions === undefined) { return }
    event.preventDefault();
    this.setState({
      menuX: event.clientX - 2,
      menuY: event.clientY - 4});
  };

  optionClicked = (event) => {
    console.log(event.target);
    this.closeMenu(event)
  }

  closeMenu = (event) => {
    this.setState({
      menuX: null,
      menuY: null});
  }

  render () {
    return (
      <div>
        <List style={{paddingTop: 0, paddingBottom: 0}}>
          {this.createItems()}
        </List>
        <Menu
            keepMounted
            open={this.state.menuX !== null}
            onClose={this.closeMenu}
            anchorReference="anchorPosition"
            anchorPosition={
              this.state.menuY !== null && this.state.menuX !== null
                ? { top: this.state.menuY, left: this.state.menuX }
                : undefined
            }
          >
            {this.createMenuItems()}
          </Menu>
      </div>
    )
  };
}

export default CustomList;
