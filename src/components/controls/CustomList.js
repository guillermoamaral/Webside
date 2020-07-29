import React, { Component } from 'react';
import { List, ListItem, ListItemText, ListItemIcon } from '@material-ui/core';
import PopupMenu from './PopupMenu';

class CustomList extends Component {
  constructor(props) {
    super(props);
    this.state = {
      menuOpen: false,
      menuPosition: {x: null, y: null}
    }
  }

  createItems = () => {
    if (!this.props.items) {return []};
    return (
      this.props.items.map((item, index) => {
        const label = this.getItemLabel(item);
        const icon = this.getItemIcon(index);
        const divider = this.getItemDivider(item);
        return (
          <ListItem
            disableGutters={divider}
            //autoFocus
            style={{paddingTop: 0, paddingBottom: 0}}
            button
            divider={divider}
            key={"item" + index}
            selected={this.props.selectedItem === item}
            onClick={event => this.itemSelected(item)}
            onKeyDown={this.keyPressed}
            onContextMenu={this.openMenu}
            >
              {icon}
              <ListItemText primary={label} />
          </ListItem>
        )
      })
    )
  }

  itemSelected = (item) => {
    const handler = this.props.onSelect;
    if (handler) {handler(item)}
  }

  getItemDivider = (item) => {
    const getter = this.props.itemDivider;
    if (!getter) {return false}    
    if (typeof getter == "string")  {return item[getter]}
    return getter(item)
  }

  getItemLabel = (item) => {
    const getter = this.props.itemLabel;
    if (!getter) {return item}    
    if (typeof getter == "string")  {return item[getter]}
    return getter(item)
  }

  getItemIcon = (index) => {
    const icons = this.props.icons;
    if (icons && index < icons.length) {
      return (
        <ListItemIcon style={{minWidth: 0}}>
          {icons[index]}
        </ListItemIcon>
      )
    }
  }

  menuOptions() {
    if (this.props.menuOptions) {
      return this.props.menuOptions.map(o => {
        return !o? null : {
          label: o.label,
          action: () => {this.menuOptionClicked(o)}
        }
      })
    }
  }

  openMenu = (event) => {
    event.preventDefault();
    this.setState({menuOpen: true, menuPosition: {x: event.clientX - 2, y: event.clientY - 4}})
  };

  closeMenu = () => {
    this.setState({menuOpen: false});
  }

  menuOptionClicked(option) {
    if (option.action) {
      option.action(this.props.selectedItem);
    }
  }

  moveUp = () => {
    const items = this.props.items;
    const index = items.indexOf(this.props.selectedItem);
    if (index > 0) {
      this.itemSelected(items[index - 1]);
    }
  }

  moveDown = () => {
    const items = this.props.items;
    const index = items.indexOf(this.props.selectedItem);
    if (index < items.length - 1) {
      this.itemSelected(items[index + 1]);
    }
  }

  keyPressed = (e) => {
    if (e.keyCode === '38') {
        this.moveUp();
    }
    else if (e.keyCode === '40') {
        this.moveDown();
    }
  }

  render () {
    return (
      <div>
        <List style={{paddingTop: 0, paddingBottom: 0}}>
          {this.createItems()}
        </List>
        <PopupMenu
          options={this.menuOptions()}
          open={this.state.menuOpen}
          position={this.state.menuPosition}
          onClose={this.closeMenu}/>
      </div>
    )
  };
}

export default CustomList;
