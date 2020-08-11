import React, { Component } from 'react';
import { List, ListItem, ListItemText, ListItemIcon } from '@material-ui/core';
import PopupMenu from './PopupMenu';
import Scrollable from './Scrollable';

class CustomList extends Component {
  constructor(props) {
    super(props);
    this.state = {
      menuOpen: false,
      menuPosition: {x: null, y: null},
      filterText: '',
    }
  }

  createItems = () => {
    if (!this.props.items) {return []};
    return (
      this.props.items.map((item, index) => {
        const label = this.getItemLabel(item);
        const icon = this.getItemIcon(index);
        const divider = this.getItemDivider(item);
        const selected = this.props.selectedItem === item;
        return (
          <ListItem
            disableGutters={divider}
            autoFocus={selected}
            style={{paddingTop: 0, paddingBottom: 0}}
            button
            divider={divider}
            key={"item" + index}
            selected={selected}
            onClick={event => this.itemSelected(item)}
            //onKeyDown={this.keyPressed}
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

  clearFilter() {
    this.setState({filterText: ''})
  }

  keyPressed = (event) => {
    event.preventDefault();
    if (event.key === 'ArrowUp') {this.moveUp()}
    if (event.key === 'ArrowDown') {this.moveDown()}
    if (event.key === 'Escape') {
      this.clearFilter()
    } else {
      const prefix = this.state.filterText + event.key;
      this.setState({filterText: prefix})
      const item = this.props.items.find(i => this.getItemLabel(i).startsWith(prefix));  
      if (item) {this.itemSelected(item)}
    }
  }

  render () {
    return (
      <Scrollable>
          <List
            onKeyDown={this.keyPressed}
            style={{paddingTop: 0, paddingBottom: 0}}>
                {this.createItems()}
          </List>
          <PopupMenu
            options={this.menuOptions()}
            open={this.state.menuOpen}
            position={this.state.menuPosition}
            onClose={this.closeMenu}/>
      </Scrollable>
    )
  }
}

export default CustomList;
