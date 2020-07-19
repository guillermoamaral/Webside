import React, { Component } from 'react';
import { List, ListItem, ListItemText, ListItemIcon } from '@material-ui/core';
import PopupMenu from './PopupMenu';

class CustomList extends Component {
  constructor(props) {
    super(props);
    this.state = {
      items: props.items,
      selectedItem: props.selectedItem,
      selectedIndex: props.selectedItem == null ? null : props.items.indexOf(props.selectedItem),
      menuOpen: false,
      menuPosition: {x: null, y: null}
    }
  }

  static getDerivedStateFromProps(props, state) {
    if (props.selecteItem !== undefined && props.selectedItem !== state.selectedItem) {
        return {
            items: props.items,
            selectedItem: props.selectedItem,
            selectedIndex: props.selectedItem == null ? null : props.items.indexOf(props.selectedItem), 
        };
    }
    return null
  }

  createItems = () => {
    if (this.props.items === undefined) {return []};
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
            selected={this.state.selectedIndex === index}
            onClick={event => this.itemSelected(event, index, item)}
            onContextMenu={this.openMenu}
            >
              {icon}
              <ListItemText primary={label} />
          </ListItem>
        )
      })
    )
  }

  itemSelected = (event, index, item) => {
    this.setState({selectedItem: item, selectedIndex: index});
    const handler = this.props.onSelect;
    if (handler !== undefined) {handler(item)}
  }

  getItemDivider = (item) => {
    const getter = this.props.itemDivider;
    if (getter === undefined) {return false}    
    if (typeof getter == "string")  {return item[getter]}
    return getter(item)
  }

  getItemLabel = (item) => {
    const getter = this.props.itemLabel;
    if (getter === undefined) {return item}    
    if (typeof getter == "string")  {return item[getter]}
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

  menuOptions() {
    if (this.props.menuOptions === undefined) {return undefined};
    return this.props.menuOptions.map(o => {
        return o == null? null : {
          label: o.label,
          action: () => {this.menuOptionClicked(o)}
        }
      }
    )
  }

  openMenu = (event) => {
    event.preventDefault();
    this.setState({menuOpen: true, menuPosition: {x: event.clientX - 2, y: event.clientY - 4}})
  };

  closeMenu = () => {
    this.setState({menuOpen: false});
  }

  menuOptionClicked(option) {
    if (option.action !== undefined) {
      option.action(this.state.selectedItem);
    }
  }

  render () {
    return (
      <div>
        <List style={{paddingTop: 0, paddingBottom: 0}}>
          {this.createItems()}
        </List>
        {
          <PopupMenu
            options={this.menuOptions()}
            open={this.state.menuOpen}
            position={this.state.menuPosition}
            onClose={this.closeMenu}/>}
      </div>
    )
  };
}

export default CustomList;
