import React, { Component } from 'react';
import TreeView from '@material-ui/lab/TreeView';
import TreeItem from '@material-ui/lab/TreeItem';
import PopupMenu from './PopupMenu';
import ArrowRightIcon from '@material-ui/icons/ArrowRight';
import ArrowDropDownIcon from '@material-ui/icons/ArrowDropDown';

class CustomTree extends Component {
    constructor(props) {
        super(props);
        this.state = {
            items: props.items,
            selectedItem: props.selectedItem,
            selectedId: props.selectedItem == null ? null : this.getItemId(props.selectedItem),
            menuOpen: false,
            menuPosition: {x: null, y: null}
        }
    }

    createItems = (items) => {
        return (
            items.map((item, index) => {
                const label = this.getItemLabel(item);
                const children = this.getItemChildren(item);
                const id = this.getItemId(item);
                return (
                    <TreeItem
                        key={label + "-item-" + index}
                        nodeId={id}
                        label={label}
                        onLabelClick={(event) => this.itemSelected(event, index, item)}
                        onIconClick={(event) => this.itemToggled(event, index, item)}
                        onContextMenu={this.openMenu}
                        >
                            {Array.isArray(children) ? this.createItems(children) : null}
                    </TreeItem>
                )
            })
        )
    }

    getItemId = (item) => {
        const getter = this.props.id;
        if (getter === undefined) { return this.getItemLabel(item) }    
        if (typeof getter == "string")  { return item[getter].toString() }
        return getter(item)
      }

    getItemLabel = (item) => {
        const getter = this.props.label;
        if (getter === undefined) { return item }    
        if (typeof getter == "string")  { return item[getter] }
        return getter(item)
      }

    getItemChildren = (item) => {
        const getter = this.props.children;
        if (getter === undefined) { return null }    
        if (typeof getter == "string")  { return item[getter] }
        return getter(item)
    }

    itemSelected = (event, index, item) => {
        event.preventDefault();
        this.setState({selectedItem: item, selectedIndex: index});
        const handler = this.props.onSelect;
        if (handler !== undefined) {

            handler(item);
        }
    };

    itemToggled = (event, index, item) => {
        const handler = this.props.onExpand;
        if (handler !== undefined) {
        //    handler.bind(this);
            handler(item);
        }
    };

    menuOptions() {
        if (this.props.menuOptions === undefined) { return undefined };
        return this.props.menuOptions.map(o => {
            return {
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
        option.action.bind();
        option.action(this.state.selectedItem);
    }

    render() {
        return (
            <div>
            <TreeView
                defaultCollapseIcon={<ArrowDropDownIcon />}
                defaultExpanded={['root']}
                defaultExpandIcon={<ArrowRightIcon />}
                defaultSelected={this.state.selectedId}
                >
                {this.createItems(this.props.items)}
            </TreeView>
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

export default CustomTree;


