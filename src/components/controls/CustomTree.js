import React, { Component } from 'react';
import TreeView from '@material-ui/lab/TreeView';
import TreeItem from '@material-ui/lab/TreeItem';
import ArrowRightIcon from '@material-ui/icons/ArrowRight';
import ArrowDropDownIcon from '@material-ui/icons/ArrowDropDown';
import PopupMenu from './PopupMenu';

class CustomTree extends Component {
    constructor(props) {
        super(props);
        this.state = {
            items: props.items,
            selectedItem: !props.selectedItem? null : props.selectedItem,
            menuOpen: false,
            menuPosition: {x: null, y: null}
        }
    }

    static getDerivedStateFromProps(props, state) {
        if (props.selecteItem !== state.selectedItem) {
            return {
                items: props.items,
                selectedItem: !props.selectedItem? null : props.selectedItem,
            };
        }
        return null
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
                        onLabelClick={event => this.itemSelected(event, item)}
                        onIconClick={event => this.itemToggled(event, item)}
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
        if (!getter) {return this.getItemLabel(item)}    
        if (typeof getter == "string")  {return item[getter].toString()}
        return getter(item)
      }

    getItemLabel = (item) => {
        const getter = this.props.itemLabel;
        if (!getter) {return item}    
        if (typeof getter == "string")  {return item[getter]}
        return getter(item)
      }

    getItemChildren = (item) => {
        const getter = this.props.children;
        if (!getter) {return null}    
        if (typeof getter == "string")  {return item[getter]}
        return getter(item)
    }

    itemSelected = (event, item) => {
        event.preventDefault();
        this.setState({selectedItem: item});
        const handler = this.props.onSelect;
        if (handler) {handler(item)}
    };

    itemToggled = (event, item) => {
        const handler = this.props.onExpand;
        if (handler) {handler(item)}
    };

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
            option.action(this.state.selectedItem);
        }
    }

    render() {
        return (
            <div>
                <TreeView
                    defaultCollapseIcon={<ArrowDropDownIcon />}
                    defaultExpanded={['root']}
                    defaultExpandIcon={<ArrowRightIcon />}
                    selected={!this.state.selectedItem? null : this.getItemId(this.state.selectedItem)}
                    >
                    {this.createItems(this.props.items)}
                </TreeView>
                <PopupMenu
                    options={this.menuOptions()}
                    open={this.state.menuOpen}
                    position={this.state.menuPosition}
                    onClose={this.closeMenu}/>
          </div>
        )
    };
}

export default CustomTree;


