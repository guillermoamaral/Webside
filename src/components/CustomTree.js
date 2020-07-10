import React, { Component } from 'react';
import TreeView from '@material-ui/lab/TreeView';
import TreeItem from '@material-ui/lab/TreeItem';
import ArrowRightIcon from '@material-ui/icons/ArrowRight';
import ArrowDropDownIcon from '@material-ui/icons/ArrowDropDown';

class CustomTree extends Component {
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
                        >
                            {Array.isArray(children) ? this.createItems(children) : null}
                    </TreeItem>)
            })
        )
    }

    getItemId = (item) => {
        const getter = this.props.id;
        if (getter === undefined) { return this.getItemLabel(item) }    
        if (typeof getter == "string")  { return item[getter].toString() }
        getter.bind(this);
        return getter(item)
      }

    getItemLabel = (item) => {
        const getter = this.props.label;
        if (getter === undefined) { return item }    
        if (typeof getter == "string")  { return item[getter] }
        getter.bind(this);
        return getter(item)
      }

    getItemChildren = (item) => {
        const getter = this.props.children;
        if (getter === undefined) { return null }    
        if (typeof getter == "string")  { return item[getter] }
        getter.bind(this);
        return getter(item)
      }

    itemSelected = (event, id) => {
        const handler = this.props.onSelect;
        if (handler !== undefined) {
            handler.bind(this);
            handler(id);
        }
    };

    itemToggled = (event, id) => {
        const handler = this.props.onExpand;
        if (handler !== undefined) {
            handler.bind(this);
            handler(id);
        }
    };

    render() {
        return (
            <TreeView
                defaultCollapseIcon={<ArrowDropDownIcon />}
                defaultExpanded={['root']}
                defaultExpandIcon={<ArrowRightIcon />}
                onNodeSelect={this.itemSelected}
                onNodeToggle={this.itemToggled}
                >
                {this.createItems(this.props.items)}
            </TreeView>
        )
    };
}

export default CustomTree;
