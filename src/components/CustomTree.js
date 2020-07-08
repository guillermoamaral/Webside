import React, { Component } from 'react';
import TreeView from '@material-ui/lab/TreeView';
import TreeItem from '@material-ui/lab/TreeItem';
import ArrowRightIcon from '@material-ui/icons/ArrowRight';
import ArrowDropDownIcon from '@material-ui/icons/ArrowDropDown';

class CustomTree extends Component {
    constructor(props){
        super(props);
        this.state = {
        }
    }
    createItems = (items) => {
        return (
            items.map((item, index) => {
                const label = this.itemLabel(item);
                const children = this.itemChildren(item);
                return (
                    <TreeItem
                        key={label + "-item-" + index}
                        nodeId={label}
                        label={label}
                        >
                            {Array.isArray(children) ? this.createItems(children) : null}
                    </TreeItem>)
            })
        )
    }

    itemLabel = (item) => {
        if (this.props.label == null) { return item }    
        if (typeof this.props.label == "string")  { return item[this.props.label] }
        this.props.label.bind(this);
        return this.props.label(item)
      }

    itemChildren = (item) => {
        if (this.props.children == null) { return null }    
        if (typeof this.props.children == "string")  { return item[this.props.children] }
        this.props.children.bind(this);
        return this.props.children(item)
      }

    itemSelected = (e, id) => {
        const handler = this.props.onSelect;
        if (handler !== null) {
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
                >
                {this.createItems(this.props.items)}
            </TreeView>
        )
    };
}

export default CustomTree;
