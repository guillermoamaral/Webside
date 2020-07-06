import React, { Component } from 'react';
import TreeView from '@material-ui/lab/TreeView';
import TreeItem from '@material-ui/lab/TreeItem';
import ArrowRightIcon from '@material-ui/icons/ArrowRight';
import ArrowDropDownIcon from '@material-ui/icons/ArrowDropDown';

class Tree extends Component {
    createItems = (items) => {
        return (
            items.map((item, index) => 
                <TreeItem
                    key={"item" + index}
                    nodeId={item[this.props.label]}
                    label={item[this.props.label]}
                    >
                        {Array.isArray(item[this.props.children]) ? this.createItems(item[this.props.children]) : null}
                </TreeItem>)
        )
    }

    nodeSelected = (e, id) => {
        const handler = this.props.onSelect;
        if (handler !== null) {
            handler.bind(this);
            handler(id);
        }
    };

    render() {
        return (
            <TreeView
                defaultCollapseIcon={<ArrowRightIcon />}
                defaultExpanded={['root']}
                defaultExpandIcon={<ArrowDropDownIcon />}
                onNodeSelect={this.nodeSelected}
                >
                {this.createItems(this.props.items)}
            </TreeView>
        )
    };
}

export default Tree
