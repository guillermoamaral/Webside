import React, { Component } from 'react';
import TreeView from '@material-ui/lab/TreeView';
import TreeItem from '@material-ui/lab/TreeItem';
import ArrowRightIcon from '@material-ui/icons/ArrowRight';
import ArrowDropDownIcon from '@material-ui/icons/ArrowDropDown';

class ClassTree extends Component {
    createItems = (classes) => {
        return (
            classes.map((c) => 
                <TreeItem key={c.name} nodeId={c.name} label={c.name}>
                    {Array.isArray(c.subclasses) ? this.createItems(c.subclasses) : null}
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
                {this.createItems(this.props.classes)}
            </TreeView>
        )
    };
}

export default ClassTree
