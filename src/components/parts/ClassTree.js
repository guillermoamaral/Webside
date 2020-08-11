import React, { Component } from 'react';
import CustomTree from '../controls/CustomTree';
import { AppContext } from '../../AppContext';
import Scrollable from '../controls/Scrollable';

class ClassTree extends Component {
    static contextType = AppContext;

    constructor(props) {
        super(props);
        this.state = {
            root: props.root,
            confirmOpen: false        }
    }

    // static getDerivedStateFromProps(props, state) {
    //     if (props.root !== state.root) {
    //         return {
    //             items: props.items,
    //         };
    //     }
    //     return null
    // }

    removeClass = async (species) => {
 //       (species) => this.setState({confirmOpen: true, classToRemove: species})
        await this.context.api.deleteClass(species.name);
        const handler = this.props.onRemove; 
        if (handler) {handler(species)}
    }

    menuOptions() {
        return [
            {label: 'New', action: this.newClass},
            {label: 'Rename', action: this.renameClass},
            {label: 'Remove', action: this.removeClass},
            null,
            {label: 'Browse', action: c => this.context.browseClass(c.name)},
            {label: 'References', action: c => this.context.browseReferences(c.name)}
        ]
    }

    render() {
        const root = this.props.root;
        return (
            <CustomTree
                items={root? [root] : []}
                itemLabel="name"
                children={"subclasses"}
                onExpand={this.props.onExpand}
                onSelect={this.props.onSelect}
                selectedItem={this.props.selectedClass}
                menuOptions={this.menuOptions()}
            />
        )
    }
}
export default ClassTree;