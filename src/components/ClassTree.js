import React, { Component } from 'react';
import CustomTree from './CustomTree';

class ClassTree extends Component {
    constructor(props) {
        super(props);
        this.state = {
            root: props.root
        }
    }

    static getDerivedStateFromProps(props, state) {
        if (props.root !== state.root) {
            return {
                items: props.items,
            };
        }
        return null
    }

    removeClass = (species) => {
        this.props.api.deleteClass(species.name)
            .then(response => {
                const handler = this.props.onRemoved; 
                if (handler !== undefined) {
                    handler(species)
                }
            })
            .catch(error => {})
    }

    browseReferences = (species) => {
        if (this.props.globalOptions === undefined) {return}
        const option = this.props.globalOptions.browseReferences;
        if (option !== undefined) {
            option(species.name)
        }
    }

    render() {
        const root = this.props.classes[this.props.root];
        return (
            <CustomTree
                items={root !== undefined ? [root] : []}
                label="name"
                children={"subclasses"}
                onExpand={this.props.onExpand}
                onSelect={this.props.onSelect}
                selectedItem={this.props.selectedClass}
                menuOptions={[
                    {label: 'New', action: this.newClass},
                    {label: 'Rename', action: this.renameClass},
                    {label: 'Remove', action: this.removeClass},
                    null,
                    {label: 'References', action: this.browseReferences}]}
            />
        )
    }
}
export default ClassTree;