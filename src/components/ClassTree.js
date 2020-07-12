import React, { Component } from 'react';
import CustomTree from './CustomTree';

class ClassTree extends Component {
    getSubclasses = (species) => {
        const classes = this.props.classes;
        return species.subclasses === undefined ? [] : species.subclasses.map(name => { return classes[name] })
    }

    removeClass = (species) => {
        this.props.api.removeClass(species.name)
            .then(response => {
                const handler = this.props.onRemoved; 
                if (handler !== undefined) {
                    handler(species)
                }
            })
            .catch(error => {})
    }

    browseReferences = (species) => {
        if (this.props.globalOptions === undefined) { return }
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
                children={this.getSubclasses}
                onSelect={this.props.onSelect}
                selectedItem={this.props.selectedClass}
                menuOptions={[
                    {label: 'Rename', action: this.renameClass},
                    {label: 'Remove', action: this.removeClass},
                    {label: 'References', action: this.browseReferences}]}
            />
        )
    }
}
export default ClassTree;