import React, { Component } from 'react';
import CustomTree from './CustomTree';
import ConfirmDialog from './ConfirmDialog';

class ClassTree extends Component {
    constructor(props) {
        super(props);
        this.state = {
            root: props.root,
            confirmOpen: false,
            classToRemove: null
        }
    }

    // static getDerivedStateFromProps(props, state) {
    //     if (props.root !== state.root) {
    //         return {
    //             items: props.items,
    //         };
    //     }
    //     return null
    // }

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
            <div>
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
                        {label: 'Remove', action: (species) => this.setState({confirmOpen: true, classToRemove: species})},
                        null,
                        {label: 'References', action: this.browseReferences}]}
                />
                <ConfirmDialog
                    title="Delete Class?"
                    open={this.state.confirmOpen}
                    setOpen={(open) => {this.setState({confirmOpen: open})}}
                    onConfirm={() => this.removeClass(this.state.classToRemove)}
                >
                    Are you sure you want to delete this class?
                </ConfirmDialog>
            </div>
        )
    }
}
export default ClassTree;