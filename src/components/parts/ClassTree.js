import React, { Component } from 'react';
import CustomTree from '../controls/CustomTree';
import ConfirmDialog from '../controls/ConfirmDialog';
import { AppContext } from '../../AppContext';

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
        const handler = this.props.onRemoved; 
        if (handler !== undefined) {handler(species)}
    }

    render() {
        const root = this.props.root;
        return (
            <div>
                <CustomTree
                    items={root !== undefined ? [root] : []}
                    itemLabel="name"
                    children={"subclasses"}
                    onExpand={this.props.onExpand}
                    onSelect={this.props.onSelect}
                    selectedItem={this.props.selectedClass}
                    menuOptions={[
                        {label: 'New', action: this.newClass},
                        {label: 'Rename', action: this.renameClass},
                        {label: 'Remove', action: this.removeClass},
                        null,
                        {label: 'References', action: c => this.browseReferences(c.name)}]}
                />
                {/* <ConfirmDialog
                    title="Delete Class"
                    open={this.state.confirmOpen}
                    question="Are you sure you want to delete this class?"
                    onCancel={() => {this.setState({confirmOpen: false})}}
                    onConfirm={() => this.removeClass(this.state.classToRemove)}>
                </ConfirmDialog> */}
            </div>
        )
    }
}
export default ClassTree;